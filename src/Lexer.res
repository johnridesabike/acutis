/**
   Copyright 2020 John Jackson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

module Queue = Belt.MutableQueue
open Acutis_Types
open Debug

module Scanner = {
  type t = {
    str: string,
    mutable position: int,
  }

  let peekCharAt = (source, x) => Js.String2.charAt(source.str, source.position + x)

  let peekChar = source => Js.String2.charAt(source.str, source.position)

  let skipChar = source => source.position = source.position + 1

  let readChar = source => {
    let c = peekChar(source)
    skipChar(source)
    c
  }

  let peek = (source, ~until) => {
    let rec aux = position => {
      if until(. Js.String2.charAt(source.str, position)) {
        position
      } else {
        aux(position + 1)
      }
    }
    aux(source.position)
  }

  let skip = (source, ~until) => {
    source.position = peek(source, ~until)
  }

  let skipBy = (source, x) => source.position = source.position + x

  let readSubstring = (source, ~until) => {
    let start = source.position
    let end = peek(source, ~until)
    source.position = end
    Js.String2.slice(source.str, ~from=start, ~to_=end)
  }

  let readSubstringBy = (source, x) => {
    let start = source.position
    source.position = source.position + x
    Js.String2.slice(source.str, ~from=start, ~to_=source.position)
  }

  let loc = x => Loc(x.position)
}
open Scanner

let endOfNumber = (. c) =>
  switch c {
  | "-" | "+" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." | "e" | "E" => false
  | _ => true
  }

let notWhiteSpace = (. c) =>
  switch c {
  | " " | "\t" | "\n" | "\r" => false
  | _ => true
  }

open Tokens

type t = {tokens: Queue.t<Tokens.t>, name: option<identifier>}

let rec readStringAux = (source, position) => {
  switch peekCharAt(source, position) {
  | "" => readSubstringBy(source, position)
  | "{" =>
    switch peekCharAt(source, position + 1) {
    | "%" | "*" | "{" => readSubstringBy(source, position)
    | _ => readStringAux(source, position + 2)
    }
  | _ => readStringAux(source, position + 1)
  }
}

let readString = (source, tokens) => {
  let loc = loc(source)
  let s = readStringAux(source, 0)
  Queue.add(tokens, String(loc, s))
}

let readComment = (source, ~name) => {
  let loc = loc(source)
  let rec aux = (~position, ~nested) =>
    switch peekCharAt(source, position) {
    | "{" =>
      switch peekCharAt(source, position + 1) {
      | "*" => aux(~position=position + 2, ~nested=nested + 1)
      | _ => aux(~position=position + 1, ~nested)
      }
    | "*" =>
      switch peekCharAt(source, position + 1) {
      | "}" when nested == 0 =>
        let x = readSubstringBy(source, position)
        skipBy(source, 2)
        x
      | "}" => aux(~position=position + 2, ~nested=nested - 1)
      | _ => aux(~position=position + 2, ~nested)
      }
    | "" => raise(CompileError(unterminatedComment(~loc, ~name)))
    | _ => aux(~position=position + 1, ~nested)
    }
  aux(~position=0, ~nested=0)
}

let unescapeQuotes = %re(`/\\\"/g`)

let readJsonString = (source, ~name) => {
  let loc = loc(source)
  let rec aux = position => {
    switch peekCharAt(source, position) {
    | "\\" =>
      switch peekCharAt(source, position + 1) {
      | "\"" | "\\" => aux(position + 2)
      | _ => aux(position + 1)
      }
    | "\"" =>
      let result = source->readSubstringBy(position)->Js.String2.replaceByRe(unescapeQuotes, "\"")
      skipChar(source) // skip the "
      result
    | "" => raise(CompileError(unterminatedString(~loc, ~name)))
    | _ => aux(position + 1)
    }
  }
  aux(0)
}

let readNumber = (source, ~name) => {
  let loc = loc(source)
  let value = readSubstring(source, ~until=endOfNumber)
  switch Belt.Float.fromString(value) {
  | Some(num) => num
  | None => raise(CompileError(illegalIdentifier(~loc, ~identifier=Id(value), ~name)))
  }
}

let makeExpression = (source, tokens, ~name) => {
  let expression = ref(true)
  while expression.contents {
    let loc = loc(source)
    switch peekChar(source) {
    | "" => raise(CompileError(unexpectedEoF(~loc, ~name)))
    | "%" =>
      skipChar(source)
      let loc = Scanner.loc(source)
      switch readChar(source) {
      | "}" => expression := false
      | c => raise(CompileError(unexpectedCharacter(~loc, ~expected=["}"], ~character=c, ~name)))
      }
    | " " | "\t" | "\n" | "\r" => skipChar(source)
    | "{" =>
      Queue.add(tokens, OpenBrace(loc))
      skipChar(source)
    | "}" =>
      Queue.add(tokens, CloseBrace(loc))
      skipChar(source)
    | "#" =>
      Queue.add(tokens, Block(loc))
      skipChar(source)
    | "/" =>
      Queue.add(tokens, Slash(loc))
      skipChar(source)
    | ":" =>
      Queue.add(tokens, Colon(loc))
      skipChar(source)
    | "[" =>
      Queue.add(tokens, OpenBracket(loc))
      skipChar(source)
    | "]" =>
      Queue.add(tokens, CloseBracket(loc))
      skipChar(source)
    | "," =>
      Queue.add(tokens, Comma(loc))
      skipChar(source)
    | "." =>
      switch readSubstringBy(source, 3) {
      | "..." => Queue.add(tokens, Spread(loc))
      | c => raise(CompileError(unexpectedCharacter(~loc, ~expected=["..."], ~character=c, ~name)))
      }
    | "=" =>
      Queue.add(tokens, Equals(loc))
      skipChar(source)
    | "\"" =>
      skipChar(source)
      Queue.add(tokens, JsonString(loc, readJsonString(source, ~name)))
    | "-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
      Queue.add(tokens, Number(loc, readNumber(source, ~name)))
    | "~" =>
      skipChar(source)
      Queue.add(tokens, Tilde(loc))
    | c when RegEx.isValidIdentifierStart(c) =>
      Queue.add(tokens, Identifier(loc, Id(readSubstring(source, ~until=RegEx.isEndOfIdentifier))))
    | c when RegEx.isValidComponentStart(c) =>
      Queue.add(
        tokens,
        ComponentName(loc, Id(readSubstring(source, ~until=RegEx.isEndOfIdentifier))),
      )
    | c => raise(CompileError(invalidCharacter(~loc, ~character=c, ~name)))
    }
  }
}

let readTildeMaybe = (source, tokens) => {
  if peekChar(source) == "~" {
    Queue.add(tokens, Tilde(loc(source)))
    skipChar(source)
  }
}

let make = (~name=?, str) => {
  let source = {
    str: str,
    position: 0,
  }
  let tokens = Queue.make()
  // All sources begin as strings
  readString(source, tokens)
  let char = ref(readChar(source))
  while char.contents != "" {
    switch char.contents {
    | "{" =>
      switch readChar(source) {
      | "%" =>
        makeExpression(source, tokens, ~name)
        readString(source, tokens)
      | "*" =>
        let loc = loc(source)
        Queue.add(tokens, Comment(loc, readComment(source, ~name)))
        readString(source, tokens)
      | "{" =>
        readTildeMaybe(source, tokens)
        skip(source, ~until=notWhiteSpace)
        switch peekChar(source) {
        | "\"" =>
          let loc = loc(source)
          skipChar(source)
          Queue.add(tokens, EchoString(loc, readJsonString(source, ~name)))
        | "-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
          let loc = loc(source)
          Queue.add(tokens, EchoNumber(loc, readNumber(source, ~name)))
        | c when RegEx.isValidIdentifierStart(c) =>
          let loc = loc(source)
          Queue.add(
            tokens,
            EchoIdentifier(loc, Id(readSubstring(source, ~until=RegEx.isEndOfIdentifier))),
          )
        | c when RegEx.isValidComponentStart(c) =>
          let loc = loc(source)
          Queue.add(
            tokens,
            EchoChildComponent(loc, Id(readSubstring(source, ~until=RegEx.isEndOfIdentifier))),
          )
        | c => raise(CompileError(invalidCharacter(~loc=loc(source), ~character=c, ~name)))
        }
        skip(source, ~until=notWhiteSpace)
        readTildeMaybe(source, tokens)
        switch readSubstringBy(source, 2) {
        | "}}" => readString(source, tokens)
        | c =>
          raise(
            CompileError(
              unexpectedCharacter(~loc=loc(source), ~expected=["}}"], ~character=c, ~name),
            ),
          )
        }
      | c =>
        raise(
          CompileError(
            unexpectedCharacter(~loc=loc(source), ~expected=["{", "%", "*"], ~character=c, ~name),
          ),
        )
      }
    | c =>
      raise(
        CompileError(
          unexpectedCharacter(
            ~loc=loc(source),
            ~expected=["{", "[end of file]"],
            ~character=c,
            ~name,
          ),
        ),
      )
    }
    char := readChar(source)
  }
  Queue.add(tokens, EndOfFile(loc(source)))
  {tokens: tokens, name: name}
}

let peekExn = x => Queue.peekExn(x.tokens)

let popExn = x => Queue.popExn(x.tokens)

let skipExn = x => ignore(Queue.popExn(x.tokens))

let name = x => x.name

let debugToArray = x => Queue.toArray(x.tokens)
