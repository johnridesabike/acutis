/**
   Copyright 2021 John Jackson

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
    let position = ref(source.position)
    while !until(. Js.String2.charAt(source.str, position.contents)) {
      position := position.contents + 1
    }
    position.contents
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

open Token

type t = {tokens: Queue.t<Token.t>, name: option<string>}

type mode = EchoMode | ExpressionMode | CommentMode | EndMode

let readText = (source, tokens) => {
  let loc = loc(source)
  let rec aux = position =>
    switch peekCharAt(source, position) {
    | "" =>
      Queue.add(tokens, Text(loc, readSubstringBy(source, position)))
      EndMode
    | "{" =>
      switch peekCharAt(source, position + 1) {
      | "%" =>
        Queue.add(tokens, Text(loc, readSubstringBy(source, position)))
        skipBy(source, 2)
        ExpressionMode
      | "*" =>
        Queue.add(tokens, Text(loc, readSubstringBy(source, position)))
        skipBy(source, 2)
        CommentMode
      | "{" =>
        Queue.add(tokens, Text(loc, readSubstringBy(source, position)))
        skipBy(source, 2)
        EchoMode
      | _ => aux(position + 2)
      }
    | _ => aux(position + 1)
    }
  aux(0)
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
        let result = readSubstringBy(source, position)
        skipBy(source, 2)
        result
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
  let rec aux = position =>
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
  aux(0)
}

let readNumber = (source, ~name) => {
  let loc = loc(source)
  let num = readSubstring(source, ~until=endOfNumber)
  switch Belt.Float.fromString(num) {
  | Some(num) => num
  | None => raise(CompileError(illegalIdentifier(~loc, ~identifier=num, ~name)))
  }
}

let makeExpression = (source, tokens, ~name, ~until) => {
  let loop = ref(true)
  while loop.contents {
    let loc = loc(source)
    switch peekChar(source) {
    | c when c == until =>
      skipChar(source)
      Queue.add(tokens, EndOfExpression(loc, until))
      loop := false
    | "" => raise(CompileError(unexpectedEoF(~loc, ~name)))
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
      | c => raise(CompileError(unexpectedCharacter(~loc, ~expected="...", ~character=c, ~name)))
      }
    | "=" =>
      Queue.add(tokens, Equals(loc))
      skipChar(source)
    | "\"" =>
      skipChar(source)
      Queue.add(tokens, String(loc, readJsonString(source, ~name)))
    | "~" =>
      skipChar(source)
      // Tildes are special-cased for end of expressions.
      // The tilde must come *after* the end-of-expression token.
      switch peekChar(source) {
      | c when c == until =>
        skipChar(source)
        Queue.add(tokens, EndOfExpression(loc, until))
        Queue.add(tokens, Tilde(loc))
        loop := false
      | _ => Queue.add(tokens, Tilde(loc))
      }
    | "?" =>
      skipChar(source)
      Queue.add(tokens, Question(loc))
    | "-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
      Queue.add(tokens, Number(loc, readNumber(source, ~name)))
    | c when RegEx.isValidIdentifierStart(c) =>
      Queue.add(tokens, Identifier(loc, readSubstring(source, ~until=RegEx.isEndOfIdentifier)))
    | c when RegEx.isValidComponentStart(c) =>
      Queue.add(tokens, ComponentName(loc, readSubstring(source, ~until=RegEx.isEndOfIdentifier)))
    | c => raise(CompileError(invalidCharacter(~loc, ~character=c, ~name)))
    }
  }
}

let make = (~name=?, str) => {
  let source = {
    str: str,
    position: 0,
  }
  let tokens = Queue.make()
  let rec aux = mode =>
    switch mode {
    | EndMode =>
      Queue.add(tokens, EndOfFile(loc(source)))
      {tokens: tokens, name: name}
    | CommentMode =>
      let loc = loc(source)
      Queue.add(tokens, Comment(loc, readComment(source, ~name)))
      aux(readText(source, tokens))
    | ExpressionMode =>
      makeExpression(source, tokens, ~name, ~until="%")
      switch readChar(source) {
      | "}" => aux(readText(source, tokens))
      | c =>
        raise(
          CompileError(unexpectedCharacter(~loc=loc(source), ~expected="}", ~character=c, ~name)),
        )
      }
    | EchoMode =>
      // The tilde must come *before* the echo token.
      let echoLoc = loc(source)
      if peekChar(source) == "~" {
        Queue.add(tokens, Tilde(loc(source)))
        skipChar(source)
      }
      Queue.add(tokens, Echo(echoLoc))
      makeExpression(source, tokens, ~name, ~until="}")
      switch readChar(source) {
      | "}" => aux(readText(source, tokens))
      | c =>
        raise(
          CompileError(unexpectedCharacter(~loc=loc(source), ~expected="}", ~character=c, ~name)),
        )
      }
    }
  // All sources begin as text
  aux(readText(source, tokens))
}

let peekExn = x => Queue.peekExn(x.tokens)

let popExn = x => Queue.popExn(x.tokens)

let name = x => x.name

let debugToArray = x => Queue.toArray(x.tokens)
