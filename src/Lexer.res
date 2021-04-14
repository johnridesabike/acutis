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

module T = Acutis_Types

module Queue = Belt.MutableQueue
module Token = T.Token

exception Exit = Debug.Exit

type source = {
  str: string,
  mutable position: int,
}

let peekChar = source => Js.String2.charAt(source.str, source.position)

let skipChar = source => source.position = min(succ(source.position), Js.String2.length(source.str))

let readChar = source => {
  let c = peekChar(source)
  skipChar(source)
  c
}

let peek = (source, ~until) => {
  let position = ref(source.position)
  while !until(. Js.String2.charAt(source.str, position.contents)) {
    position := succ(position.contents)
  }
  position.contents
}

let readSubstring = (source, ~until) => {
  let start = source.position
  let end = peek(source, ~until)
  source.position = min(end, Js.String2.length(source.str))
  Js.String2.slice(source.str, ~from=start, ~to_=end)
}

let readSubstringBy = (source, x) => {
  let start = source.position
  source.position = min(source.position + x, Js.String2.length(source.str))
  Js.String2.slice(source.str, ~from=start, ~to_=source.position)
}

let loc = x => T.Loc(x.position)

let endOfNumber = (. c) =>
  switch c {
  | "-" | "+" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." | "e" | "E" => false
  | _ => true
  }

type t = {tokens: Queue.t<Token.t>, name: string}

type mode = EchoMode | ExpressionMode | CommentMode | EndMode

let readText = (source, tokens: Queue.t<Token.t>) => {
  let loc' = loc(source)
  let rec aux = str =>
    switch readChar(source) {
    | "" =>
      Queue.add(tokens, Text(loc', str))
      EndMode
    | "{" as c =>
      switch readChar(source) {
      | "%" =>
        Queue.add(tokens, Text(loc', str))
        ExpressionMode
      | "*" =>
        Queue.add(tokens, Text(loc', str))
        CommentMode
      | "{" =>
        Queue.add(tokens, Text(loc', str))
        EchoMode
      | c' => aux(str ++ c ++ c')
      }
    | c => aux(str ++ c)
    }
  aux("")
}

let readComment = (source, ~name) => {
  let rec aux = (str, ~nested) =>
    switch readChar(source) {
    | "{" as c =>
      switch readChar(source) {
      | "*" as c' => aux(str ++ c ++ c', ~nested=nested + 1)
      | c' => aux(str ++ c ++ c', ~nested)
      }
    | "*" as c =>
      switch readChar(source) {
      | "}" if nested == 0 => str
      | "}" as c' => aux(str ++ c ++ c', ~nested=nested - 1)
      | c' => aux(str ++ c ++ c', ~nested)
      }
    | "" => raise(Exit(Debug.unterminatedComment(~loc=loc(source), ~name)))
    | c => aux(str ++ c, ~nested)
    }
  aux("", ~nested=0)
}

let readJsonString = (source, ~name) => {
  let rec aux = str =>
    switch readChar(source) {
    | "\\" =>
      switch readChar(source) {
      | ("\"" | "\\") as c => aux(str ++ c)
      | c => raise(Exit(Debug.unknownEscapeSequence(~loc=loc(source), ~name, ~char=c)))
      }
    | "\"" => str
    | "" => raise(Exit(Debug.unterminatedString(~loc=loc(source), ~name)))
    | c => aux(str ++ c)
    }
  aux("")
}

let readNumber = (source, ~name) => {
  let loc = loc(source)
  let num = readSubstring(source, ~until=endOfNumber)
  switch Belt.Float.fromString(num) {
  | Some(num) => num
  | None => raise(Exit(Debug.illegalIdentifier(~loc, ~identifier=num, ~name)))
  }
}

let identifierChar = %re("/^[a-zA-Z0-9_]$/")
let endOfIdentifier = (. s) => !Js.Re.test_(identifierChar, s)

let identifierStartChar = %re("/^[a-z_]$/")
let isValidIdentifierStart = c => Js.Re.test_(identifierStartChar, c)

let componentStart = %re("/^[A-Z]$/")
let isValidComponentStart = c => Js.Re.test_(componentStart, c)

let readIdentifier = (source, loc): Token.t =>
  switch readSubstring(source, ~until=endOfIdentifier) {
  | "true" => True(loc)
  | "false" => False(loc)
  | "null" => Null(loc)
  | s => Identifier(loc, s)
  }

let makeExpression = (source, tokens: Queue.t<Token.t>, ~name, ~until) => {
  let loop = ref(true)
  while loop.contents {
    let loc = loc(source)
    switch peekChar(source) {
    | c if c == until =>
      skipChar(source)
      loop := false
    | "" => raise(Exit(Debug.unexpectedEof(~loc, ~name)))
    | " " | "\t" | "\n" | "\r" => skipChar(source)
    | "{" =>
      skipChar(source)
      Queue.add(tokens, OpenBrace(loc))
    | "}" =>
      skipChar(source)
      Queue.add(tokens, CloseBrace(loc))
    | "#" =>
      skipChar(source)
      Queue.add(tokens, Block(loc))
    | "/" =>
      skipChar(source)
      Queue.add(tokens, Slash(loc))
    | ":" =>
      skipChar(source)
      Queue.add(tokens, Colon(loc))
    | "[" =>
      skipChar(source)
      Queue.add(tokens, OpenBracket(loc))
    | "]" =>
      skipChar(source)
      Queue.add(tokens, CloseBracket(loc))
    | "," =>
      skipChar(source)
      Queue.add(tokens, Comma(loc))
    | "." =>
      switch readSubstringBy(source, 3) {
      | "..." => Queue.add(tokens, Spread(loc))
      | c => raise(Exit(Debug.unexpectedCharacter(~loc, ~expected="...", ~character=c, ~name)))
      }
    | "=" =>
      skipChar(source)
      Queue.add(tokens, Equals(loc))
    | "\"" =>
      skipChar(source)
      Queue.add(tokens, String(loc, readJsonString(source, ~name)))
    | "~" =>
      skipChar(source)
      Queue.add(tokens, Tilde(loc))
    | "?" =>
      skipChar(source)
      Queue.add(tokens, Question(loc))
    | "&" =>
      skipChar(source)
      Queue.add(tokens, Ampersand(loc))
    | "-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
      Queue.add(tokens, Number(loc, readNumber(source, ~name)))
    | c if isValidIdentifierStart(c) => Queue.add(tokens, readIdentifier(source, loc))
    | c if isValidComponentStart(c) =>
      Queue.add(tokens, ComponentName(loc, readSubstring(source, ~until=endOfIdentifier)))
    | c => raise(Exit(Debug.invalidCharacter(~loc, ~character=c, ~name)))
    }
  }
}

let make = (~name, str) => {
  let source = {str: str, position: 0}
  let tokens: Queue.t<Token.t> = Queue.make()

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
        raise(Exit(Debug.unexpectedCharacter(~loc=loc(source), ~expected="}", ~character=c, ~name)))
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
        raise(Exit(Debug.unexpectedCharacter(~loc=loc(source), ~expected="}", ~character=c, ~name)))
      }
    }
  // All sources begin as text
  aux(readText(source, tokens))
}

let peekExn = x => Queue.peekExn(x.tokens)

let popExn = x => Queue.popExn(x.tokens)

let name = x => x.name

let debugToArray = x => Queue.toArray(x.tokens)
