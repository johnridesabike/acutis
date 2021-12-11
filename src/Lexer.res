/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Int = Belt.Int
module Float = Belt.Float
module Queue = Belt.MutableQueue

exception Exit = Debug.Exit

module Token = {
  type t =
    // Static elements
    | Text(Debug.loc, string)
    | Comment(Debug.loc, string)
    // JSON values
    | String(Debug.loc, string)
    | Int(Debug.loc, int)
    | Float(Debug.loc, float)
    | True(Debug.loc) // a reserved identifier
    | False(Debug.loc) // a reserved identifier
    | Null(Debug.loc) // a reserved identifier
    // JSON syntax
    | Comma(Debug.loc)
    | Colon(Debug.loc)
    | OpenBracket(Debug.loc)
    | CloseBracket(Debug.loc)
    | OpenBrace(Debug.loc)
    | CloseBrace(Debug.loc)
    | OpenParen(Debug.loc)
    | CloseParen(Debug.loc)
    | OpenPointyBracket(Debug.loc)
    | ClosePoointyBracket(Debug.loc)
    | Spread(Debug.loc)
    // Component syntax
    | ComponentName(Debug.loc, string)
    | Slash(Debug.loc)
    | Block(Debug.loc)
    | Equals(Debug.loc)
    // Dynamic content
    | Identifier(Debug.loc, string)
    | Tilde(Debug.loc)
    | Question(Debug.loc)
    | Ampersand(Debug.loc)
    | Bang(Debug.loc)
    | Echo(Debug.loc)
    | EndOfFile(Debug.loc)

  let toString = x =>
    switch x {
    | Text(_, x) => "[text]: " ++ x
    | String(_, x) => `"${x}"`
    | Int(_, x) => Belt.Int.toString(x)
    | Float(_, x) => Belt.Float.toString(x)
    | True(_) => "true"
    | False(_) => "false"
    | Null(_) => "null"
    | Identifier(_, x) => x
    | ComponentName(_, x) => x
    | Comment(_, x) => `{*${x}*}`
    | Comma(_) => ","
    | Colon(_) => ":"
    | Slash(_) => "/"
    | OpenBracket(_) => "["
    | CloseBracket(_) => "]"
    | OpenBrace(_) => "{"
    | CloseBrace(_) => "}"
    | OpenParen(_) => "("
    | CloseParen(_) => ")"
    | OpenPointyBracket(_) => "<"
    | ClosePoointyBracket(_) => ">"
    | Spread(_) => "..."
    | Block(_) => "#"
    | Equals(_) => "="
    | Tilde(_) => "~"
    | Question(_) => "?"
    | Ampersand(_) => "&"
    | Bang(_) => "!"
    | Echo(_) => "{{"
    | EndOfFile(_) => "[end of file]"
    }

  let toLocation = x =>
    switch x {
    | Text(x, _)
    | String(x, _)
    | Int(x, _)
    | Float(x, _)
    | Identifier(x, _)
    | True(x)
    | False(x)
    | Null(x)
    | ComponentName(x, _)
    | Comment(x, _)
    | Comma(x)
    | Colon(x)
    | Slash(x)
    | OpenBracket(x)
    | CloseBracket(x)
    | OpenBrace(x)
    | CloseBrace(x)
    | OpenParen(x)
    | CloseParen(x)
    | OpenPointyBracket(x)
    | ClosePoointyBracket(x)
    | Spread(x)
    | Block(x)
    | Equals(x)
    | Tilde(x)
    | Question(x)
    | Ampersand(x)
    | Echo(x)
    | Bang(x)
    | EndOfFile(x) => x
    }
}

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

let rec readSubstring = (str, source, ~until) =>
  if until(. peekChar(source)) {
    str
  } else {
    readSubstring(str ++ readChar(source), source, ~until)
  }

let loc = x => Debug.Loc(x.position)

let endOfInt = (. c) =>
  switch c {
  | "-" | "+" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "e" | "E" => false
  | _ => true
  }

type t = {tokens: Queue.t<Token.t>, name: string}

type mode = EchoMode | ExpressionMode | CommentMode | EndMode

// Peeking at each character and slicing it at the end is much more performant
// than consuming each character at a time.
@raises(Exit)
let readText = (source, tokens: Queue.t<Token.t>) => {
  let loc = loc(source)

  @raises(Exit)
  let rec aux = pos =>
    switch Js.String2.charAt(source.str, pos) {
    | "" =>
      Queue.add(tokens, Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)))
      source.position = pos
      EndMode
    | "{" =>
      switch Js.String2.charAt(source.str, pos + 1) {
      | "%" =>
        Queue.add(tokens, Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)))
        source.position = pos + 2
        ExpressionMode
      | "*" =>
        Queue.add(tokens, Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)))
        source.position = pos + 2
        CommentMode
      | "{" =>
        Queue.add(tokens, Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)))
        source.position = pos + 2
        EchoMode
      | _ => aux(pos + 2)
      }
    | _ => aux(succ(pos))
    }
  aux(source.position)
}

@raises(Exit)
let readComment = (source, ~name) => {
  @raises(Exit)
  let rec aux = (pos, ~nested) =>
    switch Js.String2.charAt(source.str, pos) {
    | "{" =>
      switch Js.String2.charAt(source.str, pos + 1) {
      | "*" => aux(pos + 2, ~nested=nested + 1)
      | _ => aux(pos + 2, ~nested)
      }
    | "*" =>
      switch Js.String2.charAt(source.str, pos + 1) {
      | "}" if nested == 0 =>
        let result = Js.String2.slice(source.str, ~from=source.position, ~to_=pos)
        source.position = pos + 2
        result
      | "}" => aux(pos + 2, ~nested=nested - 1)
      | _ => aux(pos + 2, ~nested)
      }
    | "" => raise(Exit(Debug.unterminatedComment(~loc=loc(source), ~name)))
    | _ => aux(pos + 1, ~nested)
    }
  aux(source.position, ~nested=0)
}

@raises(Exit)
let readJsonString = (source, ~name) => {
  @raises(Exit)
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

@raises(Exit)
let readNumber = (c, source, ~loc, ~name): Token.t => {
  let intStr = readSubstring(c, source, ~until=endOfInt)
  switch peekChar(source) {
  | "." =>
    let floatStr = intStr ++ readSubstring(readChar(source), source, ~until=endOfInt)
    switch Float.fromString(floatStr) {
    | Some(num) => Float(loc, num)
    | None => raise(Exit(Debug.illegalIdentifier(~loc, ~identifier=floatStr, ~name)))
    }
  | _ =>
    // Int.fromString isn't consistent with Float.fromString number syntax.
    switch Float.fromString(intStr) {
    | Some(num) => Int(loc, Int.fromFloat(num))
    | None => raise(Exit(Debug.illegalIdentifier(~loc, ~identifier=intStr, ~name)))
    }
  }
}

let identifierChar = %re("/^[a-zA-Z0-9_]$/")
let endOfIdentifier = (. s) => !Js.Re.test_(identifierChar, s)

let identifierStartChar = %re("/^[a-z_]$/")
let isValidIdentifierStart = c => Js.Re.test_(identifierStartChar, c)

let componentStart = %re("/^[A-Z]$/")
let isValidComponentStart = c => Js.Re.test_(componentStart, c)

let readIdentifier = (c, source, loc): Token.t =>
  switch readSubstring(c, source, ~until=endOfIdentifier) {
  | "true" => True(loc)
  | "false" => False(loc)
  | "null" => Null(loc)
  | s => Identifier(loc, s)
  }

@raises(Exit)
let makeExpression = (source, tokens: Queue.t<Token.t>, ~name, ~until) => {
  let loop = ref(true)
  while loop.contents {
    let loc = loc(source)
    switch readChar(source) {
    | c if c == until => loop := false
    | "" => raise(Exit(Debug.unexpectedEof(~loc, ~name)))
    | " " | "\t" | "\n" | "\r" => ()
    | "{" => Queue.add(tokens, OpenBrace(loc))
    | "}" => Queue.add(tokens, CloseBrace(loc))
    | "#" => Queue.add(tokens, Block(loc))
    | "/" => Queue.add(tokens, Slash(loc))
    | ":" => Queue.add(tokens, Colon(loc))
    | "[" => Queue.add(tokens, OpenBracket(loc))
    | "]" => Queue.add(tokens, CloseBracket(loc))
    | "(" => Queue.add(tokens, OpenParen(loc))
    | ")" => Queue.add(tokens, CloseParen(loc))
    | "<" => Queue.add(tokens, OpenPointyBracket(loc))
    | ">" => Queue.add(tokens, ClosePoointyBracket(loc))
    | "," => Queue.add(tokens, Comma(loc))
    | "." =>
      switch (readChar(source), readChar(source)) {
      | (".", ".") => Queue.add(tokens, Spread(loc))
      | (".", c) | (c, _) =>
        raise(Exit(Debug.unexpectedCharacter(~loc, ~expected=".", ~character=c, ~name)))
      }
    | "=" => Queue.add(tokens, Equals(loc))
    | "\"" => Queue.add(tokens, String(loc, readJsonString(source, ~name)))
    | "~" => Queue.add(tokens, Tilde(loc))
    | "?" => Queue.add(tokens, Question(loc))
    | "&" => Queue.add(tokens, Ampersand(loc))
    | "!" => Queue.add(tokens, Bang(loc))
    | ("-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") as c =>
      Queue.add(tokens, readNumber(c, source, ~loc, ~name))
    | c if isValidIdentifierStart(c) => Queue.add(tokens, readIdentifier(c, source, loc))
    | c if isValidComponentStart(c) =>
      Queue.add(tokens, ComponentName(loc, readSubstring(c, source, ~until=endOfIdentifier)))
    | c => raise(Exit(Debug.invalidCharacter(~loc, ~character=c, ~name)))
    }
  }
}

@raises(Exit)
let make = (~name, str) => {
  let source = {str: str, position: 0}
  let tokens: Queue.t<Token.t> = Queue.make()

  @raises(Exit)
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
