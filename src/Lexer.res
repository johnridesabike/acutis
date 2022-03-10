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
    | Tkn_Text(Debug.t, string)
    | Tkn_Comment(Debug.t, string)
    // JSON values
    | Tkn_String(Debug.t, string)
    | Tkn_Int(Debug.t, int)
    | Tkn_Float(Debug.t, float)
    | Tkn_True(Debug.t) // a reserved identifier
    | Tkn_False(Debug.t) // a reserved identifier
    | Tkn_Null(Debug.t) // a reserved identifier
    // JSON syntax
    | Tkn_Comma(Debug.t)
    | Tkn_Colon(Debug.t)
    | Tkn_OpenBracket(Debug.t)
    | Tkn_CloseBracket(Debug.t)
    | Tkn_OpenBrace(Debug.t)
    | Tkn_CloseBrace(Debug.t)
    | Tkn_OpenParen(Debug.t)
    | Tkn_CloseParen(Debug.t)
    | Tkn_OpenPointyBracket(Debug.t)
    | Tkn_ClosePointyBracket(Debug.t)
    | Tkn_Spread(Debug.t)
    // Component syntax
    | Tkn_ComponentName(Debug.t, string)
    | Tkn_Slash(Debug.t)
    | Tkn_Block(Debug.t)
    | Tkn_Equals(Debug.t)
    // Dynamic content
    | Tkn_Identifier(Debug.t, string)
    | Tkn_Tilde(Debug.t)
    | Tkn_Question(Debug.t)
    | Tkn_Ampersand(Debug.t)
    | Tkn_Bang(Debug.t)
    | Tkn_At(Debug.t)
    | Tkn_Echo(Debug.t)
    | Tkn_EndOfFile(Debug.t)

  let toString = x =>
    switch x {
    | Tkn_Text(_, x) => "[text]: " ++ x
    | Tkn_String(_, x) => `"${x}"`
    | Tkn_Int(_, x) => Belt.Int.toString(x)
    | Tkn_Float(_, x) => Belt.Float.toString(x)
    | Tkn_True(_) => "true"
    | Tkn_False(_) => "false"
    | Tkn_Null(_) => "null"
    | Tkn_Identifier(_, x) => x
    | Tkn_ComponentName(_, x) => x
    | Tkn_Comment(_, x) => `{*${x}*}`
    | Tkn_Comma(_) => ","
    | Tkn_Colon(_) => ":"
    | Tkn_Slash(_) => "/"
    | Tkn_OpenBracket(_) => "["
    | Tkn_CloseBracket(_) => "]"
    | Tkn_OpenBrace(_) => "{"
    | Tkn_CloseBrace(_) => "}"
    | Tkn_OpenParen(_) => "("
    | Tkn_CloseParen(_) => ")"
    | Tkn_OpenPointyBracket(_) => "<"
    | Tkn_ClosePointyBracket(_) => ">"
    | Tkn_Spread(_) => "..."
    | Tkn_Block(_) => "#"
    | Tkn_Equals(_) => "="
    | Tkn_Tilde(_) => "~"
    | Tkn_Question(_) => "?"
    | Tkn_Ampersand(_) => "&"
    | Tkn_Bang(_) => "!"
    | Tkn_At(_) => "@"
    | Tkn_Echo(_) => "{{"
    | Tkn_EndOfFile(_) => "[end of file]"
    }

  let debug = x =>
    switch x {
    | Tkn_Text(x, _)
    | Tkn_String(x, _)
    | Tkn_Int(x, _)
    | Tkn_Float(x, _)
    | Tkn_Identifier(x, _)
    | Tkn_True(x)
    | Tkn_False(x)
    | Tkn_Null(x)
    | Tkn_ComponentName(x, _)
    | Tkn_Comment(x, _)
    | Tkn_Comma(x)
    | Tkn_Colon(x)
    | Tkn_Slash(x)
    | Tkn_OpenBracket(x)
    | Tkn_CloseBracket(x)
    | Tkn_OpenBrace(x)
    | Tkn_CloseBrace(x)
    | Tkn_OpenParen(x)
    | Tkn_CloseParen(x)
    | Tkn_OpenPointyBracket(x)
    | Tkn_ClosePointyBracket(x)
    | Tkn_Spread(x)
    | Tkn_Block(x)
    | Tkn_Equals(x)
    | Tkn_Tilde(x)
    | Tkn_Question(x)
    | Tkn_Ampersand(x)
    | Tkn_Echo(x)
    | Tkn_Bang(x)
    | Tkn_At(x)
    | Tkn_EndOfFile(x) => x
    }
}

type source = {
  name: string,
  str: string,
  mutable pos: int,
}

let peekChar = source => Js.String2.charAt(source.str, source.pos)

let skipChar = source => source.pos = min(succ(source.pos), Js.String2.length(source.str))

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

let endOfInt = (. c) =>
  switch c {
  | "-" | "+" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "e" | "E" => false
  | _ => true
  }

type t = Queue.t<Token.t>

type mode = EchoMode | ExpressionMode | CommentMode | EndMode

// Peeking at each character and slicing it at the end is much more performant
// than consuming each character at a time.
@raises(Exit)
let readText = (src, tokens: Queue.t<Token.t>) => {
  let debug = Debug.make(src.name, src.pos)

  @raises(Exit)
  let rec aux = pos =>
    switch Js.String2.charAt(src.str, pos) {
    | "" =>
      Queue.add(tokens, Tkn_Text(debug, Js.String2.slice(src.str, ~from=src.pos, ~to_=pos)))
      src.pos = pos
      EndMode
    | "{" =>
      switch Js.String2.charAt(src.str, pos + 1) {
      | "%" =>
        Queue.add(tokens, Tkn_Text(debug, Js.String2.slice(src.str, ~from=src.pos, ~to_=pos)))
        src.pos = pos + 2
        ExpressionMode
      | "*" =>
        Queue.add(tokens, Tkn_Text(debug, Js.String2.slice(src.str, ~from=src.pos, ~to_=pos)))
        src.pos = pos + 2
        CommentMode
      | "{" =>
        Queue.add(tokens, Tkn_Text(debug, Js.String2.slice(src.str, ~from=src.pos, ~to_=pos)))
        src.pos = pos + 2
        EchoMode
      | _ => aux(pos + 2)
      }
    | _ => aux(succ(pos))
    }
  aux(src.pos)
}

@raises(Exit)
let readComment = src => {
  @raises(Exit)
  let rec aux = (pos, ~nested) =>
    switch Js.String2.charAt(src.str, pos) {
    | "{" =>
      switch Js.String2.charAt(src.str, pos + 1) {
      | "*" => aux(pos + 2, ~nested=nested + 1)
      | _ => aux(pos + 2, ~nested)
      }
    | "*" =>
      switch Js.String2.charAt(src.str, pos + 1) {
      | "}" if nested == 0 =>
        let result = Js.String2.slice(src.str, ~from=src.pos, ~to_=pos)
        src.pos = pos + 2
        result
      | "}" => aux(pos + 2, ~nested=nested - 1)
      | _ => aux(pos + 2, ~nested)
      }
    | "" => raise(Exit(Debug.unterminatedComment(Debug.make(src.name, src.pos))))
    | _ => aux(pos + 1, ~nested)
    }
  aux(src.pos, ~nested=0)
}

@raises(Exit)
let readJsonString = src => {
  @raises(Exit)
  let rec aux = str =>
    switch readChar(src) {
    | "\\" =>
      switch readChar(src) {
      | ("\"" | "\\") as c => aux(str ++ c)
      | c =>
        let debug = Debug.make(src.name, src.pos)
        raise(Exit(Debug.unknownEscapeSequence(debug, c)))
      }
    | "\"" => str
    | "" =>
      let debug = Debug.make(src.name, src.pos)
      raise(Exit(Debug.unterminatedString(debug)))
    | c => aux(str ++ c)
    }
  aux("")
}

@raises(Exit)
let readNumber = (c, src, debug) => {
  let intStr = readSubstring(c, src, ~until=endOfInt)
  switch peekChar(src) {
  | "." =>
    let floatStr = intStr ++ readSubstring(readChar(src), src, ~until=endOfInt)
    switch Float.fromString(floatStr) {
    | Some(num) => Token.Tkn_Float(debug, num)
    | None => raise(Exit(Debug.illegalIdentifier(debug, floatStr)))
    }
  | _ =>
    // Int.fromString isn't consistent with Float.fromString number syntax.
    switch Float.fromString(intStr) {
    | Some(num) => Tkn_Int(debug, Int.fromFloat(num))
    | None => raise(Exit(Debug.illegalIdentifier(debug, intStr)))
    }
  }
}

let identifierChar = %re("/^[a-zA-Z0-9_]$/")
let endOfIdentifier = (. s) => !Js.Re.test_(identifierChar, s)

let identifierStartChar = %re("/^[a-z_]$/")
let isValidIdentifierStart = c => Js.Re.test_(identifierStartChar, c)

let componentStart = %re("/^[A-Z]$/")
let isValidComponentStart = c => Js.Re.test_(componentStart, c)

let readIdentifier = (c, src, debug) =>
  switch readSubstring(c, src, ~until=endOfIdentifier) {
  | "true" => Token.Tkn_True(debug)
  | "false" => Tkn_False(debug)
  | "null" => Tkn_Null(debug)
  | s => Tkn_Identifier(debug, s)
  }

@raises(Exit)
let makeExpression = (src, tokens: Queue.t<Token.t>, ~until) => {
  let loop = ref(true)
  while loop.contents {
    let d = Debug.make(src.name, src.pos)
    switch readChar(src) {
    | c if c == until => loop := false
    | "" => raise(Exit(Debug.unexpectedEof(d)))
    | " " | "\t" | "\n" | "\r" => ()
    | "{" => Queue.add(tokens, Tkn_OpenBrace(d))
    | "}" => Queue.add(tokens, Tkn_CloseBrace(d))
    | "#" => Queue.add(tokens, Tkn_Block(d))
    | "/" => Queue.add(tokens, Tkn_Slash(d))
    | ":" => Queue.add(tokens, Tkn_Colon(d))
    | "[" => Queue.add(tokens, Tkn_OpenBracket(d))
    | "]" => Queue.add(tokens, Tkn_CloseBracket(d))
    | "(" => Queue.add(tokens, Tkn_OpenParen(d))
    | ")" => Queue.add(tokens, Tkn_CloseParen(d))
    | "<" => Queue.add(tokens, Tkn_OpenPointyBracket(d))
    | ">" => Queue.add(tokens, Tkn_ClosePointyBracket(d))
    | "," => Queue.add(tokens, Tkn_Comma(d))
    | "." =>
      switch (readChar(src), readChar(src)) {
      | (".", ".") => Queue.add(tokens, Tkn_Spread(d))
      | (".", c) | (c, _) => raise(Exit(Debug.unexpectedCharacter(d, ~expected=".", ~character=c)))
      }
    | "=" => Queue.add(tokens, Tkn_Equals(d))
    | "\"" => Queue.add(tokens, Tkn_String(d, readJsonString(src)))
    | "~" => Queue.add(tokens, Tkn_Tilde(d))
    | "?" => Queue.add(tokens, Tkn_Question(d))
    | "&" => Queue.add(tokens, Tkn_Ampersand(d))
    | "!" => Queue.add(tokens, Tkn_Bang(d))
    | "@" => Queue.add(tokens, Tkn_At(d))
    | ("-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") as c =>
      Queue.add(tokens, readNumber(c, src, d))
    | c if isValidIdentifierStart(c) => Queue.add(tokens, readIdentifier(c, src, d))
    | c if isValidComponentStart(c) =>
      Queue.add(tokens, Tkn_ComponentName(d, readSubstring(c, src, ~until=endOfIdentifier)))
    | c => raise(Exit(Debug.invalidCharacter(d, c)))
    }
  }
}

@raises(Exit)
let make = (~name, str) => {
  let src = {name: name, str: str, pos: 0}
  let tokens: Queue.t<Token.t> = Queue.make()

  @raises(Exit)
  let rec aux = mode =>
    switch mode {
    | EndMode =>
      Queue.add(tokens, Tkn_EndOfFile(Debug.make(name, src.pos)))
      tokens
    | CommentMode =>
      let debug = Debug.make(name, src.pos)
      Queue.add(tokens, Tkn_Comment(debug, readComment(src)))
      aux(readText(src, tokens))
    | ExpressionMode =>
      makeExpression(src, tokens, ~until="%")
      switch readChar(src) {
      | "}" => aux(readText(src, tokens))
      | c =>
        raise(
          Exit(Debug.unexpectedCharacter(Debug.make(name, src.pos), ~expected="}", ~character=c)),
        )
      }
    | EchoMode =>
      // The tilde must come *before* the echo token.
      let echoDebug = Debug.make(name, src.pos)
      if peekChar(src) == "~" {
        Queue.add(tokens, Tkn_Tilde(Debug.make(name, src.pos)))
        skipChar(src)
      }
      Queue.add(tokens, Tkn_Echo(echoDebug))
      makeExpression(src, tokens, ~until="}")
      switch readChar(src) {
      | "}" => aux(readText(src, tokens))
      | c =>
        raise(
          Exit(Debug.unexpectedCharacter(Debug.make(name, src.pos), ~expected="}", ~character=c)),
        )
      }
    }
  // All sources begin as text
  aux(readText(src, tokens))
}

let peek = t => Queue.peekExn(t)

let pop = t => Queue.popExn(t)

let debugToArray = t => Queue.toArray(t)
