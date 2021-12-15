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
    | Tkn_Text(Debug.loc, string)
    | Tkn_Comment(Debug.loc, string)
    // JSON values
    | Tkn_String(Debug.loc, string)
    | Tkn_Int(Debug.loc, int)
    | Tkn_Float(Debug.loc, float)
    | Tkn_True(Debug.loc) // a reserved identifier
    | Tkn_False(Debug.loc) // a reserved identifier
    | Tkn_Null(Debug.loc) // a reserved identifier
    // JSON syntax
    | Tkn_Comma(Debug.loc)
    | Tkn_Colon(Debug.loc)
    | Tkn_OpenBracket(Debug.loc)
    | Tkn_CloseBracket(Debug.loc)
    | Tkn_OpenBrace(Debug.loc)
    | Tkn_CloseBrace(Debug.loc)
    | Tkn_OpenParen(Debug.loc)
    | Tkn_CloseParen(Debug.loc)
    | Tkn_OpenPointyBracket(Debug.loc)
    | Tkn_ClosePointyBracket(Debug.loc)
    | Tkn_Spread(Debug.loc)
    // Component syntax
    | Tkn_ComponentName(Debug.loc, string)
    | Tkn_Slash(Debug.loc)
    | Tkn_Block(Debug.loc)
    | Tkn_Equals(Debug.loc)
    // Dynamic content
    | Tkn_Identifier(Debug.loc, string)
    | Tkn_Tilde(Debug.loc)
    | Tkn_Question(Debug.loc)
    | Tkn_Ampersand(Debug.loc)
    | Tkn_Bang(Debug.loc)
    | Tkn_Echo(Debug.loc)
    | Tkn_EndOfFile(Debug.loc)

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
    | Tkn_Echo(_) => "{{"
    | Tkn_EndOfFile(_) => "[end of file]"
    }

  let toLocation = x =>
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
    | Tkn_EndOfFile(x) => x
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
      Queue.add(
        tokens,
        Tkn_Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)),
      )
      source.position = pos
      EndMode
    | "{" =>
      switch Js.String2.charAt(source.str, pos + 1) {
      | "%" =>
        Queue.add(
          tokens,
          Tkn_Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)),
        )
        source.position = pos + 2
        ExpressionMode
      | "*" =>
        Queue.add(
          tokens,
          Tkn_Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)),
        )
        source.position = pos + 2
        CommentMode
      | "{" =>
        Queue.add(
          tokens,
          Tkn_Text(loc, Js.String2.slice(source.str, ~from=source.position, ~to_=pos)),
        )
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
    | Some(num) => Tkn_Float(loc, num)
    | None => raise(Exit(Debug.illegalIdentifier(~loc, ~identifier=floatStr, ~name)))
    }
  | _ =>
    // Int.fromString isn't consistent with Float.fromString number syntax.
    switch Float.fromString(intStr) {
    | Some(num) => Tkn_Int(loc, Int.fromFloat(num))
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
  | "true" => Tkn_True(loc)
  | "false" => Tkn_False(loc)
  | "null" => Tkn_Null(loc)
  | s => Tkn_Identifier(loc, s)
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
    | "{" => Queue.add(tokens, Tkn_OpenBrace(loc))
    | "}" => Queue.add(tokens, Tkn_CloseBrace(loc))
    | "#" => Queue.add(tokens, Tkn_Block(loc))
    | "/" => Queue.add(tokens, Tkn_Slash(loc))
    | ":" => Queue.add(tokens, Tkn_Colon(loc))
    | "[" => Queue.add(tokens, Tkn_OpenBracket(loc))
    | "]" => Queue.add(tokens, Tkn_CloseBracket(loc))
    | "(" => Queue.add(tokens, Tkn_OpenParen(loc))
    | ")" => Queue.add(tokens, Tkn_CloseParen(loc))
    | "<" => Queue.add(tokens, Tkn_OpenPointyBracket(loc))
    | ">" => Queue.add(tokens, Tkn_ClosePointyBracket(loc))
    | "," => Queue.add(tokens, Tkn_Comma(loc))
    | "." =>
      switch (readChar(source), readChar(source)) {
      | (".", ".") => Queue.add(tokens, Tkn_Spread(loc))
      | (".", c) | (c, _) =>
        raise(Exit(Debug.unexpectedCharacter(~loc, ~expected=".", ~character=c, ~name)))
      }
    | "=" => Queue.add(tokens, Tkn_Equals(loc))
    | "\"" => Queue.add(tokens, Tkn_String(loc, readJsonString(source, ~name)))
    | "~" => Queue.add(tokens, Tkn_Tilde(loc))
    | "?" => Queue.add(tokens, Tkn_Question(loc))
    | "&" => Queue.add(tokens, Tkn_Ampersand(loc))
    | "!" => Queue.add(tokens, Tkn_Bang(loc))
    | ("-" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") as c =>
      Queue.add(tokens, readNumber(c, source, ~loc, ~name))
    | c if isValidIdentifierStart(c) => Queue.add(tokens, readIdentifier(c, source, loc))
    | c if isValidComponentStart(c) =>
      Queue.add(tokens, Tkn_ComponentName(loc, readSubstring(c, source, ~until=endOfIdentifier)))
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
      Queue.add(tokens, Tkn_EndOfFile(loc(source)))
      {tokens: tokens, name: name}
    | CommentMode =>
      let loc = loc(source)
      Queue.add(tokens, Tkn_Comment(loc, readComment(source, ~name)))
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
        Queue.add(tokens, Tkn_Tilde(loc(source)))
        skipChar(source)
      }
      Queue.add(tokens, Tkn_Echo(echoLoc))
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
