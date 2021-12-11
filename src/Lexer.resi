/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Token: {
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

  let toString: t => string

  let toLocation: t => Debug.loc
}

type t

let make: (~name: string, string) => t

let peekExn: t => Token.t

let popExn: t => Token.t

let name: t => string

let debugToArray: t => array<Token.t>
