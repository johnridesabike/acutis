/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Token: {
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

  let toString: t => string

  let toLocation: t => Debug.loc
}

type t

let make: (~name: string, string) => t

let peekExn: t => Token.t

let popExn: t => Token.t

let name: t => string

let debugToArray: t => array<Token.t>
