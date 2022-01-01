/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Token: {
  type t =
    // Static elements
    | Tkn_Text(Debug.Loc.t, string)
    | Tkn_Comment(Debug.Loc.t, string)
    // JSON values
    | Tkn_String(Debug.Loc.t, string)
    | Tkn_Int(Debug.Loc.t, int)
    | Tkn_Float(Debug.Loc.t, float)
    | Tkn_True(Debug.Loc.t) // a reserved identifier
    | Tkn_False(Debug.Loc.t) // a reserved identifier
    | Tkn_Null(Debug.Loc.t) // a reserved identifier
    // JSON syntax
    | Tkn_Comma(Debug.Loc.t)
    | Tkn_Colon(Debug.Loc.t)
    | Tkn_OpenBracket(Debug.Loc.t)
    | Tkn_CloseBracket(Debug.Loc.t)
    | Tkn_OpenBrace(Debug.Loc.t)
    | Tkn_CloseBrace(Debug.Loc.t)
    | Tkn_OpenParen(Debug.Loc.t)
    | Tkn_CloseParen(Debug.Loc.t)
    | Tkn_OpenPointyBracket(Debug.Loc.t)
    | Tkn_ClosePointyBracket(Debug.Loc.t)
    | Tkn_Spread(Debug.Loc.t)
    // Component syntax
    | Tkn_ComponentName(Debug.Loc.t, string)
    | Tkn_Slash(Debug.Loc.t)
    | Tkn_Block(Debug.Loc.t)
    | Tkn_Equals(Debug.Loc.t)
    // Dynamic content
    | Tkn_Identifier(Debug.Loc.t, string)
    | Tkn_Tilde(Debug.Loc.t)
    | Tkn_Question(Debug.Loc.t)
    | Tkn_Ampersand(Debug.Loc.t)
    | Tkn_Bang(Debug.Loc.t)
    | Tkn_Echo(Debug.Loc.t)
    | Tkn_EndOfFile(Debug.Loc.t)

  let toString: t => string

  let toLocation: t => Debug.Loc.t
}

type t

let make: (~name: string, string) => t

let peekExn: t => Token.t

let popExn: t => Token.t

let name: t => string

let debugToArray: t => array<Token.t>
