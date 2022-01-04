/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Token: {
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
    | Tkn_Echo(Debug.t)
    | Tkn_EndOfFile(Debug.t)

  let toString: t => string

  let debug: t => Debug.t
}

type t

let make: (~name: string, string) => t

let peek: t => Token.t

let pop: t => Token.t

let debugToArray: t => array<Token.t>
