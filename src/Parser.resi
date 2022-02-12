/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/* This creates the untyped AST. */

module Pattern: {
  type rec t =
    | UNullable(Debug.t, option<t>)
    | UBool(Debug.t, int)
    | UString(Debug.t, string)
    | UInt(Debug.t, int)
    | UFloat(Debug.t, float)
    | UTuple(Debug.t, array<t>)
    | UStringEnum(Debug.t, string)
    | UIntEnum(Debug.t, int)
    | UList(Debug.t, array<t>, option<t>)
    | UDict(Debug.t, Belt.Map.String.t<t>)
    | URecord(Debug.t, Belt.Map.String.t<t>)
    | UBinding(Debug.t, string)

  let debug: t => Debug.t

  let make: Lexer.t => NonEmpty.t<t>
}

type echo =
  | EBinding(Debug.t, string, Utils.escape)
  | EChild(Debug.t, string)
  | EString(Debug.t, string, Utils.escape)
  | EInt(Debug.t, int, Utils.escape)
  | EFloat(Debug.t, float, Utils.escape)

type trim = NoTrim | TrimStart | TrimEnd | TrimBoth

type rec node =
  | UText(string, trim)
  | UEcho(Debug.t, array<echo>, echo)
  | UMatch(Debug.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | UMapList(Debug.t, Pattern.t, NonEmpty.t<case>)
  | UMapDict(Debug.t, Pattern.t, NonEmpty.t<case>)
  | UComponent(Debug.t, string, Belt.Map.String.t<Pattern.t>, Belt.Map.String.t<child>)

and case = {
  patterns: NonEmpty.t<NonEmpty.t<Pattern.t>>,
  nodes: t,
}

and child = UChildName(Debug.t, string) | UChildBlock(Debug.t, t)

and t = array<node>

let make: Lexer.t => t
