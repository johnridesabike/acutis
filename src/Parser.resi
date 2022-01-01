/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/* This creates the untyped AST. */

module Pattern: {
  type rec t =
    | UNull(Debug.Loc.t)
    | USome(Debug.Loc.t, t)
    | UFalse(Debug.Loc.t)
    | UTrue(Debug.Loc.t)
    | UString(Debug.Loc.t, string)
    | UInt(Debug.Loc.t, int)
    | UFloat(Debug.Loc.t, float)
    | UTuple(Debug.Loc.t, array<t>)
    | UList(Debug.Loc.t, array<t>)
    | UListWithTailBinding(Debug.Loc.t, array<t>, t)
    | UDict(Debug.Loc.t, array<(string, t)>)
    | URecord(Debug.Loc.t, array<(string, t)>)
    | UBinding(Debug.Loc.t, string)

  let toLocation: t => Debug.Loc.t

  let make: Lexer.t => NonEmpty.t<t>
}

type echo =
  | EBinding(Debug.Loc.t, string, Utils.escape)
  | EChild(Debug.Loc.t, string)
  | EString(Debug.Loc.t, string, Utils.escape)
  | EInt(Debug.Loc.t, int, Utils.escape)
  | EFloat(Debug.Loc.t, float, Utils.escape)

type trim = NoTrim | TrimStart | TrimEnd | TrimBoth

type rec node =
  | UText(string, trim)
  // The first echo item that isn't null will be returned.
  | UEcho({loc: Debug.Loc.t, nullables: array<echo>, default: echo})
  | UMatch(Debug.Loc.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | UMapList(Debug.Loc.t, Pattern.t, NonEmpty.t<case>)
  | UMapDict(Debug.Loc.t, Pattern.t, NonEmpty.t<case>)
  | UComponent({
      loc: Debug.Loc.t,
      name: string,
      props: array<(string, Pattern.t)>,
      children: array<(string, child)>,
    })

and case = {
  patterns: NonEmpty.t<NonEmpty.t<Pattern.t>>,
  nodes: t,
}

and child = UChildName(Debug.Loc.t, string) | UChildBlock(Debug.Loc.t, t)

and t = array<node>

let makeExn: (~name: string, string) => t
