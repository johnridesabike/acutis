/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Pattern: {
  type rec t =
    | UNull(Debug.loc)
    | USome(Debug.loc, t)
    | UFalse(Debug.loc)
    | UTrue(Debug.loc)
    | UString(Debug.loc, string)
    | UInt(Debug.loc, int)
    | UFloat(Debug.loc, float)
    | UTuple(Debug.loc, array<t>)
    | UList(Debug.loc, array<t>)
    | UListWithTailBinding(Debug.loc, array<t>, t)
    | UDict(Debug.loc, array<(string, t)>)
    | URecord(Debug.loc, array<(string, t)>)
    | UBinding(Debug.loc, string)

  let toLocation: t => Debug.loc

  let make: Lexer.t => NonEmpty.t<t>
}

type echo =
  | EBinding(Debug.loc, string, Utils.escape)
  | EChild(Debug.loc, string)
  | EString(Debug.loc, string, Utils.escape)
  | EInt(Debug.loc, int, Utils.escape)
  | EFloat(Debug.loc, float, Utils.escape)

type trim = TrimStart | TrimEnd | TrimBoth | NoTrim

type rec node =
  | UText(string, trim)
  // The first echo item that isn't null will be returned.
  | UEcho({loc: Debug.loc, nullables: array<echo>, default: echo})
  | UMatch(Debug.loc, NonEmpty.t<(Debug.loc, string)>, NonEmpty.t<case>)
  | UMapList(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | UMapDict(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | UComponent({
      loc: Debug.loc,
      name: string,
      props: array<(string, Pattern.t)>,
      children: array<(string, child)>,
    })

and case = {
  patterns: NonEmpty.t<NonEmpty.t<Pattern.t>>,
  nodes: t,
}

and child = UChildName(Debug.loc, string) | UChildBlock(Debug.loc, t)

and t = array<node>

let makeExn: (~name: string, string) => t
