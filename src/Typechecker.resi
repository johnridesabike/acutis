/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Pattern: {
  type constant =
    | TBool(bool)
    | TString(string)
    | TInt(int)
    | TFloat(float)

  type construct = TList | TNullable

  type rec t =
    | TConst(Debug.Loc.t, constant)
    | TConstruct(Debug.Loc.t, construct, option<t>)
    | TTuple(Debug.Loc.t, array<t>)
    | TRecord(Debug.Loc.t, array<(string, t)>)
    | TDict(Debug.Loc.t, array<(string, t)>)
    | TVar(Debug.Loc.t, string) // any binding
    | TOptionalVar(Debug.Loc.t, string) // any binding, may not be set
    | TAny(Debug.Loc.t) // ignored wildcard _

  let toString: t => string
  let toLocation: t => Debug.Loc.t
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho({loc: Debug.Loc.t, nullables: array<Parser.echo>, default: Parser.echo})
  | TMatch(Debug.Loc.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.Loc.t, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.Loc.t, Pattern.t, NonEmpty.t<case>)
  | TComponent({
      loc: Debug.Loc.t,
      props: array<(string, Pattern.t)>,
      children: array<(string, child)>,
      val: string,
    })

and nodes = array<node>

and case = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes}

and child = TChildName(string) | TChildBlock(nodes)

type t = {
  nodes: nodes,
  prop_types: Typescheme.props,
  child_types: Typescheme.Child.props,
}

type mode = Expand | Narrow

let unify: (Typescheme.t, Typescheme.t, mode, ~loc: Debug.Loc.t, ~name: string) => unit

type root = [#Root | #Component]

module Context: {
  type t
  let make: root => t
}

let makeComponents: Utils.Dagmap.map<Source.t<Parser.t, Source.fnU<'a>>> => Utils.Dagmap.map<
  Source.t<t, Source.fnU<'a>>,
>

let make: (string, Parser.t, Utils.Dagmap.map<Source.t<t, 'a>>) => t
