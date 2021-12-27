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
    | TConst(Debug.loc, constant)
    | TConstruct(Debug.loc, construct, option<t>)
    | TTuple(Debug.loc, array<t>)
    | TRecord(Debug.loc, array<(string, t)>)
    | TDict(Debug.loc, array<(string, t)>)
    | TVar(Debug.loc, string) // any binding
    | TOptionalVar(Debug.loc, string) // any binding, may not be set
    | TAny(Debug.loc) // ignored wildcard _

  let toString: t => string
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho({loc: Debug.loc, nullables: array<Parser.echo>, default: Parser.echo})
  | TMatch(Debug.loc, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | TComponent({
      loc: Debug.loc,
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

let unify: (Typescheme.t, Typescheme.t, mode, ~loc: Debug.loc, ~name: string) => unit

type root = [#Root | #Component]

module Context: {
  type t
  let make: root => t
}

module Local: {
  let fromPattern: (
    ~default: Typescheme.t,
    Parser.Pattern.t,
    Belt.MutableQueue.t<(string, Typescheme.t)>,
    ~name: string,
  ) => Typescheme.t
}

let makeCases: (
  NonEmpty.t<Parser.case>,
  Context.t,
  ~loc: Debug.loc,
  ~name: string,
  Utils.Dagmap.t<'a, Source.t<t, 'b>>,
) => (NonEmpty.t<Typescheme.t>, NonEmpty.t<case>)

let makeComponents: Utils.Dagmap.map<Source.t<Parser.t, Source.fnU<'a>>> => Utils.Dagmap.map<
  Source.t<t, Source.fnU<'a>>,
>

let make: (string, Parser.t, Utils.Dagmap.map<Source.t<t, 'a>>) => t
