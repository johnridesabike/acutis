/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Pattern: {
  type constant =
    | TString(string)
    | TInt(int)
    | TFloat(float)

  type construct = TList | TNullable

  type rec t =
    | TConst(Debug.t, constant, option<Typescheme.Enum.t>)
    | TConstruct(Debug.t, construct, option<t>)
    | TTuple(Debug.t, array<t>)
    | TRecord(
        Debug.t,
        option<(string, constant, Typescheme.Union.t<Typescheme.ty>)>,
        Belt.Map.String.t<t>,
        ref<Belt.Map.String.t<Typescheme.ty>>,
      )
    | TDict(Debug.t, Belt.Map.String.t<t>, ref<Belt.Set.String.t>)
    | TVar(Debug.t, string) // any binding
    | TOptionalVar(Debug.t, string) // any binding, may not be set
    | TAny(Debug.t) // ignored wildcard _

  let toString: t => string
  let debug: t => Debug.t
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho(Debug.t, array<Parser.echo>, Parser.echo)
  | TMatch(Debug.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TComponent(Debug.t, string, Belt.Map.String.t<Pattern.t>, Belt.Map.String.t<child>)

and nodes = array<node>

and case = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes}

and child = TChildName(string) | TChildBlock(nodes)

type t = {
  nodes: nodes,
  prop_types: Typescheme.t,
  child_types: Typescheme.Child.t,
}

let makeComponents: Utils.Dagmap.map<Source.t<Parser.t, Source.fnU<'a>>> => Utils.Dagmap.map<
  Source.t<t, Source.fnU<'a>>,
>

let make: (Parser.t, Utils.Dagmap.map<Source.t<t, Source.fnU<'a>>>) => t
