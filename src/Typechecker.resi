/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Pattern: {
  type constant =
    | TPat_Bool(bool)
    | TPat_String(string)
    | TPat_Int(int)
    | TPat_Float(float)
  type construct = TPat_List | TPat_Nullable

  type rec t =
    | TPat_Const(Utils.loc, constant)
    | TPat_Construct(Utils.loc, construct, option<t>)
    | TPat_Tuple(Utils.loc, array<t>)
    | TPat_Record(Utils.loc, array<(string, t)>)
    | TPat_Dict(Utils.loc, array<(string, t)>)
    | TPat_Var(Utils.loc, string) // any binding
    | TPat_OptionalVar(Utils.loc, string) // any binding, may not be set
    | TPat_Any(Utils.loc) // ignored wildcard _

  let compareConst: (constant, constant) => int
  let eqConst: (constant, constant) => bool
  let toString: t => string
}

module Ast: {
  type rec node<'a> =
    | TText(string, Acutis_Types.Ast.trim)
    // The first echo item that isn't null will be returned.
    | TEcho({
        loc: Utils.loc,
        nullables: array<Acutis_Types.Ast.Echo.t>,
        default: Acutis_Types.Ast.Echo.t,
      })
    | TMatch(Utils.loc, NonEmpty.t<Pattern.t>, NonEmpty.t<case<'a>>)
    | TMapList(Utils.loc, Pattern.t, NonEmpty.t<case<'a>>)
    | TMapDict(Utils.loc, Pattern.t, NonEmpty.t<case<'a>>)
    | TComponent({
        loc: Utils.loc,
        name: string,
        props: array<(string, Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and case<'a> = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes<'a>}
  and child<'a> = TChildName(string) | TChildBlock(nodes<'a>)
  type t<'a> = {
    nodes: nodes<'a>,
    prop_types: Typescheme.props,
    child_types: Typescheme.Child.props,
  }
}

// If a type is incomplete, then it can be unified more liberally. (Unused).
// type complete = Complete | Incomplete

type mode = Expand | Narrow

let unify: (Typescheme.t, Typescheme.t, mode, ~loc: Utils.loc, ~name: string) => unit

module Context: {
  type t
  let make: unit => t
}

module Local: {
  let fromPattern: (
    Acutis_Types.Ast_Pattern.t,
    Belt.MutableQueue.t<(string, Typescheme.t)>,
    ~name: string,
  ) => Typescheme.t
}

let makeCases: (
  NonEmpty.t<Acutis_Types.Ast.case<unit>>,
  Context.t,
  ~loc: Utils.loc,
  ~name: string,
  Utils.Dag.t<Source2.t<Acutis_Types.Ast.nodes<unit>, 'a>, Source2.t<Ast.t<unit>, 'a>>,
) => (NonEmpty.t<Typescheme.t>, NonEmpty.t<Ast.case<unit>>)

let makeArray: array<(string, Source2.t<Acutis_Types.Ast.nodes<unit>, 'a>)> => array<(
  string,
  Source2.t<Ast.t<unit>, 'a>,
)>

let make: (
  string,
  Acutis_Types.Ast.nodes<unit>,
  array<(string, Source2.t<Ast.t<unit>, 'a>)>,
) => Ast.t<unit>

module Deprecated: {
  let check: Acutis_Types.Ast.nodes<'a> => Belt.Map.String.t<Typescheme.t>
}
