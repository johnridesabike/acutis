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
    | TPat_Const(Debug.loc, constant)
    | TPat_Construct(Debug.loc, construct, option<t>)
    | TPat_Tuple(Debug.loc, array<t>)
    | TPat_Record(Debug.loc, array<(string, t)>)
    | TPat_Dict(Debug.loc, array<(string, t)>)
    | TPat_Var(Debug.loc, string) // any binding
    | TPat_OptionalVar(Debug.loc, string) // any binding, may not be set
    | TPat_Any(Debug.loc) // ignored wildcard _

  let compareConst: (constant, constant) => int
  let eqConst: (constant, constant) => bool
  let toString: t => string
}

module Ast: {
  type rec node =
    | TText(string, Compile.Ast.trim)
    // The first echo item that isn't null will be returned.
    | TEcho({loc: Debug.loc, nullables: array<Compile.Ast.Echo.t>, default: Compile.Ast.Echo.t})
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
}

// If a type is incomplete, then it can be unified more liberally. (Unused).
// type complete = Complete | Incomplete

type mode = Expand | Narrow

let unify: (Typescheme.t, Typescheme.t, mode, ~loc: Debug.loc, ~name: string) => unit

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
  ~loc: Debug.loc,
  ~name: string,
  Utils.Dagmap.t<'a, Source2.t<Ast.t, 'b>>,
) => (NonEmpty.t<Typescheme.t>, NonEmpty.t<Ast.case>)

let makeArray: array<(string, Source2.t<Acutis_Types.Ast.nodes<unit>, 'a>)> => array<(
  string,
  Source2.t<Ast.t, 'a>,
)>

let make: (string, Acutis_Types.Ast.nodes<unit>, array<(string, Source2.t<Ast.t, 'a>)>) => Ast.t
