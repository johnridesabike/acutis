/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type nest = Tuple | Record | Dict

type rec tree<'a> =
  | Switch({
      idx: int,
      key: string,
      ids: Belt.Set.Int.t,
      cases: switchcase<'a>,
      wildcard: option<tree<'a>>,
    })
  | Nest({
      idx: int,
      key: string,
      ids: Belt.Set.Int.t,
      kind: nest,
      child: tree<tree<'a>>,
      wildcard: option<tree<'a>>,
    })
  | Construct({
      idx: int,
      key: string,
      ids: Belt.Set.Int.t,
      kind: TypeChecker.TypedPattern.construct,
      // nil is like a wildcard. It always points to the *next* node.
      nil: option<tree<'a>>,
      // cons is always either a Wildcard node or a Nest-Tuple node.
      cons: option<tree<'a>>,
    })
  | Wildcard({idx: int, key: string, ids: Belt.Set.Int.t, child: tree<'a>})
  | End('a)

and switchcase<'a> = {
  val: TypeChecker.TypedPattern.constant,
  ifMatch: tree<'a>,
  nextCase: option<switchcase<'a>>,
}

type leaf = {names: Belt.Map.Int.t<string>, exit: int}

type t<'a> = {
  loc: Acutis_Types.loc,
  tree: tree<leaf>,
  exits: array<Acutis_Types.Ast.nodes<'a>>,
}

let make: (
  ~loc: Acutis_Types.loc,
  NonEmpty.t<Acutis_Types.Ast2.case<Acutis_Types.Ast_Pattern.t, 'a>>,
) => result<t<'a>, Debug.t>

module ParMatch: {
  let check: tree<'a> => result<tree<'a>, Debug.t>
}
