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
      kind: Typechecker.Pattern.construct,
      // nil is like a wildcard. It always points to the *next* node.
      nil: option<tree<'a>>,
      // cons is always either a Wildcard node or a Nest-Tuple node.
      cons: option<tree<'a>>,
    })
  | Wildcard({idx: int, key: string, ids: Belt.Set.Int.t, child: tree<'a>})
  | End('a)

and switchcase<'a> = {
  val: Typechecker.Pattern.constant,
  ifMatch: tree<'a>,
  nextCase: option<switchcase<'a>>,
}

type leaf = {names: Belt.Map.String.t<int>, exit: int}

type t<'a> = {
  loc: Debug.loc,
  tree: tree<leaf>,
  exits: array<'a>,
}

let make: (
  ~loc: Debug.loc,
  ~name: string,
  NonEmpty.t<Typechecker.case>,
) => result<t<Typechecker.nodes>, Debug.t>

module ParMatch: {
  let check: (tree<'a>, ~loc: Debug.loc) => result<tree<'a>, Debug.t>
}
