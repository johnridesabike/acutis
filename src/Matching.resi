/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/* This is extra information for debugging. */
type extra_nest_info = Tuple | Dict | Record

type extra_switch_info =
  | Extra_none
  | Extra_enum_closed

type rec tree<'leaf, 'key> =
  | Switch({
      key: 'key,
      ids: Belt.Set.Int.t,
      cases: switchcase<'leaf, 'key>,
      extra: extra_switch_info,
      wildcard: option<tree<'leaf, 'key>>,
    })
  | Nest({
      key: 'key,
      ids: Belt.Set.Int.t,
      child: nest<'leaf, 'key>,
      wildcard: option<tree<'leaf, 'key>>,
      extra: extra_nest_info,
    })
  | Construct({
      key: 'key,
      ids: Belt.Set.Int.t,
      nil: option<tree<'leaf, 'key>>,
      cons: option<tree<'leaf, 'key>>,
      extra: Typechecker.Pattern.construct,
    })
  | Wildcard({key: 'key, ids: Belt.Set.Int.t, child: tree<'leaf, 'key>})
  | End('leaf)

and nest<'leaf, 'key> =
  | IntKeys(tree<tree<'leaf, 'key>, int>)
  | StringKeys(tree<tree<'leaf, 'key>, string>)

and switchcase<'leaf, 'key> = {
  val: Data.Const.t,
  ifMatch: tree<'leaf, 'key>,
  nextCase: option<switchcase<'leaf, 'key>>,
}

module Exit: {
  type key
  type t<'a>
  let get: (t<'a>, key) => 'a
  let map: (t<'a>, ~f: (. 'a) => 'b) => t<'b>
  let unsafe_key: int => key // for debugging
}

type leaf = {names: Belt.Map.String.t<int>, exit: Exit.key}

type t<'a> = {
  tree: tree<leaf, int>,
  exits: Exit.t<'a>,
}

let make: NonEmpty.t<Typechecker.case> => t<Typechecker.nodes>

let partial_match_check: (tree<'a, int>, Debug.t) => unit
