/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Variant: {
  type row = Closed | Open

  type extra = Extra_none | Extra_boolean

  type ty<'a, 'b> = Int('a) | String('b)

  type t<'a, 'b> = {
    mutable cases: ty<'a, 'b>,
    mutable row: row,
    extra: extra,
  }
}

module Enum: {
  type t = Variant.t<Belt.Set.Int.t, Belt.Set.String.t>
  let string: (array<string>, Variant.row) => t
  let string_singleton: (string, Variant.row) => t
  let int: (array<int>, Variant.row) => t
  let int_singleton: (int, Variant.row) => t
  let false_and_true_cases: Variant.ty<Belt.Set.Int.t, _>
  let false_and_true: unit => t
  let false_only: unit => t
  let true_only: unit => t
}

module Union: {
  type t<'a> = Variant.t<
    Belt.Map.Int.t<ref<Belt.Map.String.t<'a>>>,
    Belt.Map.String.t<ref<Belt.Map.String.t<'a>>>,
  >
}

type rec typescheme =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Nullable(t)
  | List(t)
  | Tuple(array<t>) // 0 and 1 sized tuples are legal.
  | Record(ref<Belt.Map.String.t<t>>)
  /* The string set tracks referenced keys for matching compilation. */
  | Dict(t, ref<Belt.Set.String.t>)
  | Enum(Enum.t)
  | Union(string, Union.t<t>)

and t = ref<typescheme>

type props = Belt.Map.String.t<t>

let internal_dict_keys: (t, ref<Belt.Set.String.t>) => t
let internal_record: ref<props> => t

/* Public API for declaring type schemes: */
let unknown: unit => t
let int: unit => t
let int_: unit => t // int is a JS keyword
let float: unit => t
let float_: unit => t // float is a JS keyword
let string: unit => t
let echo: unit => t
let nullable: t => t
let list: t => t
let tuple: array<t> => t
let record: array<(string, t)> => t
let dict: t => t
let enum_int: array<int> => t
let enum_string: array<string> => t
let bool: unit => t
let union_int: (string, array<(int, array<(string, t)>)>) => t
let union_string: (string, array<(string, array<(string, t)>)>) => t
let union_boolean: (string, ~f: array<(string, t)>, ~t: array<(string, t)>) => t
let props: array<(string, t)> => props

let toString: t => string

let copy_record: props => props

module Child: {
  type t
  type props = Belt.Map.String.t<t>
  let props: array<(string, t)> => props
  let child: string => (string, t)
  let nullable: string => (string, t)
  let equal: (t, t) => bool
  let is_nullable: t => bool
  let toString: t => string
}
