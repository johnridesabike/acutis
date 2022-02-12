/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Enum: {
  type row = Closed | Open

  type extra = Extra_none | Extra_boolean

  type ty =
    | Enum_int(Belt.Set.Int.t)
    | Enum_string(Belt.Set.String.t)

  type t = {
    mutable cases: ty,
    mutable row: row,
    extra: extra,
  }

  let make: ([#String(string) | #Int(int)], row) => t

  let false_and_true_cases: ty
  let false_and_true: unit => t
  let false_only: unit => t
  let true_only: unit => t
}

type rec typescheme =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Enum(Enum.t)
  | Tuple(array<t>) // 0 and 1 sized tuples are legal.
  | Nullable(t)
  | List(t)
  | Record(ref<Belt.Map.String.t<t>>)
  /* The string set tracks referenced keys for matching compilation. */
  | Dict(t, ref<Belt.Set.String.t>)

and t = ref<typescheme>

type props = Belt.Map.String.t<t>

let toString: t => string

let copy_record: props => props

let unknown: unit => t
let boolean: unit => t
let int: unit => t
let float: unit => t
let string: unit => t
let echo: unit => t
let nullable: t => t
let list: t => t
let dict_keys: (t, ref<Belt.Set.String.t>) => t // internal use
let dict: t => t
let tuple: array<t> => t
let record: array<(string, t)> => t
let record2: ref<props> => t // internal use
let props: array<(string, t)> => props

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
