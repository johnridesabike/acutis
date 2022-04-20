/**
  Copyright (c) 2022 John Jackson.

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

type rec ty' =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Nullable(ty)
  | List(ty)
  | Tuple(array<ty>)
  | Record(ref<Belt.Map.String.t<ty>>)
  | Dict(ty, ref<Belt.Set.String.t>)
  | Enum(Enum.t)
  | Union(string, Union.t<ty>)

and ty = ref<ty'>

type t = Belt.Map.String.t<ty>

let internal_dict_keys: (ty, ref<Belt.Set.String.t>) => ty
let internal_record: ref<t> => ty
let internal_copy_record: t => t

/* Public API for declaring type schemes: */
let unknown: unit => ty
let int: unit => ty
let int_: unit => ty // int is a JS keyword
let float: unit => ty
let float_: unit => ty // float is a JS keyword
let string: unit => ty
let echo: unit => ty
let nullable: ty => ty
let list: ty => ty
let tuple: array<ty> => ty
let record: array<(string, ty)> => ty
let dict: ty => ty
let enum_int: array<int> => ty
let enum_string: array<string> => ty
let bool: unit => ty
let union_int: (string, array<(int, array<(string, ty)>)>) => ty
let union_string: (string, array<(string, array<(string, ty)>)>) => ty
let union_boolean: (string, ~f: array<(string, ty)>, ~t: array<(string, ty)>) => ty
let make: array<(string, ty)> => t

let toString: ty => string
let toString2: t => string

module Child: {
  type ty
  type t = Belt.Map.String.t<ty>
  let make: array<(string, ty)> => t
  let child: string => (string, ty)
  let nullable: string => (string, ty)
  let equal: (ty, ty) => bool
  let is_nullable: ty => bool
  let toString: ty => string
}
