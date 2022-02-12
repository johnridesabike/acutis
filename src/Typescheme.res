/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module Int = Belt.Int
module MapString = Belt.Map.String
module SetString = Belt.Set.String
module SetInt = Belt.Set.Int

module Enum = {
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

  let row_toString = x =>
    switch x {
    | Closed => ""
    | Open => " | ..."
    }

  let make = (x, row) =>
    switch x {
    | #String(s) => {
        cases: Enum_string(SetString.add(SetString.empty, s)),
        row: row,
        extra: Extra_none,
      }
    | #Int(s) => {cases: Enum_int(SetInt.add(SetInt.empty, s)), row: row, extra: Extra_none}
    }

  let false_and_true_cases = Enum_int(SetInt.fromArray([0, 1]))
  let false_only = Enum_int(SetInt.add(SetInt.empty, 0))
  let true_only = Enum_int(SetInt.add(SetInt.empty, 1))

  let false_and_true = () => {cases: false_and_true_cases, row: Closed, extra: Extra_boolean}
  let true_only = () => {cases: true_only, row: Closed, extra: Extra_boolean}
  let false_only = () => {cases: false_only, row: Closed, extra: Extra_boolean}
}

type rec typescheme =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Enum(Enum.t)
  | Tuple(array<t>)
  | Nullable(t)
  | List(t)
  | Record(ref<MapString.t<t>>)
  | Dict(t, ref<SetString.t>)

and t = ref<typescheme>

type props = MapString.t<t>

let rec toString = x =>
  switch x.contents {
  | Unknown => "_"
  | Int => "int"
  | Float => "float"
  | String => "string"
  | Echo => "echoable"
  | Enum({cases, row, extra}) =>
    let s = switch cases {
    | Enum_string(cases) => SetString.toArray(cases)->Array.joinWithU(" | ", (. s) => `@"${s}"`)
    | Enum_int(cases) =>
      switch extra {
      | Extra_none => SetInt.toArray(cases)->Array.joinWithU(" | ", (. i) => "@" ++ Int.toString(i))
      | Extra_boolean =>
        SetInt.toArray(cases)->Array.joinWithU(" | ", (. i) =>
          switch i {
          | 0 => "false"
          | _ => "true"
          }
        )
      }
    }
    s ++ Enum.row_toString(row)
  | Nullable(x) => "?" ++ toString(x)
  | List(x) => `[${toString(x)}]`
  | Dict(x, _) => `<${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x, ", ", toString)
    `(${x})`
  | Record(x) =>
    let rows = switch MapString.toArray(x.contents) {
    | [] => "_"
    | a => Array.joinWithU(a, ", ", (. (k, v)) => `"${k}": ${toString(v)}`)
    }
    "{" ++ rows ++ "}"
  }

let rec copy = x =>
  switch x {
  | (Unknown | Int | Float | String | Echo) as x => x
  | Enum({cases, row, extra}) => Enum({cases: cases, row: row, extra: extra})
  | Nullable({contents}) => Nullable(ref(copy(contents)))
  | List({contents}) => List(ref(copy(contents)))
  | Dict({contents}, fixme) => Dict(ref(copy(contents)), fixme)
  | Tuple(a) => Tuple(Array.mapU(a, (. {contents}) => ref(copy(contents))))
  | Record({contents}) => Record(ref(copy_record(contents)))
  }

and copy_record = m => MapString.mapU(m, (. {contents}) => ref(copy(contents)))

let unknown = () => ref(Unknown)
let boolean = () => ref(Enum(Enum.false_and_true()))
let int = () => ref(Int)
let float = () => ref(Float)
let string = () => ref(String)
let echo = () => ref(Echo)
let nullable = t => ref(Nullable(t))
let list = t => ref(List(t))
let dict_keys = (t, kys) => ref(Dict(t, kys))
let dict = t => ref(Dict(t, ref(SetString.empty)))
let tuple = a => ref(Tuple(a))
let record = a => ref(Record(ref(MapString.fromArray(a))))
let record2 = m => ref(Record(m))
let props = a => MapString.fromArray(a)

module Child = {
  type t' = Child | NullableChild
  type t = ref<t'>
  type props = MapString.t<t>
  let props = a => MapString.fromArray(a)
  let child = x => (x, ref(Child))
  let nullable = x => (x, ref(NullableChild))
  let equal = (a: t, b: t) => a.contents == b.contents
  let is_nullable = t => t.contents == NullableChild
  let toString = x =>
    switch x.contents {
    | Child => "Child"
    | NullableChild => "NullableChild"
    }
}
