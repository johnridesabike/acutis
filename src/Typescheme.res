/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module Int = Belt.Int
module MapInt = Belt.Map.Int
module MapString = Belt.Map.String
module SetInt = Belt.Set.Int
module SetString = Belt.Set.String

module Variant = {
  type row = Closed | Open

  type extra = Extra_none | Extra_boolean

  type ty<'a, 'b> = Int('a) | String('b)

  type t<'a, 'b> = {
    mutable cases: ty<'a, 'b>,
    mutable row: row,
    extra: extra,
  }

  let row_toString = x =>
    switch x {
    | Closed => ""
    | Open => " | ..."
    }
}

module Enum = {
  type t = Variant.t<SetInt.t, SetString.t>

  let string = (s, row) => {
    Variant.cases: String(SetString.add(SetString.empty, s)),
    row: row,
    extra: Extra_none,
  }
  let int = (i, row) => {
    Variant.cases: Int(SetInt.add(SetInt.empty, i)),
    row: row,
    extra: Extra_none,
  }

  let false_and_true_cases = Variant.Int(SetInt.fromArray([0, 1]))
  let false_only = Variant.Int(SetInt.add(SetInt.empty, 0))
  let true_only = Variant.Int(SetInt.add(SetInt.empty, 1))

  let false_and_true = () => {
    Variant.cases: false_and_true_cases,
    row: Closed,
    extra: Extra_boolean,
  }
  let true_only = () => {Variant.cases: true_only, row: Closed, extra: Extra_boolean}
  let false_only = () => {Variant.cases: false_only, row: Closed, extra: Extra_boolean}
}

module Union = {
  type t<'a> = Variant.t<MapInt.t<ref<MapString.t<'a>>>, MapString.t<ref<MapString.t<'a>>>>
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
  | Union(string, Union.t<t>)
  | Dict(t, ref<SetString.t>)

and t = ref<typescheme>

type props = MapString.t<t>

let bool_toString = i =>
  switch i {
  | 0 => "false"
  | _ => "true"
  }

let rec toString = x =>
  switch x.contents {
  | Unknown => "_"
  | Int => "int"
  | Float => "float"
  | String => "string"
  | Echo => "echoable"
  | Enum({cases, row, extra}) =>
    let s = switch cases {
    | String(cases) => SetString.toArray(cases)->Array.joinWithU(" | ", (. s) => `@"${s}"`)
    | Int(cases) =>
      switch extra {
      | Extra_none => SetInt.toArray(cases)->Array.joinWithU(" | ", (. i) => "@" ++ Int.toString(i))
      | Extra_boolean => SetInt.toArray(cases)->Array.joinWith(" | ", bool_toString)
      }
    }
    s ++ Variant.row_toString(row)
  | Nullable(x) => "?" ++ toString(x)
  | List(x) => `[${toString(x)}]`
  | Dict(x, _) => `<${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x, ", ", toString)
    `(${x})`
  | Record(x) =>
    let rows = x.contents->MapString.toArray->record_rows_toString
    "{" ++ rows ++ "}"
  | Union(key, {cases, extra, row}) =>
    let cases = switch cases {
    | String(m) => MapString.toArray(m)->Array.mapU((. (tag, m)) => (`"${tag}"`, m))
    | Int(m) =>
      switch extra {
      | Extra_none => MapInt.toArray(m)->Array.mapU((. (tag, m)) => (Int.toString(tag), m))
      | Extra_boolean => MapInt.toArray(m)->Array.mapU((. (tag, m)) => (bool_toString(tag), m))
      }
    }
    Array.joinWithU(cases, " | ", (. (tag, m)) => {
      let rows = record_rows_toString(MapString.toArray(m.contents))
      let rows = switch rows {
      | "" => ""
      | rows => ", " ++ rows
      }
      `{@"${key}": ${tag}${rows}}`
    }) ++
    Variant.row_toString(row)
  }

and record_rows_toString = a => Array.joinWithU(a, ", ", (. (k, v)) => `"${k}": ${toString(v)}`)

let rec copy = x =>
  switch x {
  | (Unknown | Int | Float | String | Echo) as x => x
  | Enum({cases, row, extra}) => Enum({cases: cases, row: row, extra: extra})
  | Nullable({contents}) => Nullable(ref(copy(contents)))
  | List({contents}) => List(ref(copy(contents)))
  | Dict({contents}, fixme) => Dict(ref(copy(contents)), fixme)
  | Tuple(a) => Tuple(Array.mapU(a, (. {contents}) => ref(copy(contents))))
  | Record({contents}) => Record(ref(copy_record(contents)))
  | Union(tag, {cases, row, extra}) =>
    let cases = switch cases {
    | String(m) => Variant.String(MapString.mapU(m, (. {contents}) => ref(copy_record(contents))))
    | Int(m) => Int(MapInt.mapU(m, (. {contents}) => ref(copy_record(contents))))
    }
    Union(tag, {cases: cases, row: row, extra: extra})
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
