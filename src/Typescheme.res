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

module Boolean = {
  type t =
    | Only_false
    | Only_true
    | False_or_true

  let subset = (a, b) =>
    switch (a, b) {
    | (Only_false, Only_false)
    | (Only_true, Only_true)
    | (_, False_or_true) => true
    | _ => false
    }

  let union = (a, b) =>
    switch (a, b) {
    | (Only_true, Only_true) => Only_true
    | (Only_false, Only_false) => Only_false
    | _ => False_or_true
    }

  let toString = x =>
    switch x {
    | Only_false => "false"
    | Only_true => "true"
    | False_or_true => "false | true"
    }
}

module Enum = {
  type row = Closed | Open

  type ty =
    | Enum_String(Belt.Set.String.t)
    | Enum_Int(Belt.Set.Int.t)

  type t = {
    mutable cases: ty,
    mutable row: row,
  }

  let row_toString = x =>
    switch x {
    | Closed => ""
    | Open => " ..."
    }

  let make = (x, row) =>
    switch x {
    | #String(s) => {cases: Enum_String(SetString.add(SetString.empty, s)), row: row}
    | #Int(s) => {cases: Enum_Int(SetInt.add(SetInt.empty, s)), row: row}
    }
}

type rec typescheme =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Enum(Enum.t)
  | Boolean(ref<Boolean.t>)
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
  | Enum({cases, row}) =>
    let s = switch cases {
    | Enum_String(cases) => SetString.toArray(cases)->Array.joinWithU(" | ", (. s) => `@"${s}"`)
    | Enum_Int(cases) =>
      SetInt.toArray(cases)->Array.joinWithU(" | ", (. i) => "@" ++ Int.toString(i))
    }
    "[" ++ s ++ Enum.row_toString(row) ++ "]"
  | Boolean({contents}) => Boolean.toString(contents)
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
  | Enum({cases, row}) => Enum({cases: cases, row: row})
  | Boolean({contents}) => Boolean({contents: contents})
  | Nullable({contents}) => Nullable(ref(copy(contents)))
  | List({contents}) => List(ref(copy(contents)))
  | Dict({contents}, fixme) => Dict(ref(copy(contents)), fixme)
  | Tuple(a) => Tuple(Array.mapU(a, (. {contents}) => ref(copy(contents))))
  | Record({contents}) => Record(ref(copy_record(contents)))
  }

and copy_record = m => MapString.mapU(m, (. {contents}) => ref(copy(contents)))

let unknown = () => ref(Unknown)
let boolean = () => ref(Boolean(ref(Boolean.False_or_true)))
let true_ = () => ref(Boolean(ref(Boolean.Only_true)))
let false_ = () => ref(Boolean(ref(Boolean.Only_false)))
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
