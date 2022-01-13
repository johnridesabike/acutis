/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module SetString = Belt.Set.String
module MapString = Belt.Map.String
module Array = Belt.Array

type rec typescheme =
  | Unknown
  | Boolean
  | Int
  | Float
  | String
  | Echo
  | Nullable(t)
  | List(t)
  | Dict(t, ref<SetString.t>)
  | Tuple(ref<array<t>>)
  | Record(ref<MapString.t<t>>)

and t = ref<typescheme>

type props = MapString.t<t>

let rec toString = x =>
  switch x.contents {
  | Unknown => "_"
  | Boolean => "boolean"
  | Int => "int"
  | Float => "float"
  | String => "string"
  | Echo => "echoable"
  | Nullable(x) => `nullable(${toString(x)})`
  | List(x) => `[${toString(x)}]`
  | Dict(x, _) => `<${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x.contents, ", ", toString)
    `(${x})`
  | Record(x) => record_toString(x)
  }

and record_toString = x => {
  let rows = switch MapString.toArray(x.contents) {
  | [] => "_"
  | a => Array.joinWithU(a, ", ", (. (k, v)) => `"${k}": ${toString(v)}`)
  }
  "{" ++ rows ++ "}"
}

let rec copy = x =>
  switch x {
  | (Unknown | Boolean | Int | Float | String | Echo) as x => x
  | Nullable({contents}) => Nullable(ref(copy(contents)))
  | List({contents}) => List(ref(copy(contents)))
  | Dict({contents}, fixme) => Dict(ref(copy(contents)), fixme)
  | Tuple({contents}) => Tuple(ref(Array.mapU(contents, (. {contents}) => ref(copy(contents)))))
  | Record({contents}) => Record(ref(copy_record(contents)))
  }

and copy_record = m => MapString.mapU(m, (. {contents}) => ref(copy(contents)))

let unknown = () => ref(Unknown)
let boolean = () => ref(Boolean)
let int = () => ref(Int)
let float = () => ref(Float)
let string = () => ref(String)
let echo = () => ref(Echo)
let nullable = t => ref(Nullable(t))
let list = t => ref(List(t))
let dict = t => ref(Dict(t, ref(SetString.empty)))
let tuple = a => ref(Tuple(ref(a)))
let record = a => ref(Record(ref(MapString.fromArray(a))))
let record2 = m => ref(Record(ref(m)))
let props = a => MapString.fromArray(a)

type rec debug = [
  | #Polymorphic
  | #Boolean
  | #Int
  | #Float
  | #String
  | #Echo
  | #Nullable(debug)
  | #List(debug)
  | #Tuple(array<debug>)
  | #Dict(debug)
  | #Record(array<(string, debug)>)
]
let rec debug = (x): debug =>
  switch x.contents {
  | Unknown => #Polymorphic
  | Boolean => #Boolean
  | Int => #Int
  | Float => #Float
  | String => #String
  | Echo => #Echo
  | Nullable(x) => #Nullable(debug(x))
  | List(x) => #List(debug(x))
  | Tuple(x) => #Tuple(Array.map(x.contents, debug))
  | Dict(x, _) => #Dict(debug(x))
  | Record(x) => #Record(MapString.map(x.contents, debug)->MapString.toArray)
  }

module Child = {
  type t' = Child | NullableChild
  type t = ref<t'>
  type props = MapString.t<t>
  let props = a => MapString.fromArray(a)
  let child = () => ref(Child)
  let nullable = () => ref(NullableChild)
  let equal = (a: t, b: t) => a.contents == b.contents
  let toString = x =>
    switch x.contents {
    | Child => "Child"
    | NullableChild => "NullableChild"
    }
}
