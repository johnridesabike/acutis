/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Queue = Belt.MutableQueue
module SetString = Belt.Set.String
module MapString = Belt.Map.String
module Array = Belt.Array

module TypeScheme = {
  type rec typescheme =
    | Unknown
    | Boolean
    //| LiteralTrue
    //| LiteralFalse
    | Int
    | Float
    //| LiteralInt(NonEmpty.t<int>)
    | String
    //| LiteralString(NonEmpty.t<string>)
    // | LiteralNull
    | Echo
    | Nullable(t)
    | List(t)
    | Dict(t, ref<SetString.t>)
    // 0 and 1 sized tuples are legal.
    | Tuple(ref<array<t>>)
    | Record(ref<MapString.t<t>>)
  // The discriminant field, common field, and variant fields cannot intersect.
  //| UnionStr({discriminant: string, common: MapString.t<t>, variants: MapString.t<MapString.t<t>>})
  //| UnionInt({discriminant: string, common: MapString.t<t>, variants: MapInt.t<MapString.t<t>>})

  and t = ref<typescheme>

  type props = MapString.t<t>

  let rec toString = x =>
    switch x.contents {
    | Unknown => "unknown"
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

  and record_toString = x =>
    "{" ++
    MapString.toArray(x.contents)->Array.joinWith(", ", ((k, v)) => `"${k}": ${toString(v)}`) ++ "}"

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
}

module type Env = {
  type t
  type e
  let return: (. string) => t
  let error: (. string) => t
  let error_internal: (. array<Debug.t>) => t
  let render: (. Queue.t<t>) => t
  let try_: (. (. unit) => t, (. e) => t) => t
  let map: (. t, string => string) => t
  let flatmap: (. t, string => t) => t
}

type env<'a> = module(Env with type t = 'a)

type fnU<'a> = (. env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type fn<'a> = (env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type t<'a, 'b> = Acutis(string, 'a) | Function(string, TypeScheme.props, TypeScheme.Child.props, 'b)

let src = (~name, src) => Acutis(name, src)

// this makes components render faster.
let uncurry = (f, . a, b, c) => f(a, b, c)

let functionU = (~name, props, children, f) => Function(name, props, children, f)

let function = (~name, props, children, f) => Function(name, props, children, uncurry(f))

let name = (Acutis(n, _) | Function(n, _, _, _)) => n
