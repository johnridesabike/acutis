/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

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
  | Dict(t, ref<Belt.Set.String.t>)
  // 0 and 1 sized tuples are legal.
  | Tuple(ref<array<t>>)
  | Record(ref<Belt.Map.String.t<t>>)
// The discriminant field, common field, and variant fields cannot intersect.
//| UnionStr({discriminant: string, common: MapString.t<t>, variants: MapString.t<MapString.t<t>>})
//| UnionInt({discriminant: string, common: MapString.t<t>, variants: MapInt.t<MapString.t<t>>})

and t = ref<typescheme>

type props = Belt.Map.String.t<t>

let toString: t => string

let record_toString: ref<props> => string

let copy: typescheme => typescheme

let copy_record: props => props

let unknown: unit => t
let boolean: unit => t
let int: unit => t
let float: unit => t
let string: unit => t
let echo: unit => t
let nullable: t => t
let list: t => t
let dict: t => t
let tuple: array<t> => t
let record: array<(string, t)> => t
let record2: props => t
let props: array<(string, t)> => props

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
let debug: t => debug

module Child: {
  type t' = Child | NullableChild
  type t = ref<t'>
  type props = Belt.Map.String.t<t>
  let props: array<(string, t)> => props
  let child: unit => t
  let nullable: unit => t
  let equal: (t, t) => bool
  let toString: t => string
}
