/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Json = Js.Json
module Float = Belt.Float
module Int = Belt.Int
module MapString = Belt.Map.String

type t

exception DecodeError

let polymorphic = Obj.magic
@val external null: t = "null"
let tuple_: array<t> => t = Obj.magic

let rec nullable = (ty, j) =>
  if Json.test(j, Null) || Js.Types.test(j, Undefined) {
    null
  } else {
    tuple_([make(ty, j)])
  }

and boolean = json =>
  if Json.test(json, Boolean) {
    Obj.magic(json)
  } else {
    raise(DecodeError)
  }

and string = json =>
  if Json.test(json, String) {
    Obj.magic(json)
  } else {
    raise(DecodeError)
  }

and int = json =>
  switch Json.decodeNumber(json) {
  | None => raise(DecodeError)
  | Some(i) => Obj.magic(Float.fromInt(Int.fromFloat(i)))
  }

and float = json =>
  if Json.test(json, Number) {
    Obj.magic(json)
  } else {
    raise(DecodeError)
  }

and echo = j =>
  switch Json.classify(j) {
  | JSONString(s) => Obj.magic(s)
  | JSONNumber(n) => Obj.magic(n)
  | _ => raise(DecodeError)
  }

and list = (ty, j) =>
  switch Json.decodeArray(j) {
  | Some(a) =>
    let l = ref(null)
    for i in Array.size(a) - 1 downto 0 {
      let x = Array.getUnsafe(a, i)
      l := tuple_([make(ty, x), l.contents])
    }
    l.contents
  | None => raise(DecodeError)
  }

and dict = (ty, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let dict = Js.Dict.empty()
    let keys = Js.Dict.keys(obj)
    Array.forEachU(keys, (. key) => Js.Dict.set(dict, key, make(ty, Js.Dict.unsafeGet(obj, key))))
    Obj.magic(dict)
  | None => raise(DecodeError)
  }

and tuple = (tys, j) =>
  switch Json.decodeArray(j) {
  | Some(arr) => tuple_(Array.zipByU(tys, arr, (. {contents}, json) => make(contents, json)))
  | None => raise(DecodeError)
  }

and record = (tys, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let dict = Js.Dict.empty()
    MapString.forEachU(tys, (. k, {contents}) =>
      switch Js.Dict.get(obj, k) {
      | None => raise(DecodeError)
      | Some(json) => Js.Dict.set(dict, k, make(contents, json))
      }
    )
    Obj.magic(dict)
  | _ => raise(DecodeError)
  }

and make = (ty, json) =>
  switch ty {
  | TypeChecker.Polymorphic => polymorphic(json)
  | Nullable({contents}) => nullable(contents, json)
  | Boolean => boolean(json)
  | String => string(json)
  | Int => int(json)
  | Float => float(json)
  | Echo => echo(json)
  | List({contents}) => list(contents, json)
  | Dict({contents}, _) => dict(contents, json)
  | Tuple({contents}) => tuple(contents, json)
  | Record({contents}) => record(contents, json)
  }

exception RenderError

let booleanExn = t =>
  if Js.typeof(t) == "boolean" {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let stringExn = t =>
  if Js.typeof(t) == "string" {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let intExn = t =>
  if Js.typeof(t) == "number" {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let floatExn = t =>
  if Js.typeof(t) == "number" {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let tupleExn = t =>
  if Js.Array2.isArray(t) {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let dictExn = t =>
  if Js.typeof(t) == "object" && !Js.Array2.isArray(t) && !(Obj.magic(t) == Js.null) {
    Obj.magic(t)
  } else {
    raise(RenderError)
  }

let isNull = t => Obj.magic(t) == Js.null
