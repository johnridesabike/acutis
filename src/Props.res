/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Dict = Js.Dict
module Json = Js.Json
module Float = Belt.Float
module Int = Belt.Int
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module Tys = Source2.TypeScheme

type t = Json.t

exception Exit = Debug.Exit

module Stack = {
  let nullable = Json.string("nullable")
  let array = Json.string("array") // add index
  let obj_key = s => Json.string("key: " ++ s)
}

let jsonInt = i => Json.number(Int.toFloat(i))

let null = Json.null

let some = x => Json.array([x])

let rec nullable = (~stack, ty, j) =>
  if Js.Types.test(j, Null) || Js.Types.test(j, Undefined) {
    null
  } else {
    some(make(~stack=list{Stack.nullable, ...stack}, ty, j))
  }

and boolean = (~stack, j) =>
  if Json.test(j, Boolean) {
    j
  } else {
    raise(Exit(Debug2.decodeError(Tys.boolean(), j, ~f=Tys.toString, ~stack)))
  }

and string = (~stack, j) =>
  if Json.test(j, String) {
    j
  } else {
    raise(Exit(Debug2.decodeError(Tys.string(), j, ~f=Tys.toString, ~stack)))
  }

and int = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | None => raise(Exit(Debug2.decodeError(Tys.int(), j, ~f=Tys.toString, ~stack)))
  | Some(i) => Json.number(Float.fromInt(Int.fromFloat(i)))
  }

and float = (~stack, j) =>
  if Json.test(j, Number) {
    j
  } else {
    raise(Exit(Debug2.decodeError(Tys.float(), j, ~f=Tys.toString, ~stack)))
  }

and echo = (~stack, j) =>
  switch Json.classify(j) {
  | JSONString(_) | JSONNumber(_) => j
  | _ => raise(Exit(Debug2.decodeError(Tys.echo(), j, ~f=Tys.toString, ~stack)))
  }

and list = (~stack, ty, j) =>
  switch Json.decodeArray(j) {
  | Some(a) =>
    let l = ref(Json.null)
    for i in Array.size(a) - 1 downto 0 {
      let x = Array.getUnsafe(a, i)
      l := Json.array([make(~stack, ty, x), l.contents])
    }
    l.contents
  | None => raise(Exit(Debug2.decodeError(Tys.list(ty), j, ~f=Tys.toString, ~stack)))
  }

and dict = (~stack, ty, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let dict = Js.Dict.empty()
    let keys = Js.Dict.keys(obj)
    Array.forEachU(keys, (. key) =>
      Js.Dict.set(dict, key, make(~stack, ty, Js.Dict.unsafeGet(obj, key)))
    )
    Json.object_(dict)
  | None => raise(Exit(Debug2.decodeError(Tys.dict(ty), j, ~f=Tys.toString, ~stack)))
  }

and tuple = (~stack, tys, j) =>
  switch Json.decodeArray(j) {
  | Some(arr) =>
    Json.array(
      Array.zipByU(tys, arr, (. ty, json) => make(~stack=list{Stack.array, ...stack}, ty, json)),
    )
  | None => raise(Exit(Debug2.decodeError(Tys.tuple(tys), j, ~f=Tys.toString, ~stack)))
  }

and recordAux = (~stack, tys, j) => {
  let dict = Js.Dict.empty()
  MapString.forEachU(tys, (. k, ty) =>
    switch (ty, Js.Dict.get(j, k)) {
    | ({contents: Tys.Nullable(_) | Unknown}, None) => Js.Dict.set(dict, k, null)
    | (ty, Some(j)) => Js.Dict.set(dict, k, make(~stack=list{Stack.obj_key(k), ...stack}, ty, j))
    | _ => raise(Exit(Debug2.decodeErrorMissingKey(~stack, k)))
    }
  )
  dict
}

and record = (~stack, tys, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) => Json.object_(recordAux(~stack, tys, obj))
  | _ => raise(Exit(Debug2.decodeError(Tys.record2(tys), j, ~f=Tys.toString, ~stack)))
  }

and make = (~stack, ty, json) =>
  switch ty.contents {
  | Tys.Unknown => json
  | Nullable(ty) => nullable(~stack, ty, json)
  | Boolean => boolean(~stack, json)
  | String => string(~stack, json)
  | Int => int(~stack, json)
  | Float => float(~stack, json)
  | Echo => echo(~stack, json)
  | List(ty) => list(~stack, ty, json)
  | Dict(ty, _) => dict(~stack, ty, json)
  | Tuple(ty) => tuple(~stack, ty.contents, json)
  | Record(ty) => record(~stack, ty.contents, json)
  }

let make = (ty, j) => recordAux(~stack=list{}, ty, j)

exception RenderError

@raises(RenderError)
let booleanExn = t =>
  switch Json.decodeBoolean(t) {
  | Some(t) => t
  | None =>
    Js.log2("bool", t)
    raise(RenderError)
  }

@raises(RenderError)
let echoExn = t =>
  switch Json.classify(t) {
  | JSONString(s) => s
  | JSONNumber(n) => Float.toString(n)
  | _ =>
    Js.log2("echo", t)
    raise(RenderError)
  }

@raises(RenderError)
let stringExn = t =>
  switch Json.decodeString(t) {
  | Some(t) => t
  | None =>
    Js.log2("string", t)
    raise(RenderError)
  }

@raises(RenderError)
let intExn = t =>
  switch Json.decodeNumber(t) {
  | Some(t) => Int.fromFloat(t)
  | None =>
    Js.log2("int", t)
    raise(RenderError)
  }

@raises(RenderError)
let floatExn = t =>
  switch Json.decodeNumber(t) {
  | Some(t) => t
  | None =>
    Js.log2("float", t)
    raise(RenderError)
  }

@raises(RenderError)
let tupleExn = t =>
  switch Json.decodeArray(t) {
  | Some(t) => t
  | None =>
    Js.log2("tuple", Js.Json.stringifyAny(t))
    assert false
  }

@raises(RenderError)
let dictExn = t =>
  switch Json.decodeObject(t) {
  | Some(t) => t
  | None =>
    Js.log2("dict", t)
    raise(RenderError)
  }

let isNull = t => Json.test(t, Null)

let nullableExn = t =>
  switch Json.classify(t) {
  | JSONNull => None
  | JSONArray([t]) => Some(t)
  | _ => assert false
  }

let rec fromPattern = (x, props) =>
  switch x {
  | Typechecker.Pattern.TPat_Const(_, TPat_Bool(x)) => Json.boolean(x)
  | TPat_Const(_, TPat_String(x)) => Json.string(x)
  | TPat_Const(_, TPat_Int(x)) => jsonInt(x)
  | TPat_Const(_, TPat_Float(x)) => Json.number(x)
  | TPat_OptionalVar(_, x) | TPat_Var(_, x) =>
    switch Js.Dict.get(props, x) {
    | Some(x) => x
    | None => assert false
    }
  | TPat_Construct(_, _, Some(x)) => fromPattern(x, props)
  | TPat_Construct(_, _, None) => Json.null
  | TPat_Tuple(_, x) => Json.array(Array.map(x, x => fromPattern(x, props)))
  | TPat_Record(_, x)
  | TPat_Dict(_, x) =>
    let d = Js.Dict.empty()
    for i in 0 to Array.size(x) - 1 {
      let (k, v) = Array.getUnsafe(x, i)
      Js.Dict.set(d, k, fromPattern(v, props))
    }
    Json.object_(d)
  | TPat_Any(_) => assert false
  }

let forEachListExn = (l, f) => {
  let rec aux = (i, l) =>
    if isNull(l) {
      ()
    } else {
      switch tupleExn(l) {
      | [hd, tl] =>
        f(. ~index=jsonInt(i), hd)
        aux(succ(i), tl)
      | _ => assert false
      }
    }
  aux(0, l)
}

let forEachDictExn = (d, f) => {
  let d = dictExn(d)
  let keys = Js.Dict.keys(d)
  for i in 0 to Array.size(keys) - 1 {
    let k = Array.getUnsafe(keys, i)
    let v = Js.Dict.unsafeGet(d, k)
    f(. ~index=Json.string(k), v)
  }
}

let rec toJson_list = (l, ty) => {
  let q = Queue.make()
  let rec aux = l =>
    switch Json.classify(l) {
    | JSONNull => Json.array(Queue.toArray(q))
    | JSONArray([hd, tl]) =>
      Queue.add(q, toJson(hd, ty))
      aux(tl)
    | _ => assert false
    }
  aux(l)
}

and toJson_record = (t, ty) => {
  let r = Dict.empty()
  MapString.forEachU(ty, (. k, v) =>
    switch Dict.get(t, k) {
    | None => assert false
    | Some(t) => Dict.set(r, k, toJson(t, v.contents))
    }
  )
  r
}

and toJson = (t, ty) =>
  switch (Json.classify(t), ty) {
  | (_, Tys.Unknown)
  | (JSONTrue | JSONFalse, Boolean)
  | (JSONNumber(_), Int | Float)
  | (JSONString(_), String)
  | (JSONNumber(_) | JSONString(_), Echo)
  | (JSONNull, Nullable(_)) => t
  | (JSONArray([t]), Nullable(ty)) => toJson(t, ty.contents)
  | (_, List(ty)) => toJson_list(t, ty.contents)
  | (JSONArray(a), Tuple(ty)) =>
    assert (Array.size(a) == Array.size(ty.contents))
    Array.zipByU(a, ty.contents, (. t, ty) => toJson(t, ty.contents))->Json.array
  | (JSONObject(o), Dict(ty, _)) =>
    let r = Dict.empty()
    Array.forEachU(Dict.entries(o), (. (k, v)) => Dict.set(r, k, toJson(v, ty.contents)))
    Json.object_(r)
  | (JSONObject(o), Record(ty)) => Json.object_(toJson_record(o, ty.contents))
  | _ => assert false
  }

let toJson = toJson_record

module Child = {
  let validate = (tys, m) =>
    MapString.forEachU(tys, (. k, {contents}) =>
      switch (Dict.get(m, k), contents) {
      | (None, Tys.Child.Child) => raise(Exit(Debug2.decodeErrorMissingChild(k)))
      | (Some(_), Child) | (Some(_) | None, NullableChild) => ()
      }
    )
}
