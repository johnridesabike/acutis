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
module Tys = Typescheme

exception Exit = Debug.Exit

type rec t =
  | PUnknown(Json.t)
  | PNull
  | PBool(bool)
  | PString(string)
  | PFloat(float)
  | PInt(int)
  | PArray(array<t>)
  | PDict(MapString.t<t>)

module Stack = {
  let nullable = Json.string("nullable")
  let array = Json.string("array") // add index
  let obj_key = s => Json.string("key: " ++ s)
}

let some = x => PArray([x])

let rec nullable = (~stack, ty, j) =>
  if Js.Types.test(j, Null) || Js.Types.test(j, Undefined) {
    PNull
  } else {
    some(make(~stack=list{Stack.nullable, ...stack}, ty, j))
  }

and boolean = (~stack, j) =>
  switch Json.decodeBoolean(j) {
  | Some(b) => PBool(b)
  | None => raise(Exit(Debug.decodeError(Tys.boolean(), j, Tys.toString, ~stack)))
  }

and string = (~stack, j) =>
  switch Json.decodeString(j) {
  | Some(s) => PString(s)
  | None => raise(Exit(Debug.decodeError(Tys.string(), j, Tys.toString, ~stack)))
  }

and int = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | Some(i) => PInt(Int.fromFloat(i))
  | None => raise(Exit(Debug.decodeError(Tys.int(), j, Tys.toString, ~stack)))
  }

and float = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | Some(f) => PFloat(f)
  | None => raise(Exit(Debug.decodeError(Tys.float(), j, Tys.toString, ~stack)))
  }

and echo = (~stack, j) =>
  switch Json.classify(j) {
  | JSONString(s) => PString(s)
  | JSONNumber(f) => PFloat(f)
  | _ => raise(Exit(Debug.decodeError(Tys.echo(), j, Tys.toString, ~stack)))
  }

and list = (~stack, ty, j) =>
  switch Json.decodeArray(j) {
  | Some(a) =>
    let l = ref(PNull)
    for i in Array.size(a) - 1 downto 0 {
      let x = Array.getUnsafe(a, i)
      l := PArray([make(~stack, ty, x), l.contents])
    }
    l.contents
  | None => raise(Exit(Debug.decodeError(Tys.list(ty), j, Tys.toString, ~stack)))
  }

and dict = (~stack, ty, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let keys = Js.Dict.entries(obj)
    let dict = Array.mapU(keys, (. (k, v)) => (k, make(~stack, ty, v)))
    PDict(MapString.fromArray(dict))
  | None => raise(Exit(Debug.decodeError(Tys.dict(ty), j, Tys.toString, ~stack)))
  }

and tuple = (~stack, tys, j) =>
  switch Json.decodeArray(j) {
  | Some(arr) =>
    PArray(
      Array.zipByU(tys, arr, (. ty, json) => make(~stack=list{Stack.array, ...stack}, ty, json)),
    )
  | None => raise(Exit(Debug.decodeError(Tys.tuple(tys), j, Tys.toString, ~stack)))
  }

and recordAux = (~stack, tys, j) => {
  MapString.mapWithKeyU(tys, (. k, ty) =>
    switch (ty, Js.Dict.get(j, k)) {
    | ({contents: Tys.Nullable(_) | Unknown}, None) => PNull
    | (ty, Some(j)) => make(~stack=list{Stack.obj_key(k), ...stack}, ty, j)
    | _ => raise(Exit(Debug.decodeErrorMissingKey(~stack, k)))
    }
  )
}

and record = (~stack, tys, j) =>
  switch Json.decodeObject(j) {
  | Some(obj) => PDict(recordAux(~stack, tys, obj))
  | _ => raise(Exit(Debug.decodeError(Tys.record2(tys), j, Tys.toString, ~stack)))
  }

and make = (~stack, ty, json) =>
  switch ty.contents {
  | Tys.Unknown => PUnknown(json)
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

let booleanExn = t =>
  switch t {
  | PBool(t) => t
  | _ =>
    Js.log2("bool", t)
    assert false
  }

let echoExn = t =>
  switch t {
  | PString(s) => s
  | PFloat(n) => Float.toString(n)
  | PInt(i) => Int.toString(i)
  | _ =>
    Js.log2("echo", t)
    assert false
  }

let stringExn = t =>
  switch t {
  | PString(t) => t
  | _ =>
    Js.log2("string", t)
    assert false
  }

let intExn = t =>
  switch t {
  | PInt(t) => t
  | _ =>
    Js.log2("int", t)
    assert false
  }

let floatExn = t =>
  switch t {
  | PFloat(t) => t
  | _ =>
    Js.log2("float", t)
    assert false
  }

let tupleExn = t =>
  switch t {
  | PArray(t) => t
  | _ =>
    Js.log2("tuple", Js.Json.stringifyAny(t))
    assert false
  }

let dictExn = t =>
  switch t {
  | PDict(t) => t
  | _ =>
    Js.log2("dict", t)
    assert false
  }

let isNull = t => t == PNull

let nullableExn = t =>
  switch t {
  | PNull => None
  | PArray([t]) => Some(t)
  | _ => assert false
  }

let rec fromPattern = (x, props) =>
  switch x {
  | Typechecker.Pattern.TConst(_, TBool(x)) => PBool(x)
  | TConst(_, TString(x)) => PString(x)
  | TConst(_, TInt(x)) => PInt(x)
  | TConst(_, TFloat(x)) => PFloat(x)
  | TOptionalVar(_, x) | TVar(_, x) =>
    switch MapString.get(props, x) {
    | Some(x) => x
    | None => assert false
    }
  | TConstruct(_, _, Some(x)) => fromPattern(x, props)
  | TConstruct(_, _, None) => PNull
  | TTuple(_, x) => PArray(Array.map(x, x => fromPattern(x, props)))
  | TRecord(_, x)
  | TDict(_, x) =>
    let d = Js.Dict.empty()
    for i in 0 to Array.size(x) - 1 {
      let (k, v) = Array.getUnsafe(x, i)
      Js.Dict.set(d, k, fromPattern(v, props))
    }
    let d = Array.mapU(x, (. (k, v)) => (k, fromPattern(v, props)))->MapString.fromArray
    PDict(d)
  | TAny(_) => assert false
  }

let forEachListExn = (l, f) => {
  let rec aux = (i, l) =>
    if isNull(l) {
      ()
    } else {
      switch tupleExn(l) {
      | [hd, tl] =>
        f(. ~index=PInt(i), hd)
        aux(succ(i), tl)
      | _ => assert false
      }
    }
  aux(0, l)
}

let forEachDictExn = (d, f) => {
  MapString.forEachU(dictExn(d), (. k, v) => f(. ~index=PString(k), v))
}

let rec toJson_list = (l, ty) => {
  let q = Queue.make()
  let rec aux = l =>
    switch l {
    | PNull => Json.array(Queue.toArray(q))
    | PArray([hd, tl]) =>
      Queue.add(q, toJson(hd, ty))
      aux(tl)
    | _ => assert false
    }
  aux(l)
}

and toJson_record = (t, ty) => {
  MapString.mapWithKeyU(ty, (. k, v) =>
    switch MapString.get(t, k) {
    | None => assert false
    | Some(t) => toJson(t, v.contents)
    }
  )
  ->MapString.toArray
  ->Dict.fromArray
}

and toJson = (t, ty) =>
  switch (t, ty) {
  | (PUnknown(j), Tys.Unknown) => j
  | (PBool(b), Boolean) => Json.boolean(b)
  | (PInt(i), Int | Echo) => Json.number(Int.toFloat(i))
  | (PFloat(f), Float | Echo) => Json.number(f)
  | (PString(s), String | Echo) => Json.string(s)
  | (PNull, Nullable(_)) => Json.null
  | (PArray([t]), Nullable(ty)) => toJson(t, ty.contents)
  | (_, List(ty)) => toJson_list(t, ty.contents)
  | (PArray(a), Tuple(ty)) =>
    assert (Array.size(a) == Array.size(ty.contents))
    Array.zipByU(a, ty.contents, (. t, ty) => toJson(t, ty.contents))->Json.array
  | (PDict(o), Dict(ty, _)) =>
    let r = Dict.empty()
    Array.forEachU(MapString.toArray(o), (. (k, v)) => Dict.set(r, k, toJson(v, ty.contents)))
    Json.object_(r)
  | (PDict(o), Record(ty)) => Json.object_(toJson_record(o, ty.contents))
  | _ => assert false
  }

let toJson = toJson_record
