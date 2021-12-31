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

module Const = {
  type t =
    | PBool(bool)
    | PInt(int)
    | PString(string)
    | PFloat(float)

  let fromTPat = x =>
    switch x {
    | Typechecker.Pattern.TBool(x) => PBool(x)
    | TString(x) => PString(x)
    | TFloat(x) => PFloat(x)
    | TInt(x) => PInt(x)
    }

  let toTPat = x =>
    switch x {
    | PBool(x) => Typechecker.Pattern.TBool(x)
    | PString(x) => TString(x)
    | PFloat(x) => TFloat(x)
    | PInt(x) => TInt(x)
    }

  let equal = (a, b) =>
    switch (a, b) {
    | (PBool(a), PBool(b)) => a == b
    | (PString(a), PString(b)) => a == b
    | (PFloat(a), PFloat(b)) => a == b
    | (PInt(a), PInt(b)) => a == b
    | _ => assert false
    }

  let compare = (a, b) =>
    switch (a, b) {
    | (PBool(a), PBool(b)) => compare(a, b)
    | (PString(a), PString(b)) => compare(a, b)
    | (PFloat(a), PFloat(b)) => compare(a, b)
    | (PInt(a), PInt(b)) => compare(a, b)
    | _ => assert false
    }

  let toString = t =>
    switch t {
    | PString(s) => s
    | PFloat(n) => Float.toString(n)
    | PInt(i) => Int.toString(i)
    | PBool(true) => "true"
    | PBool(false) => "false"
    }
}

type rec t =
  | PUnknown(Json.t)
  | PNull
  | PArray(array<t>)
  | PDict(MapString.t<t>)
  | PConst(Const.t)

module Stack = {
  let nullable = "nullable"
  let array = "array" // add index later?
  let obj_key = s => "key: " ++ s
}

@raises(Exit)
let boolean = (~stack, j) =>
  switch Json.decodeBoolean(j) {
  | Some(b) => PConst(PBool(b))
  | None => raise(Exit(Debug.decodeError(Tys.boolean(), j, Tys.toString, ~stack)))
  }

@raises(Exit)
let string = (~stack, j) =>
  switch Json.decodeString(j) {
  | Some(s) => PConst(PString(s))
  | None => raise(Exit(Debug.decodeError(Tys.string(), j, Tys.toString, ~stack)))
  }

@raises(Exit)
let int = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | Some(i) => PConst(PInt(Int.fromFloat(i)))
  | None => raise(Exit(Debug.decodeError(Tys.int(), j, Tys.toString, ~stack)))
  }

@raises(Exit)
let float = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | Some(f) => PConst(PFloat(f))
  | None => raise(Exit(Debug.decodeError(Tys.float(), j, Tys.toString, ~stack)))
  }

@raises(Exit)
let echo = (~stack, j) =>
  switch Json.classify(j) {
  | JSONString(s) => PConst(PString(s))
  | JSONNumber(f) => PConst(PFloat(f))
  | _ => raise(Exit(Debug.decodeError(Tys.echo(), j, Tys.toString, ~stack)))
  }

let some = x => PArray([x])

@raises(Exit)
let rec nullable = (~stack, j, ty) =>
  if Js.Types.test(j, Null) || Js.Types.test(j, Undefined) {
    PNull
  } else {
    some(make(j, ty, ~stack=list{Stack.nullable, ...stack}))
  }

@raises(Exit)
and list = (~stack, j, ty) =>
  switch Json.decodeArray(j) {
  | Some(a) =>
    let l = ref(PNull)
    for i in Array.size(a) - 1 downto 0 {
      let x = Array.getUnsafe(a, i)
      l := PArray([make(x, ty, ~stack), l.contents])
    }
    l.contents
  | None => raise(Exit(Debug.decodeError(Tys.list(ty), j, Tys.toString, ~stack)))
  }

@raises(Exit)
and dict = (~stack, j, ty) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let keys = Dict.entries(obj)
    let dict = Array.mapU(keys, (. (k, v)) => (k, make(v, ty, ~stack)))
    PDict(MapString.fromArray(dict))
  | None => raise(Exit(Debug.decodeError(Tys.dict(ty), j, Tys.toString, ~stack)))
  }

@raises(Exit)
and tuple = (~stack, j, tys) =>
  switch Json.decodeArray(j) {
  | Some(arr) =>
    PArray(Array.zipByU(tys, arr, (. ty, j) => make(j, ty, ~stack=list{Stack.array, ...stack})))
  | None => raise(Exit(Debug.decodeError(Tys.tuple(tys), j, Tys.toString, ~stack)))
  }

@raises(Exit)
and recordAux = (~stack, j, tys) => {
  MapString.mapWithKeyU(tys, (. k, ty) =>
    switch (ty, Dict.get(j, k)) {
    | ({contents: Tys.Nullable(_) | Unknown}, None) => PNull
    | (ty, Some(j)) => make(j, ty, ~stack=list{Stack.obj_key(k), ...stack})
    | _ => raise(Exit(Debug.decodeErrorMissingKey(~stack, k)))
    }
  )
}

@raises(Exit)
and record = (~stack, j, tys) =>
  switch Json.decodeObject(j) {
  | Some(obj) => PDict(recordAux(obj, tys, ~stack))
  | _ => raise(Exit(Debug.decodeError(Tys.record2(tys), j, Tys.toString, ~stack)))
  }

@raises(Exit)
and make = (~stack, j, ty) =>
  switch ty.contents {
  | Tys.Unknown => PUnknown(j)
  | Nullable(ty) => nullable(~stack, j, ty)
  | Boolean => boolean(~stack, j)
  | String => string(~stack, j)
  | Int => int(~stack, j)
  | Float => float(~stack, j)
  | Echo => echo(~stack, j)
  | List(ty) => list(~stack, j, ty)
  | Dict(ty, _) => dict(~stack, j, ty)
  | Tuple(ty) => tuple(~stack, j, ty.contents)
  | Record(ty) => record(~stack, j, ty.contents)
  }

@raises(Exit)
let make = (j, ty) => recordAux(j, ty, ~stack=list{})

let constantExn = t =>
  switch t {
  | PConst(x) => x
  | _ => assert false
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
  | Typechecker.Pattern.TConst(_, TBool(x)) => PConst(PBool(x))
  | TConst(_, TString(x)) => PConst(PString(x))
  | TConst(_, TInt(x)) => PConst(PInt(x))
  | TConst(_, TFloat(x)) => PConst(PFloat(x))
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
    let d = Dict.empty()
    for i in 0 to Array.size(x) - 1 {
      let (k, v) = Array.getUnsafe(x, i)
      Dict.set(d, k, fromPattern(v, props))
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
        f(. ~index=PConst(PInt(i)), hd)
        aux(succ(i), tl)
      | _ => assert false
      }
    }
  aux(0, l)
}

let forEachDictExn = (d, f) => {
  MapString.forEachU(dictExn(d), (. k, v) => f(. ~index=PConst(PString(k)), v))
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
  | (PConst(PBool(b)), Boolean) => Json.boolean(b)
  | (PConst(PInt(i)), Int | Echo) => Json.number(Int.toFloat(i))
  | (PConst(PFloat(f)), Float | Echo) => Json.number(f)
  | (PConst(PString(s)), String | Echo) => Json.string(s)
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
