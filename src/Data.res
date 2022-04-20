/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Dict = Js.Dict
module Json = Js.Json
module Float = Belt.Float
module Int = Belt.Int
module MapInt = Belt.Map.Int
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module SetInt = Belt.Set.Int
module SetString = Belt.Set.String
module Ty = Typescheme

exception Exit = Debug.Exit

module Const = {
  type t =
    | PInt(int)
    | PString(string)
    | PFloat(float)

  let fromTPat = x =>
    switch x {
    | Typechecker.Pattern.TString(x) => PString(x)
    | TFloat(x) => PFloat(x)
    | TInt(x) => PInt(x)
    }

  let toTPat = x =>
    switch x {
    | PString(x) => Typechecker.Pattern.TString(x)
    | PFloat(x) => TFloat(x)
    | PInt(x) => TInt(x)
    }

  let equal = (a, b) =>
    switch (a, b) {
    | (PString(a), PString(b)) => a == b
    | (PFloat(a), PFloat(b)) => a == b
    | (PInt(a), PInt(b)) => a == b
    | _ => assert false
    }

  let compare = (a, b) =>
    switch (a, b) {
    | (PString(a), PString(b)) => compare(a, b)
    | (PFloat(a), PFloat(b)) => compare(a, b)
    | (PInt(a), PInt(b)) => compare(a, b)
    | _ => assert false
    }

  let toString = (t, extra) =>
    switch t {
    | PString(s) => s
    | PFloat(n) => Float.toString(n)
    | PInt(i) =>
      switch (i, extra) {
      | (i, Ty.Variant.Extra_none) => Int.toString(i)
      | (0, Extra_boolean) => "false"
      | (_, Extra_boolean) => "true"
      }
    }
}

type rec t =
  | PUnknown(Json.t)
  | PNull
  | PArray(array<t>)
  | PDict(MapString.t<t>)
  | PConst(Const.t, Ty.Variant.extra)

module Stack = {
  let nullable = "nullable"
  let array = i => "index: " ++ Int.toString(i)
  let obj_key = s => "key: " ++ s
}

@raises(Exit)
let boolean = (~stack, j, ty, cases) => {
  let i = switch Json.decodeBoolean(j) {
  | Some(false) => 0
  | Some(true) => 1
  | None => raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  }
  if SetInt.has(cases, i) {
    PConst(PInt(i), Extra_boolean)
  } else {
    raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  }
}

@raises(Exit)
let string = (~stack, j, ty, cases) =>
  switch Json.decodeString(j) {
  | Some(s) =>
    switch cases {
    | None => PConst(PString(s), Extra_none)
    | Some(cases) =>
      if SetString.has(cases, s) {
        PConst(PString(s), Extra_none)
      } else {
        raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
      }
    }
  | None => raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  }

@raises(Exit)
let int = (~stack, j, ty, cases) =>
  switch Json.decodeNumber(j) {
  | Some(i) =>
    let i = Int.fromFloat(i)
    switch cases {
    | None => PConst(PInt(i), Extra_none)
    | Some(cases) =>
      if SetInt.has(cases, i) {
        PConst(PInt(i), Extra_none)
      } else {
        raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
      }
    }
  | None => raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  }

@raises(Exit)
let float = (~stack, j) =>
  switch Json.decodeNumber(j) {
  | Some(f) => PConst(PFloat(f), Extra_none)
  | None => raise(Exit(Debug.decodeError(Ty.float(), j, Ty.toString, ~stack)))
  }

@raises(Exit)
let echo = (~stack, j) =>
  switch Json.classify(j) {
  | JSONString(s) => PConst(PString(s), Extra_none)
  | JSONNumber(f) => PConst(PFloat(f), Extra_none)
  | JSONFalse => PConst(PInt(0), Extra_boolean)
  | JSONTrue => PConst(PInt(1), Extra_boolean)
  | _ => raise(Exit(Debug.decodeError(Ty.echo(), j, Ty.toString, ~stack)))
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
      l := PArray([make(x, ty, ~stack=list{Stack.array(i), ...stack}), l.contents])
    }
    l.contents
  | None => raise(Exit(Debug.decodeError(Ty.list(ty), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and dict = (~stack, j, ty) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let keys = Dict.entries(obj)
    let dict = Array.mapU(keys, (. (k, v)) => (
      k,
      make(v, ty, ~stack=list{Stack.obj_key(k), ...stack}),
    ))
    PDict(MapString.fromArray(dict))
  | None => raise(Exit(Debug.decodeError(Ty.dict(ty), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and tuple = (~stack, j, tys) =>
  switch Json.decodeArray(j) {
  | Some(arr) =>
    let arr =
      Array.zip(arr, tys)->Array.mapWithIndexU((. i, (j, ty)) =>
        make(j, ty, ~stack=list{Stack.array(i), ...stack})
      )
    PArray(arr)
  | None => raise(Exit(Debug.decodeError(Ty.tuple(tys), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and recordAux = (~stack, j, tys) => {
  MapString.mapWithKeyU(tys, (. k, ty) =>
    switch (ty, Dict.get(j, k)) {
    | ({contents: Ty.Nullable(_) | Unknown}, None) => PNull
    | (ty, Some(j)) => make(j, ty, ~stack=list{Stack.obj_key(k), ...stack})
    | _ => raise(Exit(Debug.decodeErrorMissingKey(~stack, k)))
    }
  )
}

@raises(Exit)
and record = (~stack, j, map, ty) =>
  switch Json.decodeObject(j) {
  | Some(obj) => PDict(recordAux(obj, map.contents, ~stack))
  | None => raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  }

@raises(Exit)
and union = (~stack, j, key, map, extra, ty) => {
  @raises(Exit)
  let fail = () => raise(Exit(Debug.decodeError(ty, j, Ty.toString, ~stack)))
  switch Json.decodeObject(j) {
  | Some(obj) =>
    switch Dict.get(obj, key) {
    | Some(tag) =>
      let (tag, map) = switch (Json.classify(tag), map, extra) {
      | (JSONFalse, Ty.Variant.Int(map), Ty.Variant.Extra_boolean) =>
        let tag = 0
        (PConst(PInt(tag), extra), MapInt.get(map, tag))
      | (JSONTrue, Int(map), Extra_boolean) =>
        let tag = 1
        (PConst(PInt(tag), extra), MapInt.get(map, tag))
      | (JSONNumber(tag), Int(map), Extra_none) =>
        let tag = Int.fromFloat(tag)
        (PConst(PInt(tag), extra), MapInt.get(map, tag))
      | (JSONString(tag), String(map), _) => (PConst(PString(tag), extra), MapString.get(map, tag))
      | _ => fail()
      }
      switch map {
      | Some(map) =>
        let r = recordAux(obj, map.contents, ~stack)
        PDict(MapString.set(r, key, tag))
      | None => fail()
      }
    | None => fail()
    }
  | None => fail()
  }
}

@raises(Exit)
and make = (~stack, j, ty) =>
  switch ty.contents {
  | Ty.Unknown => PUnknown(j)
  | Nullable(ty) => nullable(~stack, j, ty)
  | Enum({extra: Extra_boolean, cases: Int(cases), _}) => boolean(~stack, j, ty, cases)
  | String | Enum({row: Open, cases: String(_), _}) => string(~stack, j, ty, None)
  | Enum({cases: String(cases), row: Closed, _}) => string(~stack, j, ty, Some(cases))
  | Int | Enum({row: Open, cases: Int(_), _}) => int(~stack, j, ty, None)
  | Enum({cases: Int(cases), row: Closed, _}) => int(~stack, j, ty, Some(cases))
  | Float => float(~stack, j)
  | Echo => echo(~stack, j)
  | List(ty) => list(~stack, j, ty)
  | Dict(ty, _) => dict(~stack, j, ty)
  | Tuple(tys) => tuple(~stack, j, tys)
  | Record(tys) => record(~stack, j, tys, ty)
  | Union(key, {cases, extra, _}) => union(~stack, j, key, cases, extra, ty)
  }

@raises(Exit)
let make = (j, ty) => recordAux(j, ty, ~stack=list{})

let constantExn = t =>
  switch t {
  | PConst(x, _) => x
  | _ => assert false
  }

let tupleExn = t =>
  switch t {
  | PArray(t) => t
  | _ => assert false
  }

let dictExn = t =>
  switch t {
  | PDict(t) => t
  | _ => assert false
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
  | Typechecker.Pattern.TConst(_, x, Some({extra: Extra_boolean, _})) =>
    PConst(Const.fromTPat(x), Extra_boolean)
  | TConst(_, x, _) => PConst(Const.fromTPat(x), Extra_none)
  | TOptionalVar(_, x) | TVar(_, x) =>
    switch MapString.get(props, x) {
    | Some(x) => x
    | None => assert false
    }
  | TConstruct(_, _, Some(x)) => fromPattern(x, props)
  | TConstruct(_, _, None) => PNull
  | TTuple(_, x) => PArray(Array.map(x, x => fromPattern(x, props)))
  | TRecord(_, Some((k, v, {extra, _})), x, _) =>
    let m =
      x
      ->MapString.mapU((. v) => fromPattern(v, props))
      ->MapString.set(k, PConst(Const.fromTPat(v), extra))
    PDict(m)
  | TRecord(_, None, x, _) | TDict(_, x, _) =>
    PDict(MapString.mapU(x, (. v) => fromPattern(v, props)))
  | TAny(_) => assert false
  }

let forEachListExn = (l, f) => {
  let rec aux = (i, l) =>
    if isNull(l) {
      ()
    } else {
      switch tupleExn(l) {
      | [hd, tl] =>
        f(. ~index=PConst(PInt(i), Extra_none), hd)
        aux(succ(i), tl)
      | _ => assert false
      }
    }
  aux(0, l)
}

let forEachDictExn = (d, f) => {
  MapString.forEachU(dictExn(d), (. k, v) => f(. ~index=PConst(PString(k), Extra_none), v))
}

let toString = t =>
  switch t {
  | PConst(x, e) => Const.toString(x, e)
  | _ => assert false
  }

let rec toJson_record = (t, ty) =>
  MapString.mergeU(t, ty, (. _, t, ty) =>
    switch (t, ty) {
    | (Some(t), Some(ty)) => Some(toJson(t, ty.contents))
    | _ => None
    }
  )
  ->MapString.toArray
  ->Dict.fromArray

and toJson = (t, ty) =>
  switch (t, ty) {
  | (PUnknown(j), _) => j
  | (PConst(PFloat(f), _), _) => Json.number(f)
  | (PConst(PString(s), _), _) => Json.string(s)
  | (PConst(PInt(0), Extra_boolean), Ty.Enum({extra: Extra_boolean, _}) | Echo) =>
    Json.boolean(false)
  | (PConst(PInt(_), Extra_boolean), Enum({extra: Extra_boolean, _}) | Echo) => Json.boolean(true)
  | (PConst(PInt(i), _), Enum(_) | Int | Echo) => Json.number(Int.toFloat(i))
  | (PNull, Nullable(_)) => Json.null
  | (PArray([t]), Nullable(ty)) => toJson(t, ty.contents)
  | (t, List(ty)) =>
    let q = Queue.make()
    let rec aux = l =>
      switch l {
      | PNull => Json.array(Queue.toArray(q))
      | PArray([hd, tl]) =>
        Queue.add(q, toJson(hd, ty.contents))
        aux(tl)
      | _ => assert false
      }
    aux(t)
  | (PArray(a), Tuple(tys)) => Array.zipByU(a, tys, (. t, ty) => toJson(t, ty.contents))->Json.array
  | (PDict(o), Dict(ty, _)) =>
    let r = Dict.empty()
    Array.forEachU(MapString.toArray(o), (. (k, v)) => Dict.set(r, k, toJson(v, ty.contents)))
    Json.object_(r)
  | (PDict(o), Record(ty)) => Json.object_(toJson_record(o, ty.contents))
  | (PDict(o), Union(k, {cases, _})) =>
    let tag = MapString.getExn(o, k)
    let recordTy = switch (cases, tag) {
    | (Int(m), PConst(PInt(i), _)) => MapInt.getExn(m, i).contents
    | (String(m), PConst(PString(s), _)) => MapString.getExn(m, s).contents
    | _ => assert false
    }
    let tag = switch tag {
    | PConst(PString(s), _) => Json.string(s)
    | PConst(PInt(0), Extra_boolean) => Json.boolean(false)
    | PConst(PInt(_), Extra_boolean) => Json.boolean(true)
    | PConst(PInt(i), Extra_none) => Json.number(Int.toFloat(i))
    | _ => assert false
    }
    let jsobj = toJson_record(o, recordTy)
    Dict.set(jsobj, k, tag)
    Json.object_(jsobj)
  | _ => assert false
  }

let toJson = toJson_record
