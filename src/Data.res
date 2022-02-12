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
      | (i, Ty.Enum.Extra_none) => Int.toString(i)
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
  | PConst(Const.t, Ty.Enum.extra)

module Stack = {
  let nullable = "nullable"
  let array = "array" // add index later?
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
      l := PArray([make(x, ty, ~stack), l.contents])
    }
    l.contents
  | None => raise(Exit(Debug.decodeError(Ty.list(ty), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and dict = (~stack, j, ty) =>
  switch Json.decodeObject(j) {
  | Some(obj) =>
    let keys = Dict.entries(obj)
    let dict = Array.mapU(keys, (. (k, v)) => (k, make(v, ty, ~stack)))
    PDict(MapString.fromArray(dict))
  | None => raise(Exit(Debug.decodeError(Ty.dict(ty), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and tuple = (~stack, j, tys) =>
  switch Json.decodeArray(j) {
  | Some(arr) =>
    PArray(Array.zipByU(tys, arr, (. ty, j) => make(j, ty, ~stack=list{Stack.array, ...stack})))
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
and record = (~stack, j, tys) =>
  switch Json.decodeObject(j) {
  | Some(obj) => PDict(recordAux(obj, tys.contents, ~stack))
  | _ => raise(Exit(Debug.decodeError(Ty.record2(tys), j, Ty.toString, ~stack)))
  }

@raises(Exit)
and make = (~stack, j, ty) =>
  switch ty.contents {
  | Ty.Unknown => PUnknown(j)
  | Nullable(ty) => nullable(~stack, j, ty)
  | Enum({extra: Extra_boolean, cases: Enum_int(cases), _}) => boolean(~stack, j, ty, cases)
  | String => string(~stack, j, ty, None)
  | Enum({cases: Enum_string(cases), _}) => string(~stack, j, ty, Some(cases))
  | Int => int(~stack, j, ty, None)
  | Enum({cases: Enum_int(cases), _}) => int(~stack, j, ty, Some(cases))
  | Float => float(~stack, j)
  | Echo => echo(~stack, j)
  | List(ty) => list(~stack, j, ty)
  | Dict(ty, _) => dict(~stack, j, ty)
  | Tuple(tys) => tuple(~stack, j, tys)
  | Record(tys) => record(~stack, j, tys)
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
  | TRecord(_, x, _) | TDict(_, x, _) => PDict(MapString.mapU(x, (. v) => fromPattern(v, props)))
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
  | (PUnknown(j), Ty.Unknown) => j
  | (PConst(PInt(0), Extra_boolean), Ty.Enum({extra: Extra_boolean, _})) => Json.boolean(false)
  | (PConst(PInt(_), Extra_boolean), Ty.Enum({extra: Extra_boolean, _})) => Json.boolean(true)
  | (PConst(PInt(i), _), Int | Echo) => Json.number(Int.toFloat(i))
  | (PConst(PFloat(f), _), Float | Echo) => Json.number(f)
  | (PConst(PString(s), _), String | Echo) => Json.string(s)
  | (PNull, Nullable(_)) => Json.null
  | (PArray([t]), Nullable(ty)) => toJson(t, ty.contents)
  | (_, List(ty)) => toJson_list(t, ty.contents)
  | (PArray(a), Tuple(tys)) =>
    assert (Array.size(a) == Array.size(tys))
    Array.zipByU(a, tys, (. t, ty) => toJson(t, ty.contents))->Json.array
  | (PDict(o), Dict(ty, _)) =>
    let r = Dict.empty()
    Array.forEachU(MapString.toArray(o), (. (k, v)) => Dict.set(r, k, toJson(v, ty.contents)))
    Json.object_(r)
  | (PDict(o), Record(ty)) => Json.object_(toJson_record(o, ty.contents))
  | _ => assert false
  }

let toJson = toJson_record
