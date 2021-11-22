/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type t

let make: (Belt.Map.String.t<TypeChecker.t>, Js.Dict.t<Js.Json.t>) => Js.Dict.t<t>

exception RenderError

let booleanExn: t => bool

let echoExn: t => string

let stringExn: t => string

let intExn: t => int

let floatExn: t => float

let tupleExn: t => array<t>

let dictExn: t => Js.Dict.t<t>

let isNull: t => bool

let nullableExn: t => option<t>

let fromPattern: (TypeChecker.Pattern.t, Js.Dict.t<t>) => t

let forEachListExn: (t, (. ~index: t, t) => unit) => unit

let forEachDictExn: (t, (. ~index: t, t) => unit) => unit
