/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Const: {
  type t =
    | PInt(int)
    | PString(string)
    | PFloat(float)
  let fromTPat: Typechecker.Pattern.constant => t
  let toTPat: t => Typechecker.Pattern.constant
  let equal: (t, t) => bool
  let compare: (t, t) => int
}

type t

let make: (Js.Dict.t<Js.Json.t>, Typescheme.t) => Belt.Map.String.t<t>

let constantExn: t => Const.t

let tupleExn: t => array<t>

let dictExn: t => Belt.Map.String.t<t>

let isNull: t => bool

let nullableExn: t => option<t>

let fromPattern: (Typechecker.Pattern.t, Belt.Map.String.t<t>) => t

let forEachListExn: (t, (. ~index: t, t) => unit) => unit

let forEachDictExn: (t, (. ~index: t, t) => unit) => unit

let toString: t => string

let toJson: (Belt.Map.String.t<t>, Typescheme.t) => Js.Dict.t<Js.Json.t>
