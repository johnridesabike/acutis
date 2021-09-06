/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
type t
exception DecodeError
let make: (TypeChecker.t, Js.Json.t) => t
exception RenderError
let booleanExn: t => bool
let stringExn: t => string
let intExn: t => int
let floatExn: t => float
let tupleExn: t => array<t>
let dictExn: t => Js.Dict.t<t>
let isNull: t => bool
