/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Queue = Belt.MutableQueue

module type Env = {
  type t
  type e
  let return: (. string) => t
  let return_: (. string) => t
  let error: (. string) => t
  let error_internal: (. array<Debug.error>) => t
  let render: (. Queue.t<t>) => t
  let try_: (. (. unit) => t, (. e) => t) => t
  let map: (. t, string => string) => t
  let flatmap: (. t, string => t) => t
}

type env<'a> = module(Env with type t = 'a)

type fnU<'a> = (. env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type fn<'a> = (env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type t<'a, 'b> =
  | Acutis(string, 'a)
  | Function(string, Typescheme.t, Typescheme.Child.t, 'b)

let src = (~name, src) => Acutis(name, src)

// this makes components render faster.
let uncurry = (f, . a, b, c) => f(a, b, c)

let fnU = (~name, props, children, f) => Function(name, props, children, f)

let fn = (~name, props, children, f) => Function(name, props, children, uncurry(f))
