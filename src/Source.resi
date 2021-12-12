/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module type Env = {
  type t
  type e
  let return: (. string) => t
  let error: (. string) => t
  let error_internal: (. array<Debug.t>) => t
  let render: (. Belt.MutableQueue.t<t>) => t
  let try_: (. (. unit) => t, (. e) => t) => t
  let map: (. t, string => string) => t
  let flatmap: (. t, string => t) => t
}

type env<'a> = module(Env with type t = 'a)

type fnU<'a> = (. env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type fn<'a> = (env<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a

type t<'a, 'b> =
  | Acutis(string, 'a)
  | Function(string, Typescheme.props, Typescheme.Child.props, 'b)

let name: t<_, _> => string

let src: (~name: string, 'a) => t<'a, 'b>
let fnU: (~name: string, Typescheme.props, Typescheme.Child.props, fnU<'b>) => t<'a, fnU<'b>>
// fn is not a reserved word in JavaScript, ReScript, or OCaml.
let fn: (~name: string, Typescheme.props, Typescheme.Child.props, fn<'b>) => t<'a, fnU<'b>>
