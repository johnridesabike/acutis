(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)
open StdlibExtra

module type Env = sig
  type t

  val return : string -> t
  val render : t Queue.t -> t
end

type 'a env = (module Env with type t = 'a)

module type Data = sig
  type t

  val decode : Typescheme.t -> t -> t Data.t MapString.t
  val encode : Typescheme.t -> t Data.t MapString.t -> t
end

type 'a data = (module Data with type t = 'a)

type ('a, 'b) t =
  | Acutis of string * 'a
  | Function of string * Typescheme.t * Typescheme.Child.t * 'b

let src ~name src = Acutis (name, src)
let fn ~name props children f = Function (name, props, children, f)
