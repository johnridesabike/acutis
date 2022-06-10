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

val make :
  ('result, 'data) Source.env ->
  ('data -> 'result MapString.t -> 'result) Compile.template Compile.t ->
  'data ->
  'result

module Json : Source.Env with type t = string and type data = DataYojson.t

val json :
  (Json.data -> Json.t MapString.t -> Json.t) Compile.template Compile.t ->
  Json.data ->
  Json.t
