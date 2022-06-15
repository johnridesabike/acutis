(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml
open Acutis
module RenderJs = Render.Make (Sync) (DataJs)

let _ =
  Js.export_all
    (object%js
       method lmao src =
         Compile.make ~filename:"test" Compile.Components.empty
           (Js.to_string src)

       method lol template json = RenderJs.make template json
    end)
