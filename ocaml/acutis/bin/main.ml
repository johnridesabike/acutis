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
open StdlibExtra

module Promise_with_fixed_bind = struct
  type 'a t = 'a Promise.t

  let return = Promise.return
  let bind = Promise.Syntax.( let* )
end

module RenderJs = Render.Make (Promise_with_fixed_bind) (DataJs)

let map_to_js_aux (k, p) =
  let p = p |> Promise.map Js.string |> Js.Unsafe.inject in
  (k, p)

let map_to_js m =
  m |> MapString.to_seq |> Seq.map map_to_js_aux |> Array.of_seq
  |> Js.Unsafe.obj

let () =
  Js.export_all
    (object%js
       method src name src =
         Source.src ~name:(Js.to_string name) (Js.to_string src)

       method fn name ty children fn =
         let fn : RenderJs.component =
          fun data children ->
           Js.Unsafe.fun_call fn [| data; map_to_js children |]
         in
         Source.fn ~name:(Js.to_string name) ty children fn

       method components a =
         a |> Js.to_array |> Array.to_list |> Compile.Components.make

       method compile filename components src =
         Compile.make ~filename components (Js.to_string src)

       method render template js =
         RenderJs.make template js |> Promise.map Js.string
    end)
