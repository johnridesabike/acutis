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

module Promise_with_fixed_bind = struct
  type 'a t = 'a Promise.t

  let return = Promise.return
  let bind = Promise.Syntax.( let* )
end

module RenderSync = Render.Make (Sync) (DataJs)
module RenderAsync = Render.Make (Promise_with_fixed_bind) (DataJs)

let child_async (k, p) =
  let p = p |> Promise.map Js.string |> Js.Unsafe.inject in
  (k, p)

let child_sync (k, v) = (k, Js.Unsafe.coerce (Js.string v))

let map_to_js f m =
  m |> Map.String.to_seq |> Seq.map f |> Array.of_seq |> Js.Unsafe.obj

let () =
  Js.export "Compile"
    (object%js
       method src name src =
         Compile.Components.src ~name:(Js.to_string name) (Js.to_string src)

       method fnAsync name ty children fn =
         let fn : RenderAsync.component =
          fun data children ->
           Js.Unsafe.fun_call fn [| data; map_to_js child_async children |]
           |> Promise.map Js.to_string
         in
         Compile.Components.fn ~name:(Js.to_string name) ty children fn

       method fn name ty children fn =
         let fn : RenderSync.component =
          fun data children ->
           Js.Unsafe.fun_call fn [| data; map_to_js child_sync children |]
           |> Js.to_string
         in
         Compile.Components.fn ~name:(Js.to_string name) ty children fn

       method components a =
         a |> Js.to_array |> Array.to_list |> Compile.Components.make

       method make fname components src =
         Compile.make ~filename:(Js.to_string fname) components
           (Js.to_string src)
    end)

let () =
  Js.export "Render"
    (object%js
       method async template js =
         RenderAsync.make template js |> Promise.map Js.string

       method sync template js = RenderSync.make template js |> Js.string
    end)

let () =
  let open Typescheme in
  let array_to_2tuple a =
    let a = Js.to_array a in
    (a.(0), Obj.magic a.(1))
  in
  let key_values v =
    v |> Js.to_array
    |> Array.map (fun a ->
           let k, v = array_to_2tuple a in
           let k = Js.to_string k in
           (k, v))
    |> Array.to_list
  in
  let int_of_number x = x |> Js.float_of_number |> int_of_float in
  Js.export "Typescheme"
    (object%js
       val variantOpen = `Open
       val variantClosed = `Closed
       val empty = empty
       method make a = a |> key_values |> make
       method unknown () = unknown ()
       method int () = int ()
       method float () = float ()
       method string () = string ()
       method echo () = echo ()
       method nullable t = nullable t
       method list t = list t
       method tuple a = a |> Js.to_array |> Array.to_list |> tuple

       method record a =
         a |> Js.to_array
         |> Array.map (fun a ->
                let k, v = array_to_2tuple a in
                (Js.to_string k, v))
         |> Array.to_list |> record

       method dict t = dict t

       method enumInt r a =
         a |> Js.to_array |> Array.map int_of_number |> Array.to_list
         |> enum_int r

       method enumString r a =
         a |> Js.to_array |> Array.map Js.to_string |> Array.to_list
         |> enum_string r

       method boolean () = boolean ()
       method falseOnly () = false_only ()
       method trueOnly () = true_only ()

       method unionInt r k a =
         a |> Js.to_array
         |> Array.map (fun a ->
                let i, v = array_to_2tuple a in
                (int_of_number i, key_values v))
         |> Array.to_list
         |> union_int r (Js.to_string k)

       method unionString r k a =
         a |> Js.to_array
         |> Array.map (fun a ->
                let s, v = array_to_2tuple a in
                (Js.to_string s, key_values v))
         |> Array.to_list
         |> union_string r (Js.to_string k)

       method unionBoolean k f t =
         let f = key_values f in
         let t = key_values t in
         union_boolean (Js.to_string k) ~f ~t

       method unionTrueOnly k t =
         union_true_only (Js.to_string k) (key_values t)

       method unionFalseOnly k f =
         union_false_only (Js.to_string k) (key_values f)
    end);
  let open Child in
  Js.export "TypeschemeChildren"
    (object%js
       val empty = empty
       method make a = a |> key_values |> make
       method child s = child (Js.to_string s)
       method nullable s = nullable (Js.to_string s)
    end)

let () =
  Js.export "Utils"
    (object%js
       method isError e = match e with Error.Error _ -> true | _ -> false

       method logError e =
         match e with Error.Error s -> print_endline s | _ -> ()
    end)
