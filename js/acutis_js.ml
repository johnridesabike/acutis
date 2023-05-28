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

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let () =
  Js.export "Component"
    (object%js
       method string fname src =
         let fname = Js.to_string fname in
         Compile.Components.parse_string ~fname ~name:(fname_to_compname fname)
           (Js.to_string src)

       method uint8Array fname src =
         let fname = Js.to_string fname in
         Compile.Components.parse_string ~fname ~name:(fname_to_compname fname)
           (Typed_array.String.of_uint8Array src)

       method funAsync name ty fn =
         let fn : RenderAsync.data -> RenderAsync.t =
          fun data ->
           Js.Unsafe.fun_call fn [| data |] |> Promise.map Js.to_string
         in
         Compile.Components.from_fun ~name:(Js.to_string name) ty fn

       method funSync name ty fn =
         let fn : RenderSync.data -> RenderSync.t =
          fun data -> Js.Unsafe.fun_call fn [| data |] |> Js.to_string
         in
         Compile.Components.from_fun ~name:(Js.to_string name) ty fn

       method funPath module_path name ty =
         let function_path = Js.to_string name in
         let module_path = Js.to_string module_path in
         Compile.Components.from_fun ~name:function_path ty
           (PrintJs.jsfun ~module_path ~function_path)
    end)

let () =
  Js.export "Compile"
    (object%js
       method components a =
         Js.to_array a |> Array.to_seq |> Compile.Components.of_seq

       method string fname components src =
         Compile.from_string ~fname:(Js.to_string fname) components
           (Js.to_string src)

       method uint8Array fname components src =
         Compile.from_string ~fname:(Js.to_string fname) components
           (Typed_array.String.of_uint8Array src)

       method toJSString x =
         PrintJs.esm Format.str_formatter x;
         Format.flush_str_formatter () |> Js.string

       method toCJSString x =
         PrintJs.cjs Format.str_formatter x;
         Format.flush_str_formatter () |> Js.string
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
    Js.to_array v
    |> Array.map (fun a ->
           let k, v = array_to_2tuple a in
           let k = Js.to_string k in
           (k, v))
    |> Array.to_list
  in
  let int_of_number x = Js.float_of_number x |> int_of_float in
  Js.export "Typescheme"
    (object%js
       val variantOpen = `Open
       val variantClosed = `Closed
       val empty = empty
       method make a = key_values a |> make
       method unknown = unknown ()
       method int = int ()
       method float = float ()
       method string = string ()
       method nullable t = nullable t
       method list t = list t
       method tuple a = Js.to_array a |> Array.to_list |> tuple

       method record a =
         Js.to_array a
         |> Array.map (fun a ->
                let k, v = array_to_2tuple a in
                (Js.to_string k, v))
         |> Array.to_list |> record

       method dict t = dict t

       method enumInt r a =
         Js.to_array a |> Array.map int_of_number |> Array.to_list |> enum_int r

       method enumString r a =
         Js.to_array a |> Array.map Js.to_string |> Array.to_list
         |> enum_string r

       method boolean = boolean ()
       method falseOnly = false_only ()
       method trueOnly = true_only ()

       method unionInt r k a =
         Js.to_array a
         |> Array.map (fun a ->
                let i, v = array_to_2tuple a in
                (int_of_number i, key_values v))
         |> Array.to_list
         |> union_int r (Js.to_string k)

       method unionString r k a =
         Js.to_array a
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
    end)

let () =
  Js.export "Utils"
    (object%js
       method isError e =
         match e with Error.Acutis_error _ -> true | _ -> false

       method getError e =
         match e with
         | Error.Acutis_error s -> Js.Opt.return (Js.string s)
         | _ -> Js.Opt.empty
    end)
