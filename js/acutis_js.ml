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

module DecodeJs = struct
  type 'a linear = 'a Js.js_array Js.t

  let length a = a##.length
  let iteri f a = a##forEach (Js.wrap_callback (fun v i _ -> f i v))

  type 'a assoc = Js.Unsafe.any
  (** When we convert JavaScript objects into Acutis records, we cannot convert
      every value at once, e.g. by mapping the result of [Js.object_keys].

      JavaScript objects can cause side-effects when you access values. By only
      accessing the precise values that we need, we avoid triggering unexpected
      behavior. *)

  let assoc_find : string -> 'a assoc -> 'a =
   fun k m -> Js.Unsafe.get m (Js.string k)

  let assoc_mem : string -> 'a assoc -> bool =
   fun k m -> Js.Unsafe.global##._Object##hasOwn m (Js.string k) |> Js.to_bool

  let assoc_iter : (string -> 'a -> unit) -> 'a assoc -> unit =
   fun f m ->
    (Js.object_keys m)##forEach
      (Js.wrap_callback (fun k _ _ -> f (Js.to_string k) (Js.Unsafe.get m k)))

  type t = Js.Unsafe.any

  let coerce = Js.Unsafe.coerce
  let null = Js.Unsafe.inject Js.null
  let some = Fun.id
  let of_float x = Js.number_of_float x |> coerce
  let of_string x = Js.string x |> coerce
  let of_bool x = Js.bool x |> coerce
  let of_int x = Float.of_int x |> Js.number_of_float |> coerce
  let of_array x = Js.array x |> coerce
  let of_assoc x = Array.of_seq x |> Js.Unsafe.obj

  let decode_int j =
    match Js.to_string (Js.typeof j) with
    | "number" ->
        let n = coerce j |> Js.float_of_number in
        if Float.is_integer n then Some (Float.to_int n) else None
    | _ -> None

  let decode_string j =
    match Js.to_string (Js.typeof j) with
    | "string" -> Some (coerce j |> Js.to_string)
    | _ -> None

  let decode_float j =
    match Js.to_string (Js.typeof j) with
    | "number" -> Some (coerce j |> Js.float_of_number)
    | _ -> None

  let decode_bool j =
    match Js.to_string (Js.typeof j) with
    | "boolean" -> Some (coerce j |> Js.to_bool)
    | _ -> None

  let decode_linear j =
    if Js.Unsafe.global##._Array##isArray j then Some (coerce j) else None

  let decode_assoc j =
    match Js.to_string (Js.typeof j) with
    | "object" -> Js.Opt.to_option (Js.Opt.return j)
    | _ -> None

  let decode_some j =
    match Js.to_string (Js.typeof j) with
    | "undefined" -> None
    | _ -> Js.Opt.to_option (Js.Opt.return j)

  let to_string j =
    Js.Unsafe.fun_call Js.Unsafe.global##._String [| j |] |> Js.to_string
end

module Promise = struct
  type 'a t

  let return : 'a -> 'a t = fun x -> Js.Unsafe.global##._Promise##resolve x
  let error : 'e -> 'a t = fun x -> Js.Unsafe.global##._Promise##reject x

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun t f ->
    Js.Unsafe.meth_call t "then" [| Js.Unsafe.inject (Js.wrap_callback f) |]
end

module RenderSync = Render.MakeString (DecodeJs)
module RenderAsync = Render.Make (Promise) (DecodeJs)

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
           Promise.bind (Js.Unsafe.fun_call fn [| data |]) @@ fun s ->
           Promise.return (Js.to_string s)
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
           (PrintJs.import ~module_path ~function_path)
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
         Promise.bind (RenderAsync.eval template js) @@ fun s ->
         Promise.return (Js.string s)

       method sync template js = RenderSync.eval template js |> Js.string
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
