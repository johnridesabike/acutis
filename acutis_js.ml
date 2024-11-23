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

module DecodeJs = struct
  type t = Js.Unsafe.any

  type 'a assoc = Js.Unsafe.any
  (** When we convert JavaScript objects into Acutis records, we cannot convert
      every value at once, e.g. by mapping the result of [Js.object_keys].

      JavaScript objects can cause side-effects when you access values. By only
      accessing the precise values that we need, we avoid triggering unexpected
      behavior. *)

  let coerce = Js.Unsafe.coerce

  let get_int j =
    match Js.to_string (Js.typeof j) with
    | "number" ->
        let n = coerce j |> Js.float_of_number in
        if Float.is_integer n then Some (Float.to_int n) else None
    | _ -> None

  let get_string j =
    match Js.to_string (Js.typeof j) with
    | "string" -> Some (coerce j |> Js.to_string)
    | _ -> None

  let get_float j =
    match Js.to_string (Js.typeof j) with
    | "number" -> Some (coerce j |> Js.float_of_number)
    | _ -> None

  let get_bool j =
    match Js.to_string (Js.typeof j) with
    | "boolean" -> Some (coerce j |> Js.to_bool)
    | _ -> None

  let get_some j =
    match Js.to_string (Js.typeof j) with
    | "undefined" -> None
    | _ -> Js.Opt.to_option (Js.Opt.return j)

  let get_seq j =
    if Js.Unsafe.global##._Array##isArray j then
      let a = coerce j in
      let rec aux i () =
        Js.Optdef.case (Js.array_get a i)
          (fun () -> Seq.Nil)
          (fun x -> Seq.Cons (x, aux (succ i)))
      in
      Some (aux 0)
    else None

  let get_assoc j =
    match Js.to_string (Js.typeof j) with
    | "object" -> Js.Opt.to_option (Js.Opt.return j)
    | _ -> None

  let assoc_find : string -> 'a assoc -> 'a =
   fun k m -> Js.Unsafe.get m (Js.string k)

  let assoc_mem : string -> 'a assoc -> bool =
   fun k m -> Js.Unsafe.global##._Object##hasOwn m (Js.string k) |> Js.to_bool

  let assoc_to_seq m =
    let keys = Js.object_keys m in
    let rec aux i () =
      Js.Optdef.case (Js.array_get keys i)
        (fun () -> Seq.Nil)
        (fun k -> Seq.Cons ((Js.to_string k, Js.Unsafe.get m k), aux (succ i)))
    in
    aux 0

  let null = Js.Unsafe.inject Js.null
  let some = Fun.id
  let of_float x = Js.number_of_float x |> coerce
  let of_string x = Js.string x |> coerce
  let of_bool x = Js.bool x |> coerce
  let of_int x = Float.of_int x |> Js.number_of_float |> coerce
  let of_seq x = Array.of_seq x |> Js.array |> coerce
  let of_seq_assoc x = Array.of_seq x |> Js.Unsafe.obj

  let to_string j =
    Js.Unsafe.fun_call Js.Unsafe.global##._String [| j |] |> Js.to_string
end

module Promise = struct
  type 'a t

  let return : 'a -> 'a t = fun x -> Js.Unsafe.global##._Promise##resolve x
  let error : exn -> 'a t = fun x -> Js.Unsafe.global##._Promise##reject x

  let then_ : 'a t -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t =
   fun t resolve reject ->
    Js.Unsafe.meth_call t "then"
      [|
        Js.Unsafe.inject (Js.wrap_callback resolve);
        Js.Unsafe.inject (Js.wrap_callback reject);
      |]

  module E = Effect
  module ED = Effect.Deep

  type _ E.t += Await : 'a t -> 'a E.t

  let await p = E.perform (Await p)

  let effc : type a. a E.t -> ((a, 'b t) ED.continuation -> 'b t) option =
    function
    | Await p -> Some (fun k -> then_ p (ED.continue k) (ED.discontinue k))
    | _ -> None

  let handle_await f = ED.try_with f () { effc }
end

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let input_uint8Array arr =
  let src = Typed_array.Bytes.of_uint8Array arr in
  let length = Bytes.length src in
  let offset = ref 0 in
  fun dst out_len ->
    let out_len = min (length - !offset) out_len in
    Bytes.blit src !offset dst 0 out_len;
    offset := !offset + out_len;
    out_len

let () =
  Js.export "Component"
    (object%js
       method string fname src =
         let fname = Js.to_string fname in
         Acutis.comp_parse ~fname ~name:(fname_to_compname fname)
           (Js.to_string src |> Lexing.from_string)

       method uint8Array fname src =
         let fname = Js.to_string fname in
         Acutis.comp_parse ~fname ~name:(fname_to_compname fname)
           (input_uint8Array src |> Lexing.from_function)

       method funAsync name ty fn =
         let fn : DecodeJs.t -> string Promise.t =
          fun data ->
           Js.Unsafe.fun_call fn [| data |]
           |> Promise.await |> Js.to_string |> Promise.return
         in
         Acutis.comp_fun ~name:(Js.to_string name) ty fn

       method funSync name ty fn =
         let fn : DecodeJs.t -> string =
          fun data -> Js.Unsafe.fun_call fn [| data |] |> Js.to_string
         in
         Acutis.comp_fun ~name:(Js.to_string name) ty fn

       method funPath module_path name ty =
         let function_path = Js.to_string name in
         let module_path = Js.to_string module_path in
         Acutis.comp_fun ~name:function_path ty
           (Acutis.js_import ~module_path ~function_path)
    end)

let () =
  Js.export "Compile"
    (object%js
       method components a =
         Js.to_array a |> Array.to_seq |> Acutis.comps_compile

       method string fname components src =
         Acutis.parse ~fname:(Js.to_string fname)
           (Js.to_string src |> Lexing.from_string)
         |> Acutis.compile components

       method uint8Array fname components src =
         Acutis.parse ~fname:(Js.to_string fname)
           (input_uint8Array src |> Lexing.from_function)
         |> Acutis.compile components

       method toESMString x =
         Acutis.esm Format.str_formatter x;
         Format.flush_str_formatter () |> Js.string

       method toCJSString x =
         Acutis.cjs Format.str_formatter x;
         Format.flush_str_formatter () |> Js.string
    end)

let render_sync = Acutis.render_string (module DecodeJs)

let render_async =
  let module M = Acutis.Render (Promise) (DecodeJs) in
  M.apply

let () =
  Js.export "Render"
    (object%js
       method async template js =
         Promise.handle_await @@ fun () ->
         render_async template js |> Promise.await |> Js.string
         |> Promise.return

       method sync template js = render_sync template js |> Js.string
    end)

let () =
  let array_to_2tuple a =
    let a = Js.to_array a in
    (a.(0), Obj.magic a.(1))
  in
  let key_values v =
    Js.to_array v |> Array.to_seq
    |> Seq.map (fun a ->
           let k, v = array_to_2tuple a in
           let k = Js.to_string k in
           (k, v))
  in
  let int_of_number x = Js.float_of_number x |> int_of_float in
  Js.export "Typescheme"
    (object%js
       val variantOpen = `Open
       val variantClosed = `Closed
       val empty = Acutis.typescheme_empty
       method make a = key_values a |> Acutis.typescheme
       method unknown = Acutis.unknown ()
       method int = Acutis.int ()
       method float = Acutis.float ()
       method string = Acutis.string ()
       method nullable t = Acutis.nullable t
       method list t = Acutis.list t
       method tuple a = Js.to_array a |> Array.to_seq |> Acutis.tuple

       method record a =
         Js.to_array a |> Array.to_seq
         |> Seq.map (fun a ->
                let k, v = array_to_2tuple a in
                (Js.to_string k, v))
         |> Acutis.record

       method dict t = Acutis.dict t

       method enumInt r a =
         Js.to_array a |> Array.to_seq |> Seq.map int_of_number
         |> Acutis.enum_int r

       method enumString r a =
         Js.to_array a |> Array.to_seq |> Seq.map Js.to_string
         |> Acutis.enum_string r

       method boolean = Acutis.boolean ()
       method falseOnly = Acutis.false_only ()
       method trueOnly = Acutis.true_only ()

       method unionInt r k a =
         Js.to_array a |> Array.to_seq
         |> Seq.map (fun a ->
                let i, v = array_to_2tuple a in
                (int_of_number i, key_values v))
         |> Acutis.union_int r (Js.to_string k)

       method unionString r k a =
         Js.to_array a |> Array.to_seq
         |> Seq.map (fun a ->
                let s, v = array_to_2tuple a in
                (Js.to_string s, key_values v))
         |> Acutis.union_string r (Js.to_string k)

       method unionBoolean k f t =
         let f = key_values f in
         let t = key_values t in
         Acutis.union_boolean (Js.to_string k) ~f ~t

       method unionTrueOnly k t =
         Acutis.union_true_only (Js.to_string k) (key_values t)

       method unionFalseOnly k f =
         Acutis.union_false_only (Js.to_string k) (key_values f)
    end)

let () =
  Js.export "Utils"
    (object%js
       method isError e =
         match e with Acutis.Acutis_error _ -> true | _ -> false

       method getError e =
         match e with
         | Acutis.Acutis_error e ->
             Js.Opt.return (Js.string (Format.asprintf "%a" Acutis.pp_error e))
         | _ -> Js.Opt.empty
    end)
