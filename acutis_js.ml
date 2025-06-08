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

module Decode_js = struct
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

  let array_to_seq a =
    let l = a##.length in
    let rec aux i () =
      if i = l then Seq.Nil else Seq.Cons (Js.Unsafe.get a i, aux (succ i))
    in
    aux 0

  let get_seq j =
    if Js.Unsafe.global##._Array##isArray j then Some (array_to_seq (coerce j))
    else None

  let get_assoc j =
    match Js.to_string (Js.typeof j) with
    | "object" ->
        if Js.Unsafe.global##._Array##isArray j then None
        else Js.Opt.to_option (Js.Opt.return j)
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

  let marshal = Js.Unsafe.inject
end

module Js_promise : sig
  type 'a t

  val classify : 'a Js.t -> ('b Js.t t Js.t, 'a Js.t) Either.t
  val await : 'a t Js.t -> 'a
  val run : (unit -> 'a) -> 'a t Js.t
end = struct
  type 'a t

  let classify j =
    match Decode_js.get_assoc j with
    | Some j -> (
        match Js.Unsafe.get j "then" |> Js.typeof |> Js.to_string with
        | "function" -> Either.Left (Js.Unsafe.coerce j)
        | _ -> Either.Right j)
    | None -> Either.Right j

  type _ Effect.t += Await : 'a t Js.t -> 'a Effect.t

  let await p = Effect.perform (Await p)

  let run f =
    match f () with
    | x -> Js.Unsafe.global##._Promise##resolve x
    | exception x -> Js.Unsafe.global##._Promise##reject x
    | effect Await p, k ->
        Js.Unsafe.meth_call p "then"
          [|
            Js.Unsafe.inject (Js.wrap_callback (Effect.Deep.continue k));
            Js.Unsafe.inject (Js.wrap_callback (Effect.Deep.discontinue k));
          |]
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

module Acutis_js = Acutis.Of_decodable (Decode_js)

let bind (msgs, x) f =
  match x with
  | None -> (msgs, None)
  | Some x ->
      let msgs', x = f x in
      (Seq.append msgs msgs', x)

type 'a export_result =
  < errors : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  ; result : 'a Js.opt Js.readonly_prop >
  Js.t

let export_result_aux : string Seq.t -> 'a option -> 'a export_result =
 fun msgs result ->
  let arr = new%js Js.array_empty in
  Seq.iter (fun msg -> arr##push (Js.string msg) |> ignore) msgs;
  object%js
    val errors = arr
    val result = Js.Opt.option result
  end

let export_result msgs result =
  export_result_aux
    (Seq.map (Format.asprintf "%a" Acutis.pp_message) msgs)
    result

let export_result_tuple (msgs, result) = export_result msgs result

let () =
  Js.export "Component"
    (object%js
       method string fname src =
         let fname = Js.to_string fname in
         let lexbuf = Js.to_string src |> Lexing.from_string in
         Lexing.set_filename lexbuf fname;
         let msgs, parsed = Acutis.parse lexbuf in
         export_result msgs
           (Option.map
              (Acutis.comp_of_parsed ~name:(fname_to_compname fname))
              parsed)

       method uint8Array fname src =
         let fname = Js.to_string fname in
         let lexbuf = input_uint8Array src |> Lexing.from_function in
         Lexing.set_filename lexbuf fname;
         let msgs, parsed = Acutis.parse lexbuf in
         export_result msgs
           (Option.map
              (Acutis.comp_of_parsed ~name:(fname_to_compname fname))
              parsed)

       method func name ty f =
         let msgs, interface = Acutis_js.interface ty in
         export_result msgs
           (Option.map
              (fun interface ->
                Acutis.comp_of_fun ~name:(Js.to_string name) interface
                  (fun data ->
                    match
                      Js.Unsafe.fun_call f [| data |] |> Js_promise.classify
                    with
                    | Either.Left p -> Js_promise.await p |> Js.to_string
                    | Either.Right s -> Js.to_string s))
              interface)

       method funcPath module_path name ty =
         let function_path = Js.to_string name in
         let module_path = Js.to_string module_path in
         let msgs, interface = Acutis_js.interface ty in
         export_result msgs
           (Option.map
              (fun interface ->
                Acutis.comp_of_fun ~name:function_path interface
                  (Acutis.js_import ~module_path ~function_path))
              interface)
    end)

let () =
  Js.export "Compile"
    (object%js
       method components a =
         Decode_js.array_to_seq a |> Acutis.comps_compile |> export_result_tuple

       method string fname components src =
         let lexbuf = Js.to_string src |> Lexing.from_string in
         Lexing.set_filename lexbuf (Js.to_string fname);
         bind (Acutis.parse lexbuf) (Acutis.compile components)
         |> export_result_tuple

       method uint8Array fname components src =
         let lexbuf = input_uint8Array src |> Lexing.from_function in
         Lexing.set_filename lexbuf (Js.to_string fname);
         bind (Acutis.parse lexbuf) (Acutis.compile components)
         |> export_result_tuple

       method render template js =
         Js_promise.run @@ fun () ->
         match Acutis_js.apply template js with
         | Ok x -> export_result_aux Seq.empty (Some x)
         | Error x -> export_result_aux (Seq.return x) None

       method toESMString x = Format.asprintf "%a" Acutis.esm x |> Js.string
       method toCJSString x = Format.asprintf "%a" Acutis.cjs x |> Js.string
    end)

let () =
  Js.export "Utils"
    (object%js
       method debugInterface js =
         let msgs, interface = Acutis_js.interface js in
         export_result msgs
           (Option.map
              (fun interface ->
                Format.asprintf "%a" Acutis.pp_interface interface |> Js.string)
              interface)
    end)
