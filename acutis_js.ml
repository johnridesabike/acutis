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

(** Types for the exported JavaScript interface. *)

class type render = object
  method apply :
    Decode_js.t -> Js.js_string Js.t Js.opt Js_promise.t Js.t Js.meth
end

class type printjs = object
  method toESMString : Js.js_string Js.t Js.meth
  method toCJSString : Js.js_string Js.t Js.meth
end

class type ['func, 'result] compiler = object ('self)
  method addString :
    Js.js_string Js.t -> Js.js_string Js.t -> 'self Js.t Js.meth

  method addUint8Array :
    Js.js_string Js.t -> Typed_array.uint8Array Js.t -> 'self Js.t Js.meth

  method addFunc :
    Js.js_string Js.t -> Decode_js.t -> 'func -> 'self Js.t Js.meth

  method compileString :
    Js.js_string Js.t -> Js.js_string Js.t -> 'result Js.opt Js.meth

  method compileUint8Array :
    Js.js_string Js.t -> Typed_array.uint8Array Js.t -> 'result Js.opt Js.meth
end

class type compilers = object
  method createRender :
    ( (Decode_js.t -> Js.js_string Js.t Js_promise.t Js.t) Js.callback,
      render Js.t )
    compiler
    Js.t
    Js.meth

  method createPrintJS : (Js.js_string Js.t, printjs Js.t) compiler Js.t Js.meth
  method debugInterface : Decode_js.t -> Js.js_string Js.t Js.opt Js.meth
end

class type options = object
  method escape : 'a Js.t Js.optdef_prop
  (** Single-character keys with their string replacements. *)

  method onMessage : (Js.js_string Js.t -> unit) Js.callback Js.optdef_prop
  method onError : (unit -> unit) Js.callback Js.optdef_prop
end

class type export = object
  method _Acutis : options Js.t -> compilers Js.t Js.meth
end

(** A functor that builds the interface from given options. *)

module Make (M : sig
  val config : options Js.t
end) : sig
  val compilers : compilers Js.t
end = struct
  module Config = struct
    let escape =
      Js.Optdef.case M.config##.escape
        (fun () -> Acutis.Config_default.escape)
        (fun obj ->
          Seq.filter_map
            (fun (k, v) ->
              match (String.length k, Decode_js.get_string v) with
              | 1, Some v -> Some (k.[0], v)
              | _, Some _ | _, None -> None)
            (Decode_js.assoc_to_seq obj))
  end

  module Acutis_js = Acutis.Of_decodable (Config) (Decode_js)
  module Print_js = Acutis.Print_js (Config)

  let msg_to_js msg =
    Format.asprintf "%a" Acutis.pp_message msg |> Js.string |> Js.Unsafe.coerce

  let on_message =
    Js.Optdef.case M.config##.onMessage
      (fun () msg -> Console.console##warn (msg_to_js msg))
      (fun f msg -> Js.Unsafe.fun_call f [| msg_to_js msg |])

  let on_error : unit -> unit =
    Js.Optdef.case M.config##.onError
      (fun () () ->
        new%js Js.error_constr
          (Js.string "There was an error while processing an Acutis template.")
        |> Js.Js_error.of_error |> Js.Js_error.raise_)
      (fun f () -> Js.Unsafe.fun_call f [||])

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

  (** In case [on_error] does not throw an exception, return [null]. *)
  let handle_messages (msgs, result) =
    List.iter on_message msgs;
    match result with None -> on_error (); Js.null | Some x -> Js.Opt.return x

  let ( let* ) = Js.Opt.bind

  let add_comp_aux queue fname lexbuf =
    let fname = Js.to_string fname in
    Lexing.set_filename lexbuf fname;
    Js.Opt.iter
      (Acutis.parse lexbuf |> handle_messages)
      (fun comp ->
        Queue.add (Acutis.comp_of_parsed (fname_to_compname fname) comp) queue)

  let compile_aux handle_result components fname lexbuf =
    Js.to_string fname |> Lexing.set_filename lexbuf;
    let* components =
      Queue.to_seq components |> Acutis.comps_compile |> handle_messages
    in
    let* parsed = Acutis.parse lexbuf |> handle_messages in
    let* result = Acutis.compile components parsed |> handle_messages in
    handle_result result |> Js.Opt.return

  let compiler ~comp_of_func ~handle_result =
    let comp_queue = Queue.create () in
    object%js (_self)
      method addString fname src =
        add_comp_aux comp_queue fname (Js.to_string src |> Lexing.from_string);
        _self

      method addUint8Array fname src =
        add_comp_aux comp_queue fname
          (input_uint8Array src |> Lexing.from_function);
        _self

      method addFunc name ty func =
        let name = Js.to_string name in
        Js.Opt.iter
          (Acutis_js.interface ty |> handle_messages)
          (fun interface ->
            Queue.add
              (comp_of_func name func |> Acutis.comp_of_fun name interface)
              comp_queue);
        _self

      method compileString fname src =
        compile_aux handle_result comp_queue fname
          (Js.to_string src |> Lexing.from_string)

      method compileUint8Array fname src =
        compile_aux handle_result comp_queue fname
          (input_uint8Array src |> Lexing.from_function)
    end

  let compilers =
    object%js
      method createRender =
        compiler
          ~comp_of_func:(fun _name f data ->
            match Js.Unsafe.fun_call f [| data |] |> Js_promise.classify with
            | Either.Left p -> Js_promise.await p |> Js.to_string
            | Either.Right s -> Js.to_string s)
          ~handle_result:(fun compiled ->
            object%js
              method apply data =
                Js_promise.run @@ fun () ->
                match Acutis_js.apply compiled data with
                | Ok x -> Js.string x |> Js.Opt.return
                | Error x -> handle_messages (x, None)
            end)

      method createPrintJS =
        compiler
          ~comp_of_func:(fun function_path module_path ->
            let module_path = Js.to_string module_path in
            Acutis.js_import ~module_path ~function_path)
          ~handle_result:(fun x ->
            object%js
              method toESMString =
                Format.asprintf "%a" Print_js.esm x |> Js.string

              method toCJSString =
                Format.asprintf "%a" Print_js.cjs x |> Js.string
            end)

      method debugInterface js =
        let* interface = Acutis_js.interface js |> handle_messages in
        Format.asprintf "%a" Acutis.pp_interface interface
        |> Js.string |> Js.Opt.return
    end
end

let export : export Js.t =
  object%js
    method _Acutis config =
      let module M = Make (struct
        let config = config
      end) in
      M.compilers
  end

let () = Js.export_all export
