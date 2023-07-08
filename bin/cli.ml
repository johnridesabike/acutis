(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Acutis

module DataJson = struct
  module Linear = List

  module Assoc = struct
    type 'a t = (string * 'a) list

    let fold f l init = List.fold_left (fun acc (k, v) -> f k v acc) init l
    let find_opt = List.assoc_opt
  end

  type t = Yojson.Basic.t

  let pp = Yojson.Basic.pretty_print ~std:false
  let classify = Fun.id
  let null = `Null
  let some = Fun.id
  let of_float x = `Float x
  let of_string x = `String x
  let of_bool x = `Bool x
  let of_int x = `Int x
  let of_seq x = `List (List.of_seq x)
  let of_map x = `Assoc (Map.String.bindings x)
end

module Render = Render.Make (Sync) (DataJson)

let usage_msg =
  {|Usage:
  acutis [OPTIONS...] [TEMPLATE] [COMPONENTS...]

Compile and render Acutis language templates.

Options:|}

type jstype = CommonJs | ESModule
type mode = Render | Make_js of jstype

let arg_mode = ref Render
let arg_data = ref "-"
let arg_output = ref "-"
let arg_version = ref false
let arg_printast = ref false
let arg_printtypes = ref false
let arg_printopt = ref false
let templates = Queue.create ()
let arg_funs = Queue.create ()

let args =
  [
    ( "--mode",
      Arg.Symbol
        ( [ "render"; "js"; "cjs" ],
          function
          | "js" -> arg_mode := Make_js ESModule
          | "cjs" -> arg_mode := Make_js CommonJs
          | _ -> arg_mode := Render ),
      " Either render the template, compile it to a JavaScript module, or \
       compile it to a CommonJS module. Default: render." );
    ( "--output",
      Arg.Set_string arg_output,
      " The path to write the output. Default: stdout." );
    ( "--data",
      Arg.Set_string arg_data,
      " The path to a JSON file to be used with --mode=render. Default: stdin."
    );
    ( "--fun",
      Arg.Tuple
        (let module_path = ref "" in
         let function_path = ref "" in
         let interface_path = ref "" in
         [
           Arg.Set_string module_path;
           Arg.Set_string function_path;
           Arg.Set_string interface_path;
           Arg.Unit
             (fun () ->
               Queue.add
                 (!module_path, !function_path, !interface_path)
                 arg_funs);
         ]),
      " Add an external JavaScript function as a component. This takes three \
       arguments: file path, function name, and type interface." );
    ("--version", Arg.Set arg_version, " Print the version number and exit.");
    ( "--printast",
      Arg.Set arg_printast,
      " Print the template's untyped AST form and exit." );
    ( "--printtypes",
      Arg.Set arg_printtypes,
      " Print the template's type interface and exit." );
    ( "--printopt",
      Arg.Set arg_printopt,
      " Print the template's optimized form and exit." );
  ]

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let make_components_aux () =
  Queue.to_seq templates
  |> Seq.map @@ fun fname ->
     In_channel.with_open_text fname
     @@ Compile.Components.parse_channel ~fname ~name:(fname_to_compname fname)

let make_components () = make_components_aux () |> Compile.Components.of_seq

let make_components_js () =
  let l = make_components_aux () in
  let funl =
    Queue.to_seq arg_funs
    |> Seq.map @@ fun (module_path, function_path, interface) ->
       let typescheme = Compile.interface_from_string ~fname:"-" interface in
       let name = fname_to_compname function_path in
       Compile.Components.from_fun ~name typescheme
         (PrintJs.jsfun ~module_path ~function_path)
  in
  Seq.append l funl |> Compile.Components.of_seq

let () =
  try
    Arg.parse (Arg.align args)
      (fun fname -> Queue.add fname templates)
      usage_msg;
    if !arg_version then
      Format.printf "Version: %s\n"
        (match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v)
    else
      let fname = Queue.take templates in
      if !arg_printast then
        In_channel.with_open_text fname @@ fun chan ->
        Compile.parse ~fname @@ Lexing.from_channel chan
        |> Ast.to_sexp
        |> Sexp.pp Format.std_formatter
      else if !arg_printtypes then
        let components = make_components_js () in
        let template =
          In_channel.with_open_text fname
          @@ Compile.from_channel ~fname components
        in
        Typescheme.pp_interface Format.std_formatter template.types
      else if !arg_printopt then
        let components = make_components_js () in
        let template =
          In_channel.with_open_text fname
          @@ Compile.from_channel ~fname components
        in
        Compile.to_sexp template.nodes |> Sexp.pp Format.std_formatter
      else
        match !arg_mode with
        | Render -> (
            let components = make_components () in
            let data =
              match !arg_data with
              | "-" ->
                  if Unix.isatty Unix.stdin then
                    print_endline "Enter JSON data:";
                  Yojson.Basic.from_channel stdin
              | fname ->
                  In_channel.with_open_text fname
                  @@ Yojson.Basic.from_channel ~fname
            in
            let template =
              In_channel.with_open_text fname
              @@ Compile.from_channel ~fname components
            in
            let result = Render.eval template data in
            match !arg_output with
            | "-" -> Out_channel.output_string stdout result
            | fname ->
                Out_channel.with_open_text fname @@ fun chan ->
                Out_channel.output_string chan result)
        | Make_js ty -> (
            let printer =
              match ty with CommonJs -> PrintJs.cjs | ESModule -> PrintJs.esm
            in
            let components = make_components_js () in
            let template =
              In_channel.with_open_text fname
              @@ Compile.from_channel ~fname components
            in
            match !arg_output with
            | "-" -> printer Format.std_formatter template
            | fname ->
                Out_channel.with_open_text fname @@ fun chan ->
                printer (Format.formatter_of_out_channel chan) template)
  with
  | Error.Acutis_error e ->
      Out_channel.output_string stderr e;
      exit 1
  | Yojson.Json_error s ->
      Format.eprintf "@[<v>Error decoding JSON input.@,%s@,@]" s;
      exit 1
  | Queue.Empty ->
      Out_channel.output_string stderr "You need to provide a template.\n\n";
      Out_channel.output_string stderr
      @@ Arg.usage_string (Arg.align args) usage_msg;
      exit 1
  | Sys_error s ->
      Format.eprintf "@[<v>System error:@,%s@,@]" s;
      exit 1
