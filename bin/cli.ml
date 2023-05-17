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
module Render = Render.Make (Sync) (DataJson)

let usage_msg =
  {|Parse and execute Acutis language templates.

Usage:
  acutis [options] [template] [...templates]

Options:|}

type action = Render | Print_ast | Print_types | Print_optimized | Make_js

let doc_data = " The path to a JSON data file. Default: stdin."
let arg_data = ref "-"
let doc_output = " The path to write the output. Default: stdout."
let arg_output = ref "-"
let templates = Queue.create ()
let set_templates fname = Queue.add fname templates
let doc_version = " Show the version number and exit."
let doc_printast = " Print the template's untyped AST form and exit."
let doc_printtypes = " Print the template's type interface and exit."
let doc_printopt = " Print the template's optimized form and exit."
let arg_action = ref Render
let jsmodules = Queue.create ()
let set_printast () = arg_action := Print_ast
let set_printtypes () = arg_action := Print_types
let set_printopt () = arg_action := Print_optimized
let set_buildjs () = arg_action := Make_js

let version () =
  Format.printf "Version: %s\n"
    (match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v);
  exit 0

let args =
  Arg.align
    [
      ("--data", Set_string arg_data, doc_data);
      ("--output", Set_string arg_output, doc_output);
      ("--js", Unit set_buildjs, "TODO");
      ( "--jsmodule",
        Tuple
          (let module_path = ref "" in
           let function_path = ref "" in
           let interface_path = ref "" in
           [
             Set_string module_path;
             Set_string function_path;
             Set_string interface_path;
             Unit
               (fun () ->
                 Queue.add
                   (!module_path, !function_path, !interface_path)
                   jsmodules);
           ]),
        "TODO" );
      ("--version", Unit version, doc_version);
      ("--printast", Unit set_printast, doc_printast);
      ("--printtypes", Unit set_printtypes, doc_printtypes);
      ("--printopt", Unit set_printopt, doc_printopt);
    ]

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let make_components_aux () =
  Queue.fold
    (fun acc fname ->
      In_channel.with_open_text fname
        (Compile.Components.parse_channel ~fname ~name:(fname_to_compname fname))
      :: acc)
    [] templates

let make_components () = make_components_aux () |> Compile.Components.make

let make_components_js () =
  let l = make_components_aux () in
  Queue.fold
    (fun acc (module_path, function_path, interface_path) ->
      let typescheme =
        In_channel.with_open_text interface_path
          (Compile.interface_from_channel ~fname:interface_path)
      in
      let name = fname_to_compname function_path in
      Compile.Components.from_fun ~name typescheme
        Compile.{ module_path; function_path }
      :: acc)
    l jsmodules
  |> Compile.Components.make

let () =
  try
    Arg.parse args set_templates usage_msg;

    let fname = Queue.take templates in

    match !arg_action with
    | Print_ast ->
        In_channel.with_open_text fname (fun chan ->
            (Compile.parse ~fname) (Lexing.from_channel chan))
        |> Ast.to_sexp
        |> Sexp.pp Format.std_formatter
    | Print_types ->
        let components = make_components_js () in
        let template =
          In_channel.with_open_text fname
            (Compile.from_channel ~fname components)
        in
        Typescheme.pp_interface Format.std_formatter template.types
    | Print_optimized ->
        let components = make_components_js () in
        let template =
          In_channel.with_open_text fname
            (Compile.from_channel ~fname components)
        in
        Compile.to_sexp template.nodes |> Sexp.pp Format.std_formatter
    | Render -> (
        let components = make_components () in
        let data =
          match !arg_data with
          | "-" ->
              if Unix.isatty Unix.stdin then print_endline "Enter JSON data:";
              Yojson.Basic.from_channel stdin
          | fname ->
              In_channel.with_open_text fname (Yojson.Basic.from_channel ~fname)
        in
        let template =
          In_channel.with_open_text fname
            (Compile.from_channel ~fname components)
        in
        let result = Render.make template data in
        match !arg_output with
        | "-" -> Out_channel.output_string stdout result
        | fname ->
            Out_channel.(
              with_open_text fname (fun chan -> output_string chan result)))
    | Make_js -> (
        let components = make_components_js () in
        let template =
          In_channel.with_open_text fname
            (Compile.from_channel ~fname components)
        in
        match !arg_output with
        | "-" -> ToJs.pp Format.std_formatter template
        | fname ->
            Out_channel.with_open_text fname (fun chan ->
                ToJs.pp (Format.formatter_of_out_channel chan) template))
  with
  | Error.Acutis_error e ->
      Out_channel.output_string stderr e;
      exit 1
  | Yojson.Json_error s ->
      Format.eprintf "@[<v>Error decoding JSON input.@,%s@,@]" s;
      exit 1
  | Queue.Empty ->
      Format.eprintf "@[<v>Compile error.@,You need to provide a template.@,@]";
      exit 1
  | Sys_error s ->
      Format.eprintf "@[<v>System error:@,%s@,@]" s;
      exit 1
