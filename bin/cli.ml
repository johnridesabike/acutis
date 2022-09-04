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
module Render = Render.Make (Sync) (Acutis_json.Data)

let usage_msg =
  {|Parse and execute Acutis language templates.

Usage:
  acutis [options] [template] [...templates]

Options:|}

let doc_data = " The path to a JSON data file. Default: stdin."
let arg_data = ref None
let set_arg_data s = arg_data := Some s
let doc_output = " The path to write the output. Default: stdout."
let arg_output = ref None
let set_arg_output s = arg_output := Some s
let templates = Queue.create ()
let set_templates fname = Queue.add fname templates
let doc_version = " Show the version number and exit."

let version () =
  Format.printf "Version: %s\n"
    (match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v);
  exit 0

let args =
  Arg.align
    [
      ("--data", String set_arg_data, doc_data);
      ("--output", String set_arg_output, doc_output);
      ("--version", Unit version, doc_version);
    ]

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let () =
  try
    Arg.parse args set_templates usage_msg;

    let fname = Queue.take templates in

    let data =
      match !arg_data with
      | None ->
          if Unix.isatty Unix.stdin then print_endline "Enter JSON data:";
          Yojson.Basic.from_channel stdin
      | Some fname ->
          In_channel.with_open_text fname (Yojson.Basic.from_channel ~fname)
    in

    let components =
      Queue.fold
        (fun acc fname ->
          In_channel.with_open_text fname
            (Compile.Components.parse_channel ~fname
               ~name:(fname_to_compname fname))
          :: acc)
        [] templates
      |> Compile.Components.make
    in

    let template =
      In_channel.with_open_text fname (Compile.from_channel ~fname components)
    in

    let result = Render.make template data in

    match !arg_output with
    | None -> Out_channel.output_string stdout result
    | Some fname ->
        Out_channel.(
          with_open_text fname (fun chan -> output_string chan result))
  with
  | Error.Acutis_error e ->
      Format.eprintf "@[<v>@[Compiler error:@]@,%s@,@]" e;
      exit 1
  | Yojson.Json_error s ->
      Format.eprintf "@[<v>@[Error decoding JSON input:@]@,%s@,@]" s;
      exit 1
  | Queue.Empty ->
      Format.eprintf "Error: You need to provide a template.@,";
      exit 1
  | Sys_error s ->
      Format.eprintf "@[<v>@[System error:@]@,%s@,@]" s;
      exit 1
