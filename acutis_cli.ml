(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Acutis_json = Acutis.Of_decodable (struct
  type t = Yojson.Basic.t
  type 'a assoc = (string * 'a) list

  let get_int = function `Int x -> Some x | _ -> None
  let get_string = function `String x -> Some x | _ -> None

  let get_float = function
    | `Float x -> Some x
    | `Int x -> Some (Float.of_int x)
    | _ -> None

  let get_bool = function `Bool x -> Some x | _ -> None
  let get_some = function `Null -> None | x -> Some x
  let get_seq = function `List x -> Some (List.to_seq x) | _ -> None
  let get_assoc = function `Assoc x -> Some x | _ -> None
  let assoc_find = List.assoc
  let assoc_mem = List.mem_assoc
  let assoc_to_seq = List.to_seq
  let null = `Null
  let some = Fun.id
  let of_float x = `Float x
  let of_string x = `String x
  let of_bool x = `Bool x
  let of_int x = `Int x
  let of_seq x = `List (List.of_seq x)
  let of_seq_assoc x = `Assoc (List.of_seq x)
  let to_string t = Yojson.Basic.pretty_to_string t
  let marshal x = `String (Marshal.to_string x [])
end)

let usage_msg =
  {|Usage:
  acutis [OPTIONS...] [TEMPLATE] [COMPONENTS...]

Compile and render Acutis language templates.

Options:|}

type 'a mode = Render | Make_js of 'a

let arg_mode = ref Render
let arg_data = ref "-"
let arg_output = ref "-"
let arg_version = ref false
let arg_printast = ref false
let arg_printtypes = ref false
let arg_printopt = ref false
let arg_printinst = ref false
let templates = Queue.create ()
let arg_funs = Queue.create ()

let args =
  [
    ( "--mode",
      Arg.Symbol
        ( [ "render"; "js"; "cjs" ],
          function
          | "js" -> arg_mode := Make_js Acutis.esm
          | "cjs" -> arg_mode := Make_js Acutis.cjs
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
    ( "--printinst",
      Arg.Set arg_printinst,
      " Print the template's runtime instructions and exit." );
  ]

let ( let@ ) = ( @@ )

let fname_to_compname s =
  Filename.basename s |> Filename.remove_extension |> String.capitalize_ascii

let stderr_needs_newline = ref false

let eprintf_msg fmt =
  Format.kdprintf
    (fun t ->
      if !stderr_needs_newline then Format.eprintf "@,@,%t" t
      else Format.eprintf "%t" t;
      stderr_needs_newline := true)
    fmt

let print_msgs = List.iter (eprintf_msg "%a" Acutis.pp_message)

let get_or_exit = function
  | msgs, Some x -> print_msgs msgs; x
  | msgs, None -> print_msgs msgs; exit 1

let components_parsed () =
  Queue.to_seq templates
  |> Seq.filter_map (fun fname ->
         let@ chan = In_channel.with_open_bin fname in
         let lexbuf = Lexing.from_channel chan in
         Lexing.set_filename lexbuf fname;
         let msgs, parsed = Acutis.parse lexbuf in
         print_msgs msgs;
         let name = fname_to_compname fname in
         Option.map (Acutis.comp_of_parsed name) parsed)

let components () = Acutis.comps_compile (components_parsed ()) |> get_or_exit

let components_js () =
  let l = components_parsed () in
  let funl =
    Queue.to_seq arg_funs
    |> Seq.filter_map (fun (module_path, function_path, interface) ->
           let lexbuf = Lexing.from_string interface in
           Lexing.set_filename lexbuf "-";
           let msgs, interface = Acutis.compile_interface lexbuf in
           print_msgs msgs;
           Option.map
             (fun interface ->
               Acutis.comp_of_fun
                 (fname_to_compname function_path)
                 interface
                 (Acutis.js_import ~module_path ~function_path))
             interface)
  in
  Seq.append l funl |> Acutis.comps_compile |> get_or_exit

let () =
  Format.eprintf "@[<v>";
  try
    Arg.parse (Arg.align args)
      (fun fname -> Queue.add fname templates)
      usage_msg;
    if !arg_version then
      Format.printf "Version: %s@."
        (match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v)
    else
      let fname = Queue.take templates in
      let@ chan = In_channel.with_open_bin fname in
      let lexbuf = Lexing.from_channel chan in
      Lexing.set_filename lexbuf fname;
      let parsed = Acutis.parse lexbuf |> get_or_exit in
      if !arg_printast then Format.printf "%a" Acutis.pp_ast parsed
      else if !arg_printtypes then
        Acutis.compile (components_js ()) parsed
        |> get_or_exit |> Acutis.get_interface
        |> Format.printf "%a" Acutis.pp_interface
      else if !arg_printopt then
        Acutis.compile (components_js ()) parsed
        |> get_or_exit
        |> Format.printf "%a" Acutis.pp_compiled
      else if !arg_printinst then
        Acutis.compile (components_js ()) parsed
        |> get_or_exit
        |> Format.printf "%a" (Acutis.pp_instructions Acutis.pp_js_import)
      else
        match !arg_mode with
        | Render -> (
            let data =
              match !arg_data with
              | "-" ->
                  if In_channel.isatty stdin then
                    eprintf_msg "Enter JSON data:@.";
                  Yojson.Basic.from_channel stdin
              | fname ->
                  In_channel.with_open_bin fname
                    (Yojson.Basic.from_channel ~fname)
            in
            let template =
              Acutis.compile (components ()) parsed |> get_or_exit
            in
            match Acutis_json.apply template data with
            | Ok result -> (
                match !arg_output with
                | "-" -> Out_channel.output_string stdout result
                | fname ->
                    let@ chan = Out_channel.with_open_bin fname in
                    Out_channel.output_string chan result)
            | Error x -> print_msgs x; exit 1)
        | Make_js printer -> (
            let template =
              Acutis.compile (components_js ()) parsed |> get_or_exit
            in
            match !arg_output with
            | "-" -> Format.printf "%a" printer template
            | fname ->
                let@ chan = Out_channel.with_open_bin fname in
                printer (Format.formatter_of_out_channel chan) template)
  with
  | Yojson.Json_error s ->
      eprintf_msg "Error decoding JSON input.@,%s@." s;
      exit 1
  | Queue.Empty ->
      eprintf_msg "You need to provide a template.@,@,%s"
        (Arg.usage_string (Arg.align args) usage_msg);
      exit 1
  | Sys_error s ->
      eprintf_msg "System error:@,%s@." s;
      exit 1
