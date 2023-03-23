(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This generates a package.json file with the main JS file path and the
    version number. You can then execute npm commands (link, publish, etc.) in
    the build context directory. *)

let version = ref ""
let main = ref ""
let files = ref []

let () =
  Arg.parse
    [
      ("--main", Set_string main, "main JS file");
      ("--version", Set_string version, "version");
      ("--files", Rest_all (fun l -> files := l), "other files");
    ]
    invalid_arg "Generate the package.json."

type t = O of (string * t) list | A of t list | S of string | B of bool

open Format

let comma ppf () = fprintf ppf ",@ "

let rec pp ppf = function
  | O l ->
      fprintf ppf "@[<hv 2>{@;<1 0>";
      pp_print_list ~pp_sep:comma pp_field ppf l;
      fprintf ppf "@;<1 -2>}@]"
  | A l ->
      fprintf ppf "@[<hv 2>[@;<1 0>";
      pp_print_list ~pp_sep:comma pp ppf l;
      fprintf ppf "@;<1 -2>]@]"
  | S s -> fprintf ppf "%S" s
  | B b -> pp_print_bool ppf b

and pp_field ppf (k, v) = fprintf ppf "@[<hv 2>%S:@ %a@]" k pp v

let json =
  O
    [
      ("name", S "acutis-lang");
      ("version", S !version);
      ("private", B false);
      ("description", S "A declarative, type-safe template language");
      ( "keywords",
        A
          [
            S "template";
            S "language";
            S "parser";
            S "interpreter";
            S "eleventy-plugin";
            S "eleventy";
            S "ocaml";
          ] );
      ("homepage", S "https://johnridesa.bike/acutis/");
      ( "bugs",
        O
          [
            ("url", S "https://github.com/johnridesabike/acutis/issues");
            ("email", S "jbpjackson+acutis@icloud.com");
          ] );
      ( "repository",
        O
          [
            ("type", S "git");
            ("url", S "https://github.com/johnridesabike/acutis.git");
          ] );
      ("license", S "MPL-2.0");
      ( "author",
        O
          [
            ("name", S "John Jackson");
            ("url", S "https://johnridesa.bike/");
            ("email", S "jbpjackson+acutis@icloud.com");
          ] );
      ("main", S !main);
      ("files", A (List.map (fun s -> S s) !files));
    ]

let () = pp std_formatter json
