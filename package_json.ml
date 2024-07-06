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

let comma ppf () = Format.fprintf ppf ",@ "
let s = Format.dprintf "%S"
let b = Format.dprintf "%B"
let member ppf (k, v) = Format.fprintf ppf "@[<hv 2>%S:@ %t@]" k v

let o =
  Format.dprintf "@[<hv 2>{@;<1 0>%a@;<1 -2>}@]"
    (Format.pp_print_list ~pp_sep:comma member)

let a =
  Format.dprintf "@[<hv 2>[@;<1 0>%a@;<1 -2>]@]"
    (Format.pp_print_list ~pp_sep:comma ( |> ))

let json =
  o
    [
      ("name", s "acutis-lang");
      ("version", s !version);
      ("private", b false);
      ("description", s "A simple and type-safe template language");
      ( "keywords",
        a
          [
            s "template";
            s "language";
            s "parser";
            s "interpreter";
            s "eleventy-plugin";
            s "eleventy";
            s "ocaml";
          ] );
      ("homepage", s "https://johnridesa.bike/acutis/");
      ( "bugs",
        o
          [
            ("url", s "https://github.com/johnridesabike/acutis/issues");
            ("email", s "jbpjackson+acutis@icloud.com");
          ] );
      ( "repository",
        o
          [
            ("type", s "git");
            ("url", s "https://github.com/johnridesabike/acutis.git");
          ] );
      ("license", s "MPL-2.0");
      ( "author",
        o
          [
            ("name", s "John Jackson");
            ("url", s "https://johnridesa.bike/");
            ("email", s "jbpjackson+acutis@icloud.com");
          ] );
      ("main", s !main);
      ("files", a (List.map s !files));
    ]

let () = json Format.std_formatter
