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

module Set_string = Set.Make (String)

let version = ref ""
let path = ref ""
let exports = ref [ ("./package.json", s "./package.json") ]
let imports = ref []
let files = ref Set_string.empty

let () =
  Arg.parse
    [
      ("--version", Set_string version, "Version");
      ( "--export",
        Tuple
          [
            Set_string path;
            String
              (fun file ->
                files := Set_string.add file !files;
                exports := (!path, s file) :: !exports);
          ],
        "Subpath export" );
      ( "--import",
        Tuple
          [
            Set_string path;
            String
              (fun file ->
                files := Set_string.add file !files;
                imports := (!path, s file) :: !imports);
          ],
        "Internal subpath import" );
    ]
    invalid_arg "Generate the package.json."

let () =
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
      ("homepage", s "https://acutis.johnridesa.bike/");
      ( "bugs",
        o
          [
            ("url", s "https://todo.sr.ht/~johnridesabike/acutis");
            ("email", s "~johnridesabike/public-inbox@lists.sr.ht");
          ] );
      ( "repository",
        o
          [
            ("type", s "git");
            ("url", s "git+https://git.sr.ht/~johnridesabike/acutis");
          ] );
      ("license", s "MPL-2.0");
      ( "author",
        o
          [
            ("name", s "John Jackson");
            ("url", s "https://johnridesa.bike/");
            ("email", s "john@johnridesa.bike");
          ] );
      ("type", s "module");
      ("exports", o !exports);
      ("imports", o !imports);
      ("files", a (Set_string.to_seq !files |> Seq.map s |> List.of_seq));
    ]
    Format.std_formatter
