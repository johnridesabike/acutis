(lang dune 3.3)

(using menhir 2.1)

(name acutis)

(version 0.15.0)

(generate_opam_files true)

(source
 (github johnridesabike/acutis))

(authors "John Jackson")

(maintainers "John Jackson")

(license MPL-2.0)

(documentation https://johnridesa.bike/acutis/)

(package
 (name acutis)
 (synopsis "A declarative template language")
 (depends
   (ocaml (>= 4.14))
   dune
   (ocamlformat (= 0.22.4))
   (odoc (>= 2.1))
   (js_of_ocaml (>= 4.0))
   (js_of_ocaml-compiler (>= 4.0))
   (js_of_ocaml-ppx (>= 4.0))
   (ppx_deriving (>= 5.2.1))
   (menhir (= 20220210))
   (promise_jsoo (>= 0.3.1))
   (alcotest (>= 1.5))
   (alcotest-lwt (>= 1.5))
   (lwt (>= 5.5)))
 (tags
  (template language)))

(package
 (name acutis_data_json)
 (synopsis "Encode and decode JSON data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (yojson (>= 2.0))))

(package
 (name acutis_data_js)
 (synopsis "Encode and decode JavaScript data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (js_of_ocaml (>= 4.0))))
