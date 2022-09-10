(lang dune 3.3)

(using menhir 2.1)

(name acutis)

(version 0.18.1)

(generate_opam_files true)

(source
 (github johnridesabike/acutis))

(authors "John Jackson")

(maintainers "John Jackson")

(license MPL-2.0)

(documentation https://johnridesa.bike/acutis/)

(package
 (name acutis)
 (synopsis "A declarative, type-safe template language")
 (depends
   (ocaml (>= 4.14))
   (menhir (= 20220210))
   ppx_deriving
   (alcotest :with-test)
   (alcotest-lwt :with-test)
   (lwt :with-test))
 (tags
  (template language parser interpreter)))

(package
 (name acutis_json)
 (synopsis "Encode and decode JSON data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (acutis (= :version))
   yojson
   dune-build-info))

(package
 (name acutis_js)
 (synopsis "Encode and decode JavaScript data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (acutis (= :version))
   promise_jsoo
   js_of_ocaml
   js_of_ocaml-compiler
   js_of_ocaml-ppx))
