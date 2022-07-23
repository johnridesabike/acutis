(lang dune 3.3)

(using menhir 2.1)

(name acutis)

(version 0.15.1)

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
   (ppx_deriving (>= 5.2.1))
   (ocamlformat (and :dev (= 0.22.4)))
   (ocaml-lsp-server :dev)
   (alcotest (and :with-test (>= 1.5)))
   (alcotest-lwt (and :with-test (>= 1.5)))
   (lwt (and :with-test (>= 5.5))))
 (tags
  (template language parser interpreter)))

(package
 (name acutis_json)
 (synopsis "Encode and decode JSON data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (acutis (= :version))
   (yojson (>= 2.0))))

(package
 (name acutis_js)
 (synopsis "Encode and decode JavaScript data for Acutis")
 (depends
   (ocaml (>= 4.14))
   (acutis (= :version))
   (promise_jsoo (>= 0.3.1))
   (js_of_ocaml (>= 4.0))
   (js_of_ocaml-compiler (>= 4.0))
   (js_of_ocaml-ppx (>= 4.0))))
