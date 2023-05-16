(lang dune 3.3)

(using menhir 2.1)

(name acutis)

(version 0.22.0)

(generate_opam_files true)

(source
 (github johnridesabike/acutis))

(authors "John Jackson <jbpjackson+acutis@icloud.com>")

(maintainers "John Jackson <jbpjackson+acutis@icloud.com>")

(license MPL-2.0)

(documentation https://johnridesa.bike/acutis/)

(package
 (name acutis)
 (synopsis "A declarative, type-safe template language")
 (depends
  (ocaml
   (>= 4.14))
  menhir
  yojson
  dune-build-info
  (ocamlformat
   (and
    (= 0.25.1)
    :with-test)))
 (tags
  (template language parser interpreter)))

(package
 (name acutis_js)
 (synopsis "A virtual package containing the Acutis JavaScript dependencies")
 (allow_empty)
 (depends
  (ocaml
   (>= 4.14))
  (acutis
   (= :version))
  promise_jsoo
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx))
