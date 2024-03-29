(lang dune 3.13)

(using menhir 3.0)

(name acutis)

(version 0.24.1)

(generate_opam_files true)

(source
 (github johnridesabike/acutis))

(authors "John Jackson <jbpjackson+acutis@icloud.com>")

(maintainers "John Jackson <jbpjackson+acutis@icloud.com>")

(license MPL-2.0)

(documentation https://johnridesa.bike/acutis/)

(package
 (name acutis)
 (synopsis "A simple and type-safe template language")
 (description "
Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. You can execute your templates like scripts or
compile them to self-contained JavaScript files.
")
 (depends
  (ocaml
   (>= 5.0))
  menhir
  yojson
  dune-build-info
  (ocamlformat
   (and
    (= 0.26.1)
    :with-test))))

(package
 (name acutis_js)
 (synopsis "A virtual package containing the Acutis JavaScript dependencies")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.0))
  (acutis
   (= :version))
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx))
