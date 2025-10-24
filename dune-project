(lang dune 3.13)

(using menhir 3.0)

(name acutis)

(version 0.28.1)

(generate_opam_files true)

(source
 (sourcehut johnridesabike/acutis))

(homepage https://acutis.johnridesa.bike/)

(documentation https://acutis.johnridesa.bike/api/acutis/Acutis/)

(authors "John Jackson <john@johnridesa.bike>")

(maintainers "John Jackson <john@johnridesa.bike>")

(license MPL-2.0)

(package
 (name acutis)
 (synopsis "A simple and type-safe template language")
 (description
"Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. You can execute your templates like scripts or
compile them to self-contained JavaScript files.
")
 (depends
  (ocaml
   (>= 5.3))
  menhir
  yojson
  dune-build-info
  (ocamlformat
   (and
    (= 0.27.0)
    :with-test))
  (sherlodoc :with-doc)))

(package
 (name acutis_js)
 (synopsis "A virtual package containing the Acutis JavaScript dependencies")
 (allow_empty)
 (depends
  ocaml
  (acutis
   (= :version))
  (js_of_ocaml
   (>= 5.8.1))
  js_of_ocaml-compiler
  js_of_ocaml-ppx))
