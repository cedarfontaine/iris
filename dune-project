(lang dune 3.8)

(name iris)
(version 0.1.0)

(generate_opam_files true)

(source (github username/iris))
(license MIT)
(authors "cedar")
(maintainers "")

(package
 (name iris)
 (synopsis "x86 code generation library in OCaml")
 (description "")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.8.0))
  (zarith (>= 1.12))
  (ppx_deriving (>= 5.2.1))
  (ppx_deriving_yojson (>= 3.7.0))
  (cmdliner (>= 1.1.1))
  (alcotest :with-test)
  (qcheck-alcotest :with-test)))