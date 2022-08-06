(lang dune 3.4)

(cram enable)

(generate_opam_files true)

(source
 (github Kakadu/zanuda))

(license LGPL-3.0-only)

(authors "Kakadu")

(maintainers "kakadu@pm.me")

(name zanuda)

(package
 (name zanuda)
 (synopsis "An attempt to create a linter")
 (description
  "Inspired by https://discuss.ocaml.org/t/how-possible-is-a-clippy-like-linter-for-ocaml")
 (depends
  yojson
  (ppxlib
   (< 0.26.0))
  sexplib
  ppx_inline_test
  ppx_assert
  base
  angstrom
  (ppx_blob :with-test)
  (ocamlformat
   (>= 0.23.0))))
