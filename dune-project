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
   (and
    (>= 0.25.1)
    (< 0.27.0)))
  sexplib
  ppx_inline_test ; implies dependecy on base
  ppx_assert
  ppx_expect
  (base :with-test)
  stdune
  angstrom
  (ppx_blob :with-test)
  (ppx_fields_conv :with-test)
  ;(menhir :with-test)
  (ocamlformat
   (>= 0.23.0))))
