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
 (synopsis "Linter for OCaml+dune projects")
 (description
  "Lints for OCaml projects. Primary usage is for teaching/education")
 (tags
  (lint test))
 (depends
  (ocaml
   (and
    (>= 4.14)
    (< 5.0)))
  (yojson
   (>= 2.0.0))
  ; (ppxlib
  ;  (and
  ;   (>= 0.25.1)
  ;   (<= 0.28.0)))
  sexplib
  ppx_inline_test ; implies dependecy on base
  ppx_assert
  ppx_expect
  bisect_ppx
  stdune
  dune-build-info
  (angstrom
   (>= 0.15.0))
  (base :with-test)
  (ppx_fields_conv :with-test)
  (ppx_deriving :with-test)
  (ppx_blob :with-test) ; currently required for ToplevelEval test
  (menhir :with-test)
  (ocamlformat
   (and
    (= 0.26.2)
    :dev))
  (odoc :with-doc)
  (odig :with-doc)))

(package
 (name reviewer)
 (synopsis "Creator of review for GitHub PR")
 (description
  "The tool for creating review in GitHub Pull Requests by zanuda's report")
 (depends
  (ocaml
   (and
    (>= 4.14)
    (< 5.0)))
  ppx_inline_test
  ppx_show
  ppx_expect
  bisect_ppx
  curly
  yojson
  stdune
  (angstrom
   (>= 0.15.0))
  (ocamlformat
   (and
    (= 0.26.2)
    :dev))))
