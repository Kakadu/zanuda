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
 (version 1.1.0)
 (synopsis "Linter for OCaml+dune projects")
 (description
  "Lints for OCaml projects. Primary usage is for teaching/education")
 (tags
  (lint test))
 (depends
  (ocaml
   (or
    (= 4.14)
    (and
     (>= 5.3.0)
     (< 5.4.0))))
  (yojson
   (>= 2.0.0))
  ; (ppxlib
  ;  (and
  ;   (>= 0.25.1)
  ;   (<= 0.28.0)))
  (angstrom
   (and
    (>= 0.15.0)
    (<= 0.16.0)))
  sexplib
  bisect_ppx
  dune-build-info
  ppx_assert
  ppx_expect_nobase
  ppx_optcomp
  (base :with-test)
  (ppx_assert :with-test)
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
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
 (version 1.1.0)
 (synopsis "Creator of review for GitHub PR")
 (description
  "The tool for creating review in GitHub Pull Requests by zanuda's report")
 (depends
  (ocaml
   (or
    (= 4.14)
    (and
     (>= 5.3.0)
     (< 5.4.0))))
  curly
  yojson
  (angstrom
   (and
    (>= 0.15.0)
    (<= 0.16.0)))
  ppx_inline_test
  ppx_show
  ppx_expect
  bisect_ppx
  (ocamlformat
   (and
    (= 0.26.2)
    :dev))))
