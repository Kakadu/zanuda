(lang dune 3.4)

(cram enable)

(generate_opam_files true)

(source
 (github Kakadu/zanuda))

(license LGPL-3.0-only)

(authors "Kakadu")

(maintainers "kakadu.hafanana@gmail.com")

(name zanuda)

(package
 (name zanuda)
 (version 2.0.0)
 (synopsis "Linter for OCaml+dune projects")
 (description
  "Lints for OCaml projects. Primary usage is for teaching/education")
 (tags
  (lint test))
 (depends
  (ocaml
   (or
    (and
     (>= 4.14.2)
     (< 5.0.0))
    (and
     (>= 5.3.0)
     (< 5.4.0))
    (and
     (>= 5.5.0)
     (< 5.6.0))))
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
  dune-build-info
  ppx_expect_nobase
  ppx_optcomp
  ppx_sexp_conv
  (base :with-test)
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (ppx_fields_conv :with-test)
  (ppx_deriving :with-test)
  (ppx_blob :with-test) ; currently required for ToplevelEval test
  (menhir :with-test)
  (bisect_ppx :with-dev-setup)
  (ocamlformat
   (and
    (= 0.27.0)
    :with-dev-setup))
  ;odig ; documentation flag remove for an experiment
  (odig :with-doc)
  (odoc :with-doc)))

(package
 (name reviewer)
 (version 2.0.0)
 (synopsis "Creator of review for GitHub PR")
 (description
  "The tool for creating review in GitHub Pull Requests by zanuda's report")
 (depends
  (ocaml
   (or
    (and
     (>= 4.14.2)
     (< 5.0.0))
    (and
     (>= 5.3.0)
     (< 5.4.0))
    (and
     (>= 5.5.0))))
  curly
  yojson
  (angstrom
   (and
    (>= 0.15.0)
    (<= 0.16.0)))
  dune-build-info
  ppx_inline_test
  ppx_expect
  (bisect_ppx :with-dev-setup)
  (ocamlformat
   (and
    (= 0.27.0)
    :with-dev-setup))))

(package
 (name demo-zanuda-plugin)
 (synopsis "A demo for dynamically loadable Zanuda plugin")
 (depends zanuda dune-build-info))
