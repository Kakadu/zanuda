(lang dune 2.8)

(cram enable)
(generate_opam_files true)

(source (github Kakadu/mylinter))
(license GPL)
(authors "Kakadu")
(maintainers "kakadu@pm.me")

; TODO: invent fancy name

(name mylinter)

(package
 (name mylinter)
 (synopsis "An attempt to create a linter")
 (description "Inspired by https://discuss.ocaml.org/t/how-possible-is-a-clippy-like-linter-for-ocaml")
 (depends
  (yojson (>= 0))
  (base (>= v0.14.1)))
)
