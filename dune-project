(lang dune 2.8)

(generate_opam_files true)

(name mylinter-example)

(package
 (name mylinter-example)
 (depends
  (ppx_inline_test :with-test)
  base))
