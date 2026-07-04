
  $ echo '(lang dune 3.4)' > dune-project
  $ echo '(library (name lib1) (modules lib1))' > dune
  $ echo '[@@@zanuda "-eta_reduction"]' > lib1.ml
  $ echo 'let str = (fun s -> print_endline s) "VERY LONG TAINTED STRING FOR ZANUDA"' >> lib1.ml
  $ echo '' > lib1.mli
  $ dune b @check --profile=release
  $ untyped/zanuda.exe -dir . -with-plugins -zanuda-plugin -no-top_file_license
  Loading plugins: [ demo-zanuda-plugin ]
  File "lib1.ml", line 2, characters 37-74:
  2 | let str = (fun s -> print_endline s) "VERY LONG TAINTED STRING FOR ZANUDA"
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Demo dynamically loadable plugin.
