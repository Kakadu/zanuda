  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -no-propose_function -no-propose_function_untyped -dir .  -ordjsonl /dev/null
  File "Tuple_matching.ml", lines 2-3, characters 2-18:
  2 | ..match scru with
  3 |   | (_, _) -> true
  Alert zanuda-linter: Using `let ... in` is recommended
