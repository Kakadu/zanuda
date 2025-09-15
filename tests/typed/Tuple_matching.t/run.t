  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -no-propose_function -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "Tuple_matching.ml", lines 2-3, characters 2-18:
  2 | ..match scru with
  3 |   | (_, _) -> true
  Alert zanuda-linter: Using `let ... in` is recommended
