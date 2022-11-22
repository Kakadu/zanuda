  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .
  File "Failwith.ml", line 1, characters 12-20:
  1 | let foo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using `failwith` (or `assert false`) usually is a clue that a corner case is not being handled properly. To report errors we recommend using error monad instead. In princliple, these construction are OK for temporary work-in-progress code, but in release they should be eliminated
  File "Failwith.ml", line 5, characters 12-20:
  5 | let boo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using `failwith` (or `assert false`) usually is a clue that a corner case is not being handled properly. To report errors we recommend using error monad instead. In princliple, these construction are OK for temporary work-in-progress code, but in release they should be eliminated
  File "Failwith.ml", line 7, characters 15-27:
  7 | let coucou _ = assert false
                     ^^^^^^^^^^^^
  Alert zanuda-linter: Using `failwith` (or `assert false`) usually is a clue that a corner case is not being handled properly. To report errors we recommend using error monad instead. In princliple, these construction are OK for temporary work-in-progress code, but in release they should be eliminated
