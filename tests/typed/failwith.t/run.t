  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .
  File "Failwith.ml", line 1, characters 12-20:
  1 | let foo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using failwith unsafely
  File "Failwith.ml", line 5, characters 12-20:
  5 | let boo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using failwith unsafely
