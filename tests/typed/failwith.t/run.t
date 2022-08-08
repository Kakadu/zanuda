  $ dune build
  $ zanuda -no-top_file_license -dir .
  File "Failwith.ml", line 1, characters 12-20:
  1 | let foo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using failwith unsafely
