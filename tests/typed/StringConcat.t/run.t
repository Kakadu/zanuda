  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "StringConcat.ml", line 6, characters 8-26:
  6 | let _ = List.fold_left (^)
              ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Concatenating a container of strings via fold-like iteration may lead to performance issues.
  File "StringConcat.ml", line 7, characters 8-35:
  7 | let _ = ListLabels.fold_left ~f:(^)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Concatenating a container of strings via fold-like iteration may lead to performance issues.
  $ zanuda  -no-check-filesystem -no-top_file_license -skip-level-allow false -dir .
  File "StringConcat.ml", line 2, characters 1-10:
  2 |  a ^ b ^ c
       ^^^^^^^^^
  Alert zanuda-linter: Concatenating multiple strings at once (`a^b^c`) has a perfomance issue.
  
  File "StringConcat.ml", line 6, characters 8-26:
  6 | let _ = List.fold_left (^)
              ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Concatenating a container of strings via fold-like iteration may lead to performance issues.
  File "StringConcat.ml", line 7, characters 8-35:
  7 | let _ = ListLabels.fold_left ~f:(^)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Concatenating a container of strings via fold-like iteration may lead to performance issues.
