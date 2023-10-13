  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "Nested_if.ml", line 5, characters 2-77:
  5 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
  File "Nested_if.ml", line 11, characters 2-77:
  11 |   if x then x else (if x then (if x then x else (if x then x else y)) else y)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
