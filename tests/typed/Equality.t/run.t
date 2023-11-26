  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "Equality.ml", line 1, characters 11-36:
  1 | let __ x = if x = None then 1 else 1
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using generic equality for type option and other algebraic data types is not recommended. Use pattern matching
  File "Equality.ml", line 2, characters 11-34:
  2 | let __ x = if x = [] then 2 else 2
                 ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using generic equality for type list and other algebraic data types is not recommended. Use pattern matching
  File "Equality.ml", line 3, characters 11-36:
  3 | let __ x = if x = true then 3 else 3
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using generic equality for type bool and other algebraic data types is not recommended. Use pattern matching
  File "Equality.ml", line 14, characters 2-18:
  14 |   x == (1::suffix)
         ^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Do you really need physical equality? Physical means the equality of pointers.
