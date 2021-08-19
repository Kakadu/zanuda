  $ dune build
$ dune describe
  $ mylinter -dir .
  File "exec1.ml", line 1, characters 13-32:
  1 | let __  xs = List.length xs <= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 2, characters 13-32:
  2 | let __  xs = 0 >= List.length xs
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 3, characters 13-32:
  3 | let __  xs = List.length xs >= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 4, characters 13-32:
  4 | let __  xs = 0 <= List.length xs
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "Parsetree.mli", line 2, characters 2-16:
  2 |   | App of t * t
        ^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
  File "list_len.ml", line 6, characters 12-31:
  6 | let __ xs = List.length xs <= 0
                  ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 8, characters 12-30:
  8 | let __ xs = List.length xs = 0
                  ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 10, characters 12-31:
  10 | let __ xs = List.length xs >= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 12, characters 12-30:
  12 | let __ xs = List.length xs > 0
                   ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 14, characters 13-31:
  14 | let __ xs =  0 < List.length xs
                    ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "Function.ml", line 1, characters 23-56:
  1 | let should_give_a_lint x = match x with [] -> 1 | _ -> 2
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using `function` is recommended
  File "Failwith.ml", line 1, characters 12-20:
  1 | let foo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using failwith unsafely
