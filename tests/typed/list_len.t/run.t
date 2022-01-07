  $ dune build
  $ mylinter -dir .
  File "list_len.ml", line 3, characters 12-31:
  3 | let __ xs = List.length xs <= 0
                  ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between 'List.length xs' and '0'.
  File "list_len.ml", line 5, characters 12-30:
  5 | let __ xs = List.length xs = 0
                  ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between 'List.length xs' and '0'.
  File "list_len.ml", line 7, characters 12-31:
  7 | let __ xs = List.length xs >= 0
                  ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between 'List.length xs' and '0'.
  File "list_len.ml", line 9, characters 12-30:
  9 | let __ xs = List.length xs > 0
                  ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between 'List.length xs' and '0'.
  File "list_len.ml", line 11, characters 13-31:
  11 | let __ xs =  0 < List.length xs
                    ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between '0' and 'List.length xs'.
  File "list_len.ml", line 13, characters 12-30:
  13 | let __ xs = 0 = List.length xs
                   ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  Between '0' and 'List.length xs'.
