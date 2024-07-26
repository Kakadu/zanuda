  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "dollar.ml", line 4, characters 2-21:
  4 |   pp @@ { field = 1 };;
        ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
  File "dollar.ml", line 5, characters 8-26:
  5 | let _ = string_of_int @@ 4
              ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
  File "dollar.ml", line 6, characters 8-32:
  6 | let _ = string_of_int @@ max_int
              ^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
  File "dollar.ml", line 7, characters 8-35:
  7 | let _ = (function _ -> ()) @@ (1,2)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
  File "dollar.ml", line 8, characters 8-34:
  8 | let _ = (function _ -> ()) @@ None
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
