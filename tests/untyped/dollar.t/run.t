  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "dollar.ml", line 4, characters 2-21:
  4 |   pp @@ { field = 1 };;
        ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Extranous `@@`.
