  $ dune build @check
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "main.ml", line 1, characters 0-17:
  1 | [@@@warning "-A"]
      ^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Disabling *all* warnings is usually a bad idea.
  File "main.ml", line 3, characters 0-23:
  3 | [@@@ocaml.warning "-A"]
      ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Disabling *all* warnings is usually a bad idea.
