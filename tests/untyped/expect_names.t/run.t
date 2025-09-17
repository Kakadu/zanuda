  $ dune test
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "expect_names.ml", line 2, characters 0-22:
  2 | let%expect_test _ = ()
      ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: A test without description. Try `let%expect_test "name" = ...
  File "expect_names.ml", line 4, characters 0-22:
  4 | let%expect_test _ = ()
      ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: A test without description. Try `let%expect_test "name" = ...
