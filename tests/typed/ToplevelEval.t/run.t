  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .
  File "test_toplevel_eval.ml", line 1, characters 0-3:
  1 | 1+1;;
      ^^^
  Alert zanuda-linter: Toplevel eval not recommended
