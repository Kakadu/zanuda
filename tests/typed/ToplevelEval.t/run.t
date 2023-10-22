  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "test_toplevel_eval.ml", line 1, characters 0-3:
  1 | 1+1;;
      ^^^
  Alert zanuda-linter: Toplevel eval not recommended
  File "test_toplevel_eval.ml", line 2, characters 25-41:
  2 | let _ : string  = [%blob "./anonymous.rb"]
                               ^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Toplevel eval not recommended
  File "test_toplevel_eval.ml", line 5, characters 6-32:
  5 | [%%if ocaml_version < (4, 11, 2)]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Toplevel eval not recommended
