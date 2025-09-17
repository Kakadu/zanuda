  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-eta_reduction -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "laws.ml", line 5, characters 12-35:
  5 | let foo x = x >>= fun y -> return y
                  ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Applying monad laws allows to write monadic code in more compact way.
  File "laws.ml", line 6, characters 28-56:
  6 | let _should_be_reported x = x >>= function x -> return x [@@warning "-27"]
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Applying monad laws allows to write monadic code in more compact way.
