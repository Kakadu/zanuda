  $ dune build
  $ zanuda -no-check-filesystem -no-eta_reduction -no-top_file_license -dir .  -ordjsonl /dev/null
  File "laws.ml", line 5, characters 12-35:
  5 | let foo x = x >>= fun y -> return y
                  ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Applying monad laws allows to write monadic code in more compact way.
