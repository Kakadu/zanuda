  $ dune build
  $ zanuda -dir .
  File "laws.ml", line 5, characters 12-35:
  5 | let foo x = x >>= fun y -> return y
                  ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Applying monad laws allows to write monadic code in more compact way.
