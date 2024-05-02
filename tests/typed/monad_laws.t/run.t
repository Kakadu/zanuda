  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "laws.ml", line 5, characters 12-35:
  5 | let foo x = x >>= fun y -> return y
                  ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Applying monad laws allows to write monadic code in more compact way.
  File "laws.ml", line 5, characters 18-35:
  5 | let foo x = x >>= fun y -> return y
                        ^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite it as 'let (_: 'a -> 'a) = return'
