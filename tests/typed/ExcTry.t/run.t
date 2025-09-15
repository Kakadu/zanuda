  $ dune build
  $ ../zanuda.exe  -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "ExcTryWithWildcard.ml", lines 2-3, characters 2-11:
  2 | ..try raise Not_found
  3 | with _ -> 1
  Alert zanuda-linter: Antipattern: error swallowing
  File "ExcTryWithWildcard.ml", line 24, characters 4-23:
  24 |     try 1 with | _ -> 2
           ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Antipattern: error swallowing
