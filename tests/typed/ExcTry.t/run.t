  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "ExcTryWithWildcard.ml", lines 2-3, characters 2-11:
  2 | ..try raise Not_found
  3 | with _ -> 1
  Alert zanuda-linter: Antipattern: error swallowing
