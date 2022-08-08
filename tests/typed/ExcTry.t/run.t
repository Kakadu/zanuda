  $ dune build
  $ zanuda -no-top_file_license -dir .
  File "ExcTryWithWildcard.ml", lines 2-3, characters 2-11:
  2 | ..try raise Not_found
  3 | with _ -> 1
  Alert zanuda-linter: Antipattern: error swallowing
