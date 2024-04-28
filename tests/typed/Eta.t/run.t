  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null
  File "Eta.ml", line 3, characters 12-23:
  3 | let wrapper x = my_id x
                  ^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction may be applied. It is recommenden to rewrite it as 'let (_: 'a -> 'a) = my_id'
