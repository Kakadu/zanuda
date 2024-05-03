  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null
  File "Eta.ml", line 3, characters 12-23:
  3 | let wrapper x = my_id x
                  ^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite it as 'my_id'
  File "Eta.ml", line 5, characters 11-22:
  5 | let my_add x y = x + y
                 ^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite it as '(+)'
  File "Eta.ml", line 11, characters 7-28:
  11 | let xx f g h = my_add3 f g h 
              ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite it as 'my_add3'
  File "Eta.ml", line 15, characters 35-72:
  15 | let listsAreEqual a b = List.equal (fun lhs rhs -> String.equal lhs rhs) a b
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite it as 'String.equal'
