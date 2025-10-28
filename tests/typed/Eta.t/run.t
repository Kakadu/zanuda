  $ dune build
  $ ../zanuda.exe  -no-check-filesystem -no-top_file_license -no-mutability_check -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "Eta.ml", line 3, characters 12-23:
  3 | let wrapper x = my_id x
                  ^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun x -> my_id x' as 'my_id'
  File "Eta.ml", line 5, characters 11-22:
  5 | let my_add x y = x + y
                 ^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun x -> fun y -> x + y' as '(+)'
  File "Eta.ml", line 13, characters 7-28:
  13 | let xx f g h = my_add3 f g h
              ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun f -> fun g -> fun h -> my_add3 f g h' as 'my_add3'
  File "Eta.ml", line 17, characters 35-72:
  17 | let listsAreEqual a b = List.equal (fun lhs rhs -> String.equal lhs rhs) a b
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun lhs -> fun rhs -> String.equal lhs rhs' as 
                       'String.equal'
