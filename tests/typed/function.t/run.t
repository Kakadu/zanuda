  $ dune build
  $ zanuda -dir .
  File "Function.ml", line 1, characters 23-56:
  1 | let should_give_a_lint x = match x with [] -> 1 | _ -> 2
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using `function` is recommended
