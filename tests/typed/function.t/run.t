  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .
  File "Function.ml", line 1, characters 23-56:
  1 | let should_give_a_lint x = match x with [] -> 1 | _ -> 2
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using `function` is recommended
  File "Function.ml", lines 5-7, characters 8-40:
  5 | ........fun c ->
  6 |   match c with
  7 |   | xs -> List.mem (fun c -> c = 'a') xs
  Alert zanuda-linter: Using `function` is recommended
  File "Function.ml", lines 10-14, characters 16-17:
  10 | ................fun ch ->
  11 |   match ch with
  12 |   | ch when List.mem ch [ "$"; "'"; "\""; "\\"; "\n" ] -> ch
  13 |   | "\n" -> ""
  14 |   | ch -> "" ^ ch
  Alert zanuda-linter: Using `function` is recommended
