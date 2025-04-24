  $ dune build @check
  $ ../zanuda.exe -no-check-filesystem -no-eta_reduction -no-top_file_license -no-propose_function -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "manual_fold.ml", lines 3-6, characters 0-39:
  3 | let rec fold_left f acc l =
  4 |   match l with
  5 |   | [] -> acc
  6 |   | x :: xs -> fold_left f (f acc x) xs
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left`
  
  File "manual_fold.ml", lines 10-12, characters 0-39:
  10 | let rec fold_left f acc = function
  11 |   | [] -> acc
  12 |   | x :: xs -> fold_left f (f acc x) xs
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left`
  
  File "manual_fold.ml", lines 14-17, characters 0-40:
  14 | let rec fold_left1 f acc l =
  15 |   match l with
  16 |   | [] -> acc
  17 |   | x :: xs -> fold_left1 f (f acc x) xs
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left1`
  
  File "manual_fold.ml", lines 19-22, characters 0-41:
  19 | let rec fold_right f acc l =
  20 |   match l with
  21 |   | [] -> acc
  22 |   | x :: xs -> f x (fold_right f  acc xs)
  Alert zanuda-linter: Consider using `List.fold_right` instead of `fold_right`
