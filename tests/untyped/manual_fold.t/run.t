  $ dune build
  $ zanuda  -no-check-filesystem -no-eta_reduction -no-top_file_license -no-propose_function_untyped -no-propose_function -dir .  -ordjsonl /dev/null
  File "manual_fold.ml", lines 2-5, characters 0-39:
  2 | let rec fold_left f acc l =
  3 |   match l with
  4 |   | [] -> acc
  5 |   | x :: xs -> fold_left f (f acc x) xs
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left`
  File "manual_fold.ml", lines 8-11, characters 0-40:
   8 | let rec fold_right f l acc =
   9 |   match l with
  10 |   | [] -> acc
  11 |   | x :: xs -> f x (fold_right f xs acc)
  Alert zanuda-linter: Consider using `List.fold_right` instead of `fold_right`
  File "manual_fold.ml", lines 16-18, characters 0-39:
  16 | let rec fold_right1 acc = function
  17 |   | [] -> acc
  18 |   | x :: xs -> f x (fold_right1 acc xs)
  Alert zanuda-linter: Consider using `List.fold_right` instead of `fold_right1`
  File "manual_fold.ml", lines 21-24, characters 0-40:
  21 | let rec fold_left1 f acc l =
  22 |   match l with
  23 |   | [] -> acc
  24 |   | x :: xs -> fold_left1 f (f acc x) xs
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left1`
  File "manual_fold.ml", lines 27-29, characters 0-13:
  27 | let rec fold_right2 acc = function
  28 |   | x :: xs -> f x (fold_right2 acc xs)
  29 |   | [] -> acc
  Alert zanuda-linter: Consider using `List.fold_right` instead of `fold_right2`
  File "manual_fold.ml", lines 32-34, characters 0-13:
  32 | let rec fold_left2 acc = function
  33 |   | x :: xs -> fold_left2 (f acc x) xs
  34 |   | [] -> acc
  Alert zanuda-linter: Consider using `List.fold_left` instead of `fold_left2`
  File "manual_fold.ml", lines 38-40, characters 2-34:
  38 | ..let rec helper acc = function
  39 |     | [] -> acc 
  40 |     | x :: xs -> x + helper acc xs
  Alert zanuda-linter: Consider using `List.fold_right` instead of `helper`
