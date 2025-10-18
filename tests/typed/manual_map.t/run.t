  $ dune build
  $ ../zanuda.exe  -no-check-filesystem -no-top_file_license -no-propose_function -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "manual_map.ml", lines 2-4, characters 0-31:
  2 | let rec map1 f = function
  3 |   | [] -> []
  4 |   | h :: tl -> f h :: map1 f tl
  Alert zanuda-linter: Consider using `List.map` instead of `map1`
  File "manual_map.ml", lines 7-9, characters 0-12:
  7 | let rec map2 f = function
  8 |   | x :: xs -> f x :: map2 f xs
  9 |   | [] -> []
  Alert zanuda-linter: Consider using `List.map` instead of `map2`
  File "manual_map.ml", lines 12-14, characters 0-31:
  12 | let rec map3 = function
  13 |   | [] -> []
  14 |   | h :: tl -> h + 1 :: map3 tl
  Alert zanuda-linter: Consider using `List.map` instead of `map3`
  File "manual_map.ml", lines 16-18, characters 0-31:
  16 | let rec map4 f l = match l with
  17 |   | [] -> []
  18 |   | x :: xs -> f x :: map4 f xs
  Alert zanuda-linter: Consider using `List.map` instead of `map4`
  File "manual_map.ml", lines 27-29, characters 2-14:
  27 | ..let rec map = function
  28 |     | x :: xs -> Int.to_string x :: map xs
  29 |     | [] -> []
  Alert zanuda-linter: Consider using `List.map` instead of `map`
