  $ dune build
$ dune describe
  $ mylinter -dir .
  File "exec1.ml", line 1, characters 13-32:
  1 | let __  xs = List.length xs <= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 2, characters 13-32:
  2 | let __  xs = 0 >= List.length xs
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 3, characters 13-32:
  3 | let __  xs = List.length xs >= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "exec1.ml", line 4, characters 13-32:
  4 | let __  xs = 0 <= List.length xs
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "Record1.ml", line 4, characters 11-34:
  4 | let f2 r = { x=r.x; y=r.y; z=r.z }
                 ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Rewrite record as 'r'
  File "Record1.ml", line 5, characters 11-33:
  5 | let f3 r = { x=r.x; y=r.y; z=18 }
                 ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Rewrite record as '{ r with z = 18 }'
  File "Record1.ml", line 6, characters 14-38:
  6 | let f4 r r2 = { x=r.x; y=r.y; z=r2.z }
                    ^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Rewrite record as '{ r with z = (r2.z) }'
  File "Record1.ml", line 7, characters 14-36:
  7 | let f5 r r2 = { x=r.x; y=1; z=r2.z }
                    ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Rewrite record as '{ r with z = (r2.z); y = 1 }'
  File "Parsetree.mli", line 2, characters 2-16:
  2 |   | App of t * t
        ^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
  File "list_len.ml", line 6, characters 12-31:
  6 | let __ xs = List.length xs <= 0
                  ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 8, characters 12-30:
  8 | let __ xs = List.length xs = 0
                  ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 10, characters 12-31:
  10 | let __ xs = List.length xs >= 0
                   ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 12, characters 12-30:
  12 | let __ xs = List.length xs > 0
                   ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "list_len.ml", line 14, characters 13-31:
  14 | let __ xs =  0 < List.length xs
                    ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "ListFusion.ml", line 1, characters 12-48:
  1 | let __ xs = List.map Fun.id (List.map Fun.id xs)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Performance issue. It's recommended to rewrite
  	'List.map f (List.map g xs)'
  as
  	'List.map (fun y -> f (g y)) xs'
  
  File "ListFusion.ml", line 2, characters 14-48:
  2 | let __ f xs = List.filter f (List.map Fun.id xs)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Performance issue. It's recommended to rewrite
  	'List.filter f (List.map g xs)'
  as
  	'List.filter_map (fun x -> let r = g x in if f r then Some y else None) xs'
  
  File "ListFusion.ml", line 3, characters 12-44:
  3 | let __ xs = List.concat (List.map Fun.id xs)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Performance issue. It's recommended to rewrite
  	'List.concat (List.map f xs)'
  as
  	'List.concat_map f xs'
  
  File "Ignore.ml", line 2, characters 2-29:
  2 |   ignore (List.map ((+)1) xs);
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Unsafe ingore. It's recommended to rewrite it as 'let (_: int list) = List.map ((+) 1) xs'
  File "Ignore.ml", line 6, characters 2-41:
  6 |   Base.ignore (List.map string_of_int xs)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Unsafe ingore. It's recommended to rewrite it as 'let (_: string list) = List.map string_of_int xs'
  File "Function.ml", line 1, characters 23-56:
  1 | let should_give_a_lint x = match x with [] -> 1 | _ -> 2
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Using `function` is recommended
  File "Failwith.ml", line 1, characters 12-20:
  1 | let foo _ = failwith "not implemented"
                  ^^^^^^^^
  Alert zanuda-linter: Using failwith unsafely
  File "ExcTryWithWildcard.ml", lines 2-3, characters 2-11:
  2 | ..try raise Not_found
  3 | with _ -> 1
  Alert zanuda-linter: Antipattern: error swallowing
