  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
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
  

