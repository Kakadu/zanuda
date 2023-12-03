  $ dune build
  $ zanuda -dir . > /dev/null
  $ cat fix_gen/diffs.log
  
  Diffs for file IfBool.ml
  2c2
  < let f1 x = if x then true else false
  ---
  > let f1 x = x
  4c4
  < let f2 x = if x then false else true
  ---
  > let f2 x = not  x
  6c6
  < let f3 = if true then false else true
  ---
  > let f3 = false
  8c8
  < let f4 x y = if false then x else y
  ---
  > let f4 x y = y
  10c10
  < let unwise_bool_exp x = x && true
  ---
  > let unwise_bool_exp x = x
  12c12
  < let with_comments x = if x then true (* first comment*) else true (* second comment*)
  ---
  > let with_comments x =  (* first comment*)true (* second comment*)
  
