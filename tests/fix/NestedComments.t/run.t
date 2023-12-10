  $ dune build
  $ zanuda -dir . > /dev/null 
  $ cat fix_gen/diffs.log
  Diffs for file BugDemo.ml
  2c2
  < let f x = match (* lvl1 (* lvl2*)*) x with true -> 0 | false -> 1
  ---
  > let f  = function   (* lvl1 (* lvl2*)          true -> 0 | false -> 1
  4c4
  < let f1 x = if x (* (*nested_comment*) *) && true then x else false
  ---
  > let f1 x = if x (* (*nested_comment*) then x else false
  
