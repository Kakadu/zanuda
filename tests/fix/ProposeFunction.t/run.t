  $ dune build
  $ zanuda -dir . > /dev/null 
  $ cat fix_gen/diffs.log
  Diffs for file ProposeFunction.ml
  6,8c6,8
  < let fix_required x y = 
  <   match y with
  <   | true -> x
  ---
  > let fix_required x  = 
  >   function        
  >     true -> x
  11,12c11,12
  < let with_comments x = 
  <   match(*some comment*) x with true -> 0 | false -> 1 (* another comment *)
  ---
  > let with_comments  = 
  >   function  (*some comment*)        true -> 0 | false -> 1 (* another comment *)
  14,15c14,15
  < let without_comments x = 
  <   match x with "(* [comment] *)" -> 0 | "{| [comment] |}" -> 1 | _ -> 2
  ---
  > let without_comments  = 
  >   function         "(* [comment] *)" -> 0 | "{| [comment] |}" -> 1 | _ -> 2
  
