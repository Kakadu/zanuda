  $ dune build
  $ zanuda -dir . -diffs-with-fixes > /dev/null
  $ cat fix_gen/diffs.log
  Diffs for file Record1.ml
  6,8c6
  <   {x1 = point.x1 + 1;
  <   x2 = point.x2;
  <   x3 = point.x3}
  ---
  >   { point with x1 = (point.x1 + 1) } 
  13,18c11
  <   {
  <   pos_fname = point.pos_fname; (* first field*)
  <   pos_bol = point.pos_bol; (* second field*)
  <   pos_cnum = point.pos_cnum; (* third field*)
  <   pos_lnum = point.pos_lnum + x (* fourth field *)
  <   }
  ---
  >   { point with pos_lnum = (point.pos_lnum + x) }  (* first field*) (* second field*) (* third field*) (* fourth field *)
  
