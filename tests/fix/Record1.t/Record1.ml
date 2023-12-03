

type coordinates = {x1: int; x2: int; x3: int}

let increase_x1 point = 
  {x1 = point.x1 + 1;
  x2 = point.x2;
  x3 = point.x3}

open Lexing

let shift_pos_line point x = 
  {
  pos_fname = point.pos_fname; (* first field*)
  pos_bol = point.pos_bol; (* second field*)
  pos_cnum = point.pos_cnum; (* third field*)
  pos_lnum = point.pos_lnum + x (* fourth field *)
  }
