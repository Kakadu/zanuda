let f x y = 
  match x with
  | true -> y
  | false -> y - 1

let fix_required x y = 
  match y with
  | true -> x
  |false -> x - 1

let with_comments x = 
  match(*some comment*) x with true -> 0 | false -> 1 (* another comment *)

let without_comments x = 
  match x with "(* [comment] *)" -> 0 | "{| [comment] |}" -> 1 | _ -> 2
