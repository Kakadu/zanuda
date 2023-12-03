
let f1 x = if x then true else false

let f2 x = if x then false else true

let f3 = if true then false else true

let f4 x y = if false then x else y

let unwise_bool_exp x = x && true

let with_comments x = if x then true (* first comment*) else true (* second comment*)

