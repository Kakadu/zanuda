let x = true
let y = false

let f1 () = 
  if x then (if x then (if x then (if x then x else y) else y) else y) else y

let f2 () = 
  if (if x then (if x then (if x then x else y) else y) else y) then x else y

let f3 () = 
  if x then x else (if x then (if x then x else (if x then x else y)) else y)