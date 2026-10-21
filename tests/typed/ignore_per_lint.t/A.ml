let nested x y =
  if x then (if x then (if x then (if x then x else y) else y) else y) else y

let if_bool x y = if x then y else false
let phys x y = x == y
