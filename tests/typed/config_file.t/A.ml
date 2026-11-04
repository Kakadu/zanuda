let nested x y =
  if x then (if x then (if x then (if x then x else y) else y) else y) else false

let phys x y = x == y
