let add x y = x + y
let rec even n = if n = 0 then true else odd (n - 1)
and odd n = if n = 0 then false else even (n - 1)

let not_a_function = 42

let partial = add

let local () = let nested x = x in nested

module Inner = struct
  let twice x = x + x
end

class counter = object
  val mutable n = 0
  method get = n
  method bump () = n <- n + 1
end
