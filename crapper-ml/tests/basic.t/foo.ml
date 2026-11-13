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

let complex xs n =
  let r =
    match xs with
    | [] -> None
    | x :: _ when x = n && n > 0 -> Some x
    | _ -> None
  in
  if r = None || n < 0 then None else r

let guarded f =
  try f () with
  | Failure _ -> 0
  | Exit -> 1

let loops sum =
  for i = 1 to 10 do sum := !sum + i done;
  while !sum < 100 do incr sum done;
  !sum

let andor a b c = a && b || c

let g x =
  match x with
  | y when y > 0 -> y
  | _ -> 0
