(* [complex_copy] is an exact copy of [complex]: same code, same complexity.
   The test below calls only the copy, so the report compares the same code
   covered and uncovered. *)
let complex xs n =
  let r =
    match xs with
    | [] -> None
    | x :: _ when x = n && n > 0 -> Some x
    | _ -> None
  in
  if r = None || n < 0 then None else r

let complex_copy xs n =
  let r =
    match xs with
    | [] -> None
    | x :: _ when x = n && n > 0 -> Some x
    | _ -> None
  in
  if r = None || n < 0 then None else r
