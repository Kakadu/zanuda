let f _ =
  try raise Not_found
with _ -> 1

let foo f =
  match f () with
  | [] -> 1
  | exception _ -> 2
  | _ -> 3
