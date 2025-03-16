[@@@ocaml.warning "-11"]

let _ =
    match 5 with
    | n -> if n>0 then 1 else 2
    | _ -> 3

(* I don't remember why this is needed  *)
