let f a b c =
 a ^ b ^ c

[@@@ocaml.warning "-5"]

let _ = List.fold_left (^)
let _ = ListLabels.fold_left ~f:(^)
