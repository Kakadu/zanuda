[@@@ocaml.warnerror "-33"]
[@@@ocaml.warning "-33"]

open Only_mli_lib.A

let _ =
  let open Only_mli_lib.B in
  Printf.printf "my_id 5 = %d\n" (my_id 5)
