let first = let open Simple in meow;;
let second = let open Simple in Meow.InnerMeow.inner;;

let%test _ = Simple.is_space ' '
(* let%expect_test _ =
  Printf.printf "%b\n" (Simple.is_space ' ');
  [%expect "true"] *)