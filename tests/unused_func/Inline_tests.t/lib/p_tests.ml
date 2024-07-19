open Wrapped_lib.Parser
open Wrapped_lib.Ast

(* PARSERS *)
let show_wrap form = function
  | Some x -> form Format.std_formatter x
  | _ -> print_endline "Parsing error"
;;

let print_pars ps pp str = show_wrap pp (parse_to_some ps str)

(* statements tests*)

let test_decl = print_pars s_declaration pp_statement

let%expect_test _ =
  print_endline "Hello, world!";
  [%expect "Hello, world!"]
;;
