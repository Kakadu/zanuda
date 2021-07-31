external realpath_stub : string -> string = "caml_realpath"

let realpath s =
  let s0 =
    match Sys.getenv "TESTCASE_ROOT" with
    | prefix -> Str.global_replace (Str.regexp "TESTCASE_ROOT") prefix s
    | exception Not_found -> s
  in
  (* Format.printf "s0 = %s\n%!" s0; *)
  realpath_stub s0
;;
