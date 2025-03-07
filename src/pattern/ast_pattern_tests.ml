open Compenv

let translate filename =
  Compmisc.init_path ();
  let outputprefix = output_prefix filename in
  let modulename = module_of_filename filename outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env () in
  try
    let parsetree = Pparse.parse_implementation ~tool_name:"xxx" filename in
    let { Typedtree.structure = typedtree; _ } =
      Typemod.type_implementation filename outputprefix modulename env parsetree
    in
    parsetree, typedtree
  with
  | Typetexp.Error (_loc, env, e) as exc ->
    Typetexp.report_error env Format.std_formatter e;
    Format.printf "\n%!";
    raise exc
  | x -> raise x
;;

let extract_first =
  let open Parsetree in
  function
  | [ { pstr_desc = Pstr_value (_, [ vb ]) } ] -> vb.pvb_expr
  | _ -> assert false
;;

let run_string code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let parsetree, _ = translate filename in
  let expr = extract_first parsetree in
  Tast_pattern.parse pat Location.none expr sk ~on_error:(Printf.printf "ERROR: %s\n")
;;

let default_sk pats _cases =
  Format.printf
    "patterns: @[%a@]\n%!"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
       Pprintast.pattern)
    pats;
  print_endline "success"
;;

let%expect_test _ =
  let code = {| let f x y = function true -> 1 | false -> 0 |} in
  run_string code __LINE__ Tast_pattern.(pexp_function_cases __ __) default_sk;
  [%expect {|
    patterns: x y
    success|}]
;;

let%expect_test _ =
  let code = {| let f x y = function true -> 1 | false -> 0 |} in
  run_string
    code
    __LINE__
    Tast_pattern.(pexp_function_cases (as__ (drop ^:: drop ^:: nil)) __)
    default_sk;
  [%expect {|
    patterns: x y
    success |}]
;;

let () = ()
