open Compenv

[%%if ocaml_version < (5, 0, 0)]

let type_implementation source_file =
  let outputprefix = output_prefix source_file in
  let modulename = "Module1" in
  Env.set_unit_name modulename;
  Typemod.type_implementation source_file outputprefix modulename
;;

[%%else]

let type_implementation file =
  Typemod.type_implementation Unit_info.(make ~source_file:file Impl "")
;;

[%%endif]

let translate filename =
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  try
    let parsetree = Pparse.parse_implementation ~tool_name:"xxx" filename in
    let { Typedtree.structure = typedtree; _ } =
      type_implementation filename env parsetree
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

let extract_first_typed =
  let open Typedtree in
  fun str ->
    match str.str_items with
    | [ { str_desc = Tstr_value (_, [ vb ]) } ] -> vb.vb_expr
    | _ -> assert false
;;

let run_string code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let parsetree, _ = translate filename in
  let expr = extract_first parsetree in
  Tast_pattern.parse pat Location.none expr sk ~on_error:(Printf.printf "ERROR: %s\n")
;;

let run_string_typed code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let _, ttree = translate filename in
  let expr = extract_first_typed ttree in
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

let%expect_test _ =
  let code =
    {|
let backslash = fun ch ->
  match ch with
  | ch when List.mem ch [ "$"; "'"; "\""; "\\"; "\n" ] -> ch
  | "\n" -> ""
  | ch -> "" ^ ch
|}
  in
  run_string_typed
    code
    __LINE__
    Tast_pattern.(
      texp_function_body
        ((nolabel ** __) ^:: nil)
        (as__ (texp_match (texp_ident_loc __) drop __)))
    (fun (id, _) _ _ path cases ->
      Format.printf "Ident = %a\n%!" Ident.print id;
      (match path with
       | Path.Pident id -> Format.printf "Ident = %a\n%!" Ident.print id
       | _ -> assert false);
      Format.printf "cases count = %d\n%!" (List.length cases);
      print_endline "OK");
  [%expect {|
    Ident = ch/269
    Ident = ch/269
    cases count = 3
    OK |}]
;;

let () = ()
