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
  | Typecore.Error (_, _, _) as exc ->
    Location.report_exception Format.std_formatter exc;
    Format.printf "\n%!";
    raise_notrace
      (Failure (Printf.sprintf "Typecheck error at %s line %d" __FILE__ __LINE__))
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

let run_si code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let parsetree, _ = translate filename in
  let expr = List.hd parsetree in
  Tast_pattern.parse pat Location.none expr sk ~on_error:(Printf.printf "ERROR: %s\n")
;;

let run_string_typed ?(verbose = false) code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let _, ttree = translate filename in
  let expr = extract_first_typed ttree in
  if verbose then Format.printf "@[%a@]\n" Printtyped.implementation ttree;
  Tast_pattern.parse pat Location.none expr sk ~on_error:(Printf.printf "ERROR: %s\n")
;;

let run_si_typed code line pat sk =
  let filename = Printf.sprintf "tmp%d.ml" line in
  Out_channel.with_open_text filename (fun ch -> output_string ch code);
  let _, ttree = translate filename in
  let expr = List.hd ttree.Typedtree.str_items in
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
  [%expect
    {|
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
  [%expect
    {|
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
      Format.printf "Ident = %s\n%!" (Ident.name id);
      (match path with
       | Path.Pident id -> Format.printf "Ident = %s\n%!" (Ident.name id)
       | _ -> assert false);
      Format.printf "cases count = %d\n%!" (List.length cases);
      print_endline "OK");
  [%expect
    {|
    Ident = ch
    Ident = ch
    cases count = 3
    OK |}]
;;

let () = ()

let%expect_test _ =
  let code = {| let f x y = function true -> 1 | false -> 0 |} in
  run_string code __LINE__ Tast_pattern.(pexp_function_cases (list __) __) default_sk;
  [%expect
    {|

    patterns: x y
    success|}];
  let code = {| let f x y = x+y |} in
  run_string
    code
    __LINE__
    Tast_pattern.(pexp_function_body drop (pexp_apply drop (list (drop ** __))))
    (fun args ->
      Format.printf
        "%a%!"
        Format.(
          pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Pprintast.expression)
        args
      (* List.iter (Format.printf "%a\n%!" Pprintast.expression) args *));
  [%expect
    {|

    x y|}]
;;

let%expect_test "Parse typed zanuda attribute" =
  let code = {| [@@@zanuda "asdf"] |} in
  run_si_typed code __LINE__ Tast_pattern.(tstr_zanuda_attr __) (fun s -> print_endline s);
  [%expect {| asdf |}]
;;

let%expect_test "List.fold circumflex" =
  let code = {| let foo  = List.fold_left (^)  |} in
  run_string_typed
    (* ~verbose:true *)
    code
    __LINE__
    Tast_pattern.(
      let list_fold = texp_ident_typ (path [ "Stdlib"; "List"; "fold_left" ]) drop in
      let concat_op =
        let typ_str =
          typ_constr (path [ "Stdlib"; "string" ] ||| path [ "string" ]) nil
        in
        texp_ident_typ
          (path [ "Stdlib"; "^" ])
          (typ_arrow typ_str (typ_arrow typ_str typ_str))
      in
      texp_apply list_fold ((nolabel ** arg concat_op) ^:: __))
    (fun _ -> print_endline "OK");
  [%expect "OK"]
;;

open Tast_pattern

let path_of_list = function
  | [] -> failwith "Bad argument: path_of_list"
  | s :: tl ->
    ListLabels.fold_left
      tl
      ~init:(Path.Pident (Ident.create_local s))
      ~f:(fun acc x -> Path.Pdot (acc, x))
;;

let%test_module "Pasing List.length" =
  (module struct
    [@@@coverage off]

    let names = [ "Stdlib!"; "List"; "length" ]

    [%%if ocaml_version < (5, 0, 0)]

    let pp_path = Path.print

    [%%else]

    let pp_path = Format_doc.compat Path.print

    [%%endif]

    let%test "Check List.length has the path we expect" =
      (* Previously we used here ppx_assert that can print the values that are not equal,
      but for the sake of reducting deps count it is removed *)
      let old = !Clflags.unique_ids in
      Clflags.unique_ids := false;
      let ans =
        String.equal
          "Stdlib!.List.length"
          (Format.asprintf "%a" pp_path (path_of_list names))
      in
      Clflags.unique_ids := old;
      ans
    ;;

    let%test "parse List.length" =
      let noloc =
        Warnings.
          { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true }
      in
      parse (path names) noloc ~on_error:(fun _ -> false) (path_of_list names) true
    ;;
  end)
;;

let%test_module "Fake tests, only to increase coverage" =
  (module struct
    [@@@coverage off]

    let noloc =
      Warnings.
        { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true }
    ;;

    let%test _ =
      match path_of_list [] with
      | exception Failure _ -> true
      | _ -> false
    ;;

    let mk p ?(inv = false) what =
      let on_error, rez = if inv then Fun.const true, false else Fun.const false, true in
      parse p noloc ~on_error what rez
    ;;

    let%test _ =
      Bool.equal
        true
        (mk
           ~inv:true
           (path_pident drop)
           Path.(Pdot (Pident (Ident.create_local "List"), "map")))
    ;;

    let%test _ = mk (path_pident drop) Path.(Pident (Ident.create_local "compare"))
    let%test _ = mk ~inv:true (labelled drop) Asttypes.Nolabel

    let%test _ =
      mk
        (econst drop)
        { Typedtree.exp_desc = Texp_constant (Asttypes.Const_string ("", noloc, None))
        ; exp_extra = []
        ; exp_type = Predef.type_string
        ; exp_loc = noloc
        ; exp_env = Env.empty
        ; exp_attributes = []
        }
    ;;

    let%test _ =
      let _42 =
        { Typedtree.exp_desc = Texp_constant (Asttypes.Const_int 42)
        ; exp_extra = []
        ; exp_type = Predef.type_int
        ; exp_loc = noloc
        ; exp_env = Env.empty
        ; exp_attributes = []
        }
      in
      mk (econst drop) _42
    ;;
  end)
;;
