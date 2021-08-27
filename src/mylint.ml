open Caml
open Base
open Zanuda_core
open Utils

let untyped_linters =
  let open UntypedLints in
  [ (module GuardInsteadOfIf : LINT.UNTYPED)
  ; (module Casing : LINT.UNTYPED)
  ; (module ParsetreeHasDocs : LINT.UNTYPED)
  ; (module ToplevelEval : LINT.UNTYPED)
  ]
;;

let typed_linters =
  let open TypedLints in
  [ (* * *********************** *)
    (module Failwith : LINT.TYPED)
  ; (module ListLength : LINT.TYPED)
  ; (module ProposeFunction : LINT.TYPED)
  ; (module ExcTryWithWildcard : LINT.TYPED)
  ; (module Record1 : LINT.TYPED)
  ; (module Ignore : LINT.TYPED)
  ; (module ListFusion : LINT.TYPED)
    (* * *********************** *)
  ]
;;

(* TODO: Functions below are a little bit copy-pasty. Rework them *)
let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let untyped_on_structure info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.structure o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let untyped_on_signature info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.signature o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let typed_on_structure info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let typed_on_signature info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let with_info filename f =
  Compile_common.with_info
    ~native:false
    ~source_file:filename
    ~tool_name:"asdf" (* TODO: pass right tool name *)
    ~output_prefix:"asdf"
    ~dump_ext:"asdf"
    f
;;

let process_cmt_typedtree filename typedtree =
  (* if Config.Options.verbose () then printfn "Analyzing cmt: %s" filename; *)
  with_info filename (fun info -> typed_on_structure info typedtree)
;;

let process_cmti_typedtree filename typedtree =
  (* if Config.Options.verbose ()
  then (
    let () = printfn "Analyzing cmti: %s" filename in
    printfn "%a" Printtyped.interface typedtree); *)
  with_info filename (fun info -> typed_on_signature info typedtree)
;;

let load_file filename =
  Clflags.error_style := Some Misc.Error_style.Contextual;
  Clflags.include_dirs := Config.includes () @ Clflags.include_dirs.contents;
  let with_info f =
    Compile_common.with_info
      ~native:false
      ~source_file:filename
      ~tool_name:"asdf" (* TODO: pass right tool name *)
      ~output_prefix:"asdf"
      ~dump_ext:"asdf"
      f
  in
  let () =
    let process_structure info =
      let parsetree = Compile_common.parse_impl info in
      untyped_on_structure info parsetree;
      try
        (* let typedtree, _ = Compile_common.typecheck_impl info parsetree in
        typed_on_structure info typedtree;  *)
        ()
      with
      | Env.Error e ->
        Caml.Format.eprintf "%a\n%!" Env.report_error e;
        Caml.exit 1
    in
    let process_signature info =
      let parsetree = Compile_common.parse_intf info in
      untyped_on_signature info parsetree;
      let typedtree = Compile_common.typecheck_intf info parsetree in
      typed_on_signature info typedtree
    in
    with_info (fun info ->
        if String.is_suffix info.source_file ~suffix:".ml"
        then process_structure info
        else if String.is_suffix info.source_file ~suffix:".mli"
        then process_signature info
        else (
          let () =
            Caml.Format.eprintf
              "Don't know to do with file '%s'\n%s %d\n%!"
              info.source_file
              Caml.__FILE__
              Caml.__LINE__
          in
          Caml.exit 1))
  in
  ()
;;

let () =
  Config.parse_args ();
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dump filename ->
      let info =
        List.concat
          [ List.map untyped_linters ~f:(fun (module L : LINT.UNTYPED) ->
                L.describe_itself ())
          ; List.map typed_linters ~f:(fun (module L : LINT.TYPED) ->
                L.describe_itself ())
          ]
      in
      let ch = Caml.open_out filename in
      Exn.protect
        ~f:(fun () -> Yojson.Safe.pretty_to_channel ~std:true ch (`List info))
        ~finally:(fun () -> Caml.close_out ch);
      Caml.exit 0
    | File file ->
      load_file file;
      CollectedLints.report ()
    | Dir path ->
      PerDictionary.analyze_dir
        load_file
        process_cmt_typedtree
        process_cmti_typedtree
        path;
      CollectedLints.report ()
  in
  ()
;;
