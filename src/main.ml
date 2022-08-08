open Caml
open Base
open Zanuda_core
open Utils

let per_file_linters = [ (module UntypedLints.License : LINT.TYPED) ]

let untyped_linters =
  let open UntypedLints in
  [ (module Casing : LINT.UNTYPED)
  ; (module ParsetreeHasDocs : LINT.UNTYPED)
  ; (module ToplevelEval : LINT.UNTYPED)
  ; (module VarShouldNotBeUsed : LINT.UNTYPED)
  ]
;;

let typed_linters =
  let open TypedLints in
  [ (* * *********************** *)
    (module Failwith : LINT.TYPED)
  ; (module Hashtables : LINT.TYPED)
  ; (module ListLength : LINT.TYPED)
  ; (module ProposeFunction : LINT.TYPED)
  ; (module ExcTryWithWildcard : LINT.TYPED)
  ; (module Record1 : LINT.TYPED)
  ; (module Ignore : LINT.TYPED)
  ; (module ListFusion : LINT.TYPED)
  ; (module IfBool : LINT.TYPED)
  ; (module Equality : LINT.TYPED)
  ; (module StringConcat : LINT.TYPED)
  ; (module UntypedLints.Dollar : LINT.TYPED)
  ; (module UntypedLints.GuardInsteadOfIf : LINT.TYPED)
  ; (module MonadLaws : LINT.TYPED) (* * *********************** *)
  ]
;;

(* prepare for disabling some lints *)
let () =
  let h = Config.enabled_lints () in
  List.iter untyped_linters ~f:(fun (module L : LINT.UNTYPED) ->
    assert (not (Hash_set.mem h L.lint_id));
    Hash_set.add h L.lint_id);
  List.iter typed_linters ~f:(fun (module L : LINT.TYPED) ->
    assert (not (Hash_set.mem h L.lint_id));
    Hash_set.add h L.lint_id);
  List.iter per_file_linters ~f:(fun (module L : LINT.TYPED) ->
    assert (not (Hash_set.mem h L.lint_id));
    Hash_set.add h L.lint_id);
  ()
;;

let process_per_file_linters info parsetree =
  let hash = Config.enabled_lints () in
  let is_enabled = Hash_set.mem hash in
  List.iter per_file_linters ~f:(fun (module L : LINT.TYPED) ->
    if is_enabled L.lint_id
    then
      let open Tast_iterator in
      (L.run info default_iterator).structure default_iterator parsetree)
;;

(* TODO: Functions below are a little bit copy-pasty. Rework them *)
let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let untyped_on_structure info =
  let hash = Config.enabled_lints () in
  let is_enabled = Hash_set.mem hash in
  build_iterator
    ~f:(fun o -> o.Ast_iterator.structure o)
    ~compose:(fun (module L : LINT.UNTYPED) acc ->
      if is_enabled L.lint_id then L.run info acc else acc)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let untyped_on_signature info =
  let hash = Config.enabled_lints () in
  let is_enabled = Hash_set.mem hash in
  build_iterator
    ~f:(fun o -> o.Ast_iterator.signature o)
    ~compose:(fun (module L : LINT.UNTYPED) acc ->
      if is_enabled L.lint_id then L.run info acc else acc)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let typed_on_structure info =
  let hash = Config.enabled_lints () in
  let is_enabled = Hash_set.mem hash in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : LINT.TYPED) acc ->
      if is_enabled L.lint_id then L.run info acc else acc)
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
  if Config.verbose () then printfn "Analyzing cmt: %s" filename;
  (* Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree; *)
  with_info filename (fun info ->
    process_per_file_linters info typedtree;
    typed_on_structure info typedtree)
;;

let process_cmti_typedtree filename typedtree =
  (* if Config.Options.verbose ()
  then (
    let () = printfn "Analyzing cmti: %s" filename in
    printfn "%a" Printtyped.interface typedtree); *)
  (* Format.printf "Typedtree MLI:\n%a\n%!" Printtyped.interface typedtree; *)
  with_info filename (fun info -> typed_on_signature info typedtree)
;;

module Migr = Ppxlib_ast.Selected_ast.Of_ocaml

let process_untyped filename =
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
      untyped_on_structure info parsetree
    in
    let process_signature info =
      let parsetree = Compile_common.parse_intf info in
      untyped_on_signature info parsetree
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
      process_untyped file;
      CollectedLints.report ()
    | Dir path ->
      LoadDune.analyze_dir
        ~untyped:process_untyped
        ~cmt:process_cmt_typedtree
        ~cmti:process_cmti_typedtree
        path;
      CollectedLints.report ()
  in
  ()
;;