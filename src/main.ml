[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Zanuda_core
open Utils

let per_file_linters = [ (module TypedLints.License : LINT.TYPED) ]

let untyped_linters =
  let open UntypedLints in
  [ (module Casing : LINT.UNTYPED)
  ; (module UntypedLints.Dollar : LINT.UNTYPED)
  ; (module Expect_names : LINT.UNTYPED)
  ; (module Toplevel_eval : LINT.UNTYPED)
    (* ; (module UntypedLints.Propose_function : LINT.UNTYPED) *)
  ; (module Var_should_not_be_used : LINT.UNTYPED)
  ]
;;

let typed_linters =
  let open TypedLints in
  [ (* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)
    (module Aggregate_defs : LINT.TYPED)
  ; (module Ambiguous_constructors : LINT.TYPED)
  ; (module Exc_try_with_wildcard : LINT.TYPED)
  ; (module Failwith : LINT.TYPED)
  ; (module Equality : LINT.TYPED)
  ; (module Equality_phys : LINT.TYPED)
  ; (module Eta : LINT.TYPED)
  ; (module Guard_instead_of_if : LINT.TYPED)
  ; (module Hashtables : LINT.TYPED)
  ; (module If_bool : LINT.TYPED)
  ; (module Ignore : LINT.TYPED)
  ; (module List_fusion : LINT.TYPED)
  ; (module List_length : LINT.TYPED)
  ; (module Manual_fold : LINT.TYPED)
  ; (module Manual_map : LINT.TYPED)
  ; (module Match_Bool : LINT.TYPED)
  ; (module Monad_laws : LINT.TYPED)
  ; (module Mutually_rec_types : LINT.TYPED)
  ; (module Nested_if : LINT.TYPED)
  ; (module Parsetree_has_docs : LINT.TYPED)
  ; (module Printf : LINT.TYPED)
  ; (module Propose_function : LINT.TYPED)
  ; (module Record_punning : LINT.TYPED)
  ; (module String_concat : LINT.TYPED)
  ; (module String_concat_fold : LINT.TYPED)
  ; (module Tuple_matching : LINT.TYPED)
    (* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)
  ]
;;

(* prepare for disabling some lints *)
let () =
  let enabled = Config.enabled_lints () in
  let all = Config.all_lints () in
  assert (Hash_set.length enabled = 0);
  assert (Hash_set.length all = 0);
  List.iter untyped_linters ~f:(fun (module L : LINT.UNTYPED) ->
    Hash_set.add all L.lint_id;
    if not (String.equal L.lint_id UntypedLints.Toplevel_eval.lint_id)
    then Hash_set.add enabled L.lint_id);
  List.iter typed_linters ~f:(fun (module L : LINT.TYPED) ->
    (* Format.printf "   ENABLE %s\n%!" L.lint_id; *)
    Hash_set.add all L.lint_id;
    Hash_set.add enabled L.lint_id);
  List.iter per_file_linters ~f:(fun (module L : LINT.TYPED) ->
    Hash_set.add all L.lint_id;
    Hash_set.add enabled L.lint_id);
  ()
;;

(* TODO(Kakadu): Functions below are a little bit copy-pasty. Rework them *)
let process_per_file_linters_str info parsetree =
  let is_enabled = Config.is_enabled () in
  List.iter per_file_linters ~f:(fun ((module L : LINT.TYPED) as lint) ->
    if is_enabled (lint :> (module LINT.GENERAL))
    then
      let open Tast_iterator in
      (L.run info default_iterator).structure default_iterator parsetree)
;;

let process_per_file_linters_sig info parsetree =
  let is_enabled = Config.is_enabled () in
  List.iter per_file_linters ~f:(fun ((module L : LINT.TYPED) as lint) ->
    if is_enabled (lint :> (module LINT.GENERAL))
    then
      let open Tast_iterator in
      (L.run info default_iterator).signature default_iterator parsetree)
;;

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

let run_typed_lints entry info =
  let is_enabled = Config.is_enabled () in
  build_iterator
    ~f:entry
    ~compose:(fun ((module L : LINT.TYPED) as lint) acc ->
      if is_enabled (lint :> (module LINT.GENERAL))
      then L.run info acc
      else (
        let __ () = Format.printf "%s is disabled\n%!" L.lint_id in
        acc))
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let typed_on_structure = run_typed_lints (fun o -> o.Tast_iterator.structure o)
let typed_on_signature = run_typed_lints (fun o -> o.Tast_iterator.signature o)

let process_cmt_typedtree _is_wrapped filename typedtree =
  if Config.verbose ()
  then
    printfn "process_cmt_typedtree cmt: %s" filename
    (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree*);
  Utils.(with_info Impl) filename (fun info ->
    process_per_file_linters_str info typedtree;
    typed_on_structure info typedtree)
;;

let process_cmti_typedtree _is_wrapped filename typedtree =
  Utils.(with_info Intf) filename (fun info ->
    process_per_file_linters_sig info typedtree;
    typed_on_signature info typedtree)
;;

let find_unused_in_cmti_typedtree is_wrapped filename typedtree =
  (* Format.printf "find_unused_in_cmti_typedtree %s\n%!" filename; *)
  (* Format.printf "tree:\n%a" Printtyped.interface typedtree; *)
  Collected_decls.collect_from_mli_tree is_wrapped filename typedtree
;;

let find_unused_in_cmt_typedtree is_wrapped filename typedtree =
  let _ : string = filename in
  (* Format.printf "find_unused_in_cmt_typedtree %s\n" filename; *)
  let it = Unused_ML_logger.run is_wrapped filename Tast_iterator.default_iterator in
  it.Tast_iterator.structure it typedtree
;;

let process_untyped filename =
  if not (Stdlib.Sys.file_exists filename)
  then Format.eprintf "Error: file %s doesn't exist. Continuing\n%!" filename
  else (
    Clflags.error_style := Some Misc.Error_style.Contextual;
    Clflags.include_dirs := Config.includes () @ Clflags.include_dirs.contents;
    let process_structure info =
      let parsetree = Compile_common.parse_impl info in
      untyped_on_structure info parsetree
    in
    let process_signature info =
      let parsetree = Compile_common.parse_intf info in
      untyped_on_signature info parsetree
    in
    if String.is_suffix filename ~suffix:".ml"
    then Utils.(with_info Impl) filename (fun info -> process_structure info)
    else if String.is_suffix filename ~suffix:".mli"
    then Utils.(with_info Intf) filename (fun info -> process_signature info)
    else (
      let () =
        Stdlib.Format.eprintf
          "Don't know to do with file '%s'\n%s %d\n%!"
          filename
          __FILE__
          __LINE__
      in
      exit 1))
;;

let () =
  let config_filename = ".zanuda" in
  if Stdlib.Sys.file_exists config_filename
  then (
    let s = In_channel.with_open_text config_filename In_channel.input_all in
    String.split s ~on:'\n'
    |> List.iter ~f:(fun s ->
      let s = Stdlib.String.trim s in
      if String.is_empty s
      then ()
      else if String.is_prefix s ~prefix:"-no-"
      then (
        let lint_name = String.chop_prefix_exn s ~prefix:"-no-" in
        Config.(Hash_set.remove opts.enabled_lints lint_name))
      else (
        match String.split ~on:' ' s with
        | "forward" :: lint_id :: rest
          when String.equal lint_id TypedLints.Hashtables.lint_id ->
          TypedLints.Hashtables.process_switches rest
        | "forward" :: lint_id :: rest
          when String.equal lint_id TypedLints.Equality_phys.lint_id ->
          TypedLints.Equality_phys.process_switches rest
        | _ -> Format.eprintf ".zanuda: Don't know what to do with %S\n%!" s)))
;;

let () =
  Config.parse_args ();
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dump_text ->
      let f (module L : LINT.GENERAL) =
        Format.printf "\n\n## Lint '%s'\n\n%s\n" L.lint_id L.documentation
      in
      List.iter ~f (untyped_linters :> (module LINT.GENERAL) list);
      List.iter ~f (typed_linters :> (module LINT.GENERAL) list);
      exit 0
    | Dump_json filename ->
      let info =
        List.concat
          [ List.map untyped_linters ~f:(fun (module L : LINT.UNTYPED) ->
              L.lint_id, L.describe_as_json ())
          ; List.map (per_file_linters @ typed_linters) ~f:(fun (module L : LINT.TYPED) ->
              L.lint_id, L.describe_as_json ())
          ; [ Lint_filesystem.lint_id, Lint_filesystem.describe_as_json () ]
          ]
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
        |> List.map ~f:snd
      in
      Out_channel.with_open_text filename (fun ch ->
        Yojson.Safe.pretty_to_channel ~std:true ch (`List info));
      Stdlib.exit 0
    | File file ->
      process_untyped file;
      Collected_lints.report ();
      if Config.gen_replacements () then Replacement.Refill.apply_all ()
    | Dir path ->
      Load_dune.analyze_dir
        ~untyped:process_untyped
        ~cmt:process_cmt_typedtree
        ~cmti:process_cmti_typedtree
        path;
      Collected_lints.report ();
      if Config.gen_replacements () then Replacement.Refill.apply_all ()
    | UnusedDecls path ->
      Load_dune.analyze_dir
        ~untyped:process_untyped
        ~cmt:find_unused_in_cmt_typedtree
        ~cmti:find_unused_in_cmti_typedtree
        path;
      if Config.verbose ()
      then (
        Collected_decls.print_all_decls ();
        Collected_decls.print_used_decls ());
      Collected_decls.collect_unused ()
    | Fix path -> Replacement.Log.promote path
  in
  ()
;;
