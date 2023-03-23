(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "var_should_not_be_used"
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Report identifier starting with '_' and used later

### Why is this bad?
OCaml compiler has a tendency to report warning 26 about unused variables. Usually this warning could be supressed by adding '_' in the beginning of identifier to make it look like wildcard variable. But if that identifier is used later it contradicts the purpose of adding undescore in the beginnning.
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

type input = Ast_iterator.iterator

open Ast_iterator

let msg ppf name =
  fprintf ppf "Identifier `%s` used somewhere else but supposed to be unused." name
;;

let report ~loc ~filename typ_name =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg typ_name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        typ_name
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

exception Found

let occurs_check name =
  let fallback = Ast_iterator.default_iterator in
  { fallback with
    expr =
      (fun self e ->
        let open Parsetree in
        match e.pexp_desc with
        | Pexp_ident { txt = Lident s } when String.equal s name -> raise Found
        | _ -> fallback.expr self e)
  ; pat =
      (fun self p ->
        match p.ppat_desc with
        | Ppat_var { txt } when String.equal txt name -> ()
        | _ -> fallback.pat self p)
  ; value_binding =
      (fun self vb ->
        match vb.pvb_pat.ppat_desc with
        | Ppat_var { txt } when String.equal txt name -> ()
        | _ -> fallback.value_binding self vb)
  }
;;

let check_occurances_exn txt e =
  let it = occurs_check txt in
  it.expr it e
;;

let is_name_suspicious txt =
  (not (String.is_prefix txt ~prefix:"_menhir_action_"))
  && (not (String.is_prefix txt ~prefix:"_menhir_cell"))
  && (not (String.is_prefix txt ~prefix:"_menhir_lexer"))
  && String.is_prefix txt ~prefix:"_"
;;

let run { Compile_common.source_file; _ } fallback =
  { fallback with
    expr =
      (fun self expr ->
        let () =
          match expr.pexp_desc with
          | Pexp_fun (_, _, ({ ppat_desc = Ppat_var { txt } } as pat), ebody)
            when is_name_suspicious txt ->
            (try check_occurances_exn txt ebody with
             | Found ->
               let loc = pat.ppat_loc in
               CollectedLints.add ~loc (report ~loc ~filename:source_file txt))
          | Pexp_let (_, [ ({ pvb_pat = { ppat_desc = Ppat_var { txt } } } as vb) ], ebody)
            when is_name_suspicious txt ->
            (try check_occurances_exn txt ebody with
             | Found ->
               let loc = vb.pvb_pat.ppat_loc in
               CollectedLints.add ~loc (report ~loc ~filename:source_file txt))
          | _ -> ()
        in
        fallback.expr self expr)
  ; structure =
      (fun self x ->
        let open Parsetree in
        let loop_vb wher vb =
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt; loc } when is_name_suspicious txt ->
            (try
               let it = occurs_check txt in
               it.expr it vb.pvb_expr;
               List.iter ~f:(it.structure_item it) wher
             with
             | Found -> CollectedLints.add ~loc (report ~loc ~filename:source_file txt))
          | _ ->
            (* TODO: support Ppat_as ... *)
            ()
        in
        let rec loop_str = function
          | [] -> ()
          | h :: tl ->
            let () =
              match h.pstr_desc with
              | Pstr_value (_, vbs) -> List.iter ~f:(loop_vb tl) vbs
              | _ -> ()
            in
            loop_str tl
        in
        loop_str x;
        fallback.structure self x)
  }
;;
