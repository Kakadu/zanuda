[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Utils

type input = Tast_iterator.iterator

let lint_id = "var_should_not_be_used"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
Report identifier starting with '_' and used later

### Why is this bad?
OCaml compiler has a tendency to report warning 26 about unused variables.
Usually this warning could be supressed by adding '_' in the beginning of identifier to make it look like wildcard variable.
But if that identifier is used later it contradicts the purpose of adding undescore in the beginnning.
  |}
  |> String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~impl:LINT.Untyped ~docs:documentation
;;

let msg ppf =
  Format.fprintf ppf "Identifier `%s` used somewhere else but supposed to be unused."
;;

let report ~loc ~filename ident =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg (Ident.name ident)

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        (Ident.name ident)
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let has_occurences txt e = Utils.has_ident txt (fun it -> it.expr it e)

let is_name_suspicious txt =
  (* TODO(Kakadu): Invent better solution to deal with menhir generated files. *)
  String.starts_with txt ~prefix:"_"
  && (not (String.equal txt "_startpos"))
  && (not (String.equal txt "_endpos"))
  && (not (String.equal txt "_2"))
  && (not (String.equal txt "_tok"))
  && (not (String.equal txt "_v"))
  && (not (String.starts_with txt ~prefix:"___bisect"))
  && (not (String.starts_with txt ~prefix:"__ocaml_lex"))
  && not (String.starts_with txt ~prefix:"_menhir_")
;;

let run info (fallback : Tast_iterator.iterator) =
  let source_file = Tast_pattern.source_of_info info in
  { fallback with
    expr =
      (fun self expr ->
        fallback.expr self expr;
        Tast_pattern.(
          parse
            (texp_function_body __ __
             |> map2 ~f:(fun pats expr -> `Fbody (pats, expr))
             ||| (texp_function_cases __ __
                  |> map2 ~f:(fun args cases -> `Fcases (args, cases)))
             ||| (texp_let (value_binding (tpat_id __') __ ^:: nil) __
                  |> map3 ~f:(fun pat pat_rhs expr -> `Let1 (pat, pat_rhs, expr)))))
          expr.exp_loc
          expr
          ~on_error:(fun _ () -> ())
          (fun founds () ->
            match founds with
            | `Fbody (args, ebody) ->
              List.iter
                (fun (_, (txt, loc)) ->
                  if is_name_suspicious (Ident.name txt)
                  then (
                    if has_occurences txt ebody then
                      Collected_lints.add ~loc (report ~loc ~filename:source_file txt)))
                args
            | `Fcases (args, cases) ->
              List.iter
                (fun (_, (txt, loc)) ->
                  if is_name_suspicious (Ident.name txt)
                  then
                    List.iter
                      (fun case ->
                        if has_occurences txt case.Typedtree.c_rhs then
                          Collected_lints.add ~loc (report ~loc ~filename:source_file txt))
                      cases)
                args
            | `Let1 (argid, _rhs, wher) when is_name_suspicious (Ident.name argid.txt) ->
              (if has_occurences argid.txt wher then
                 let loc = argid.loc in
                 Collected_lints.add ~loc (report ~loc ~filename:source_file argid.txt))
            | _ -> ())
          ())
  ; structure =
      (fun self x ->
        let loop_vb wher vb =
          Tast_pattern.(parse (tpat_id __))
            vb.Typedtree.vb_pat.pat_loc
            vb.Typedtree.vb_pat
            ~on_error:(fun _ -> ())
            (fun id ->
              if is_name_suspicious (Ident.name id)
              then (
                try
                  let it = Utils.no_ident_iterator id in
                  it.expr it vb.vb_expr;
                  List.iter (it.structure_item it) wher
                with
                | Utils.Ident_is_found ->
                  let loc = vb.Typedtree.vb_pat.pat_loc in
                  Collected_lints.add ~loc (report ~loc ~filename:source_file id)))
          (* match vb.Typedtree.vb_pat.pat_desc with
             | Tpat_var (id, _) when is_name_suspicious (Ident.name id) ->
             (try
             let it = Utils.no_ident_iterator id in
             it.expr it vb.vb_expr;
             List.iter (it.structure_item it) wher
             with
             | Utils.Ident_is_found ->
             let loc = vb.Typedtree.vb_pat.pat_loc in
             Collected_lints.add ~loc (report ~loc ~filename:source_file id))
             | _ ->
             (* TODO: support Ppat_as ... *)
             () *)
        in
        let rec loop_str = function
          | [] -> ()
          | h :: tl ->
            let () =
              match h.Typedtree.str_desc with
              | Tstr_value (_, vbs) -> List.iter (loop_vb tl) vbs
              | _ -> ()
            in
            loop_str tl
        in
        loop_str x.Typedtree.str_items;
        fallback.structure self x)
  }
;;
