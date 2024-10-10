(** Aggregate all typed defined in a file. Not really a lint *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "misc_aggregate_defs"
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation = {|
### What it does


#### Explanation


|} |> Stdlib.String.trim

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~level ~docs:documentation
;;

let expr2string e0 =
  let open Parsetree in
  let e = My_untype.untype_expression e0 in
  let open Ast_helper in
  Stdlib.Format.asprintf
    "let (_: %a) = %a"
    Printtyp.type_expr
    e0.exp_type
    Pprintast.expression
    e
;;

let msg ppf (old_expr, new_expr) =
  let open Parsetree in
  Caml.Format.fprintf
    ppf
    "Eta reduction proposed. It's recommended to rewrite @['%a'@] as @['%a'@]%!"
    Pprintast.expression
    (My_untype.expr old_expr)
    Pprintast.expression
    (My_untype.expr new_expr)
;;

let report filename ~loc ~old_expr new_expr =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg (old_expr, new_expr)

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        (old_expr, new_expr)
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

(* let no_ident c ident = Utils.no_ident ident (fun it -> it.expr it c) *)
let has_deriving_attribute (attrs : Typedtree.attributes) =
  try
    let open Parsetree in
    let _ : attribute =
      List.find
        (function
          | { attr_name = { txt = "deriving" }; _ } -> true
          | _ -> false)
        attrs
    in
    true
  with
  | Not_found -> false
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    (* type_declarations =
       (fun self tdecls ->

       fallback.type_declarations self tdecls)
       ; *)
    type_declaration =
      (fun self tdecl ->
        (match tdecl.typ_kind, tdecl.Typedtree.typ_manifest with
         | (Ttype_variant _ | Ttype_open | Ttype_record _), _ -> ()
         | Ttype_abstract, None -> ()
         | Ttype_abstract, Some t when has_deriving_attribute tdecl.typ_attributes ->
           Collected_lints.add_tdecl t.ctyp_loc
         | _ -> ());
        fallback.type_declaration self tdecl)
  }
;;
