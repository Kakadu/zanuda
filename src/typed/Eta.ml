(** Detection of possible eta-conversion.

    Initial implementation was contrivbuted by Github user jegorpopow *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "eta_reduction"
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Straightforward wrapper functions are excessive and may be reduced

#### Explanation

Let's look at the expression 'let f x = g x'.
It may be simply replaced with an expression, `let f = g` which has the same semantics.
In general, wrappers like this may be confusing, so it is recommended to get rid of them.
|}
  |> String.trim
;;

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
  Format.fprintf
    ppf
    "Eta reduction proposed. It's recommended to rewrite @['%a'@] as @['%a'@]%!"
    My_pprintast.expression
    (My_untype.expr old_expr)
    My_pprintast.expression
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

let no_ident c ident = Utils.no_ident ident (fun it -> it.expr it c)

let run _ fallback =
  let pat =
    let open Tast_pattern in
    let is_ident_expr arg =
      parse
        (texp_ident __)
        arg.Typedtree.exp_loc
        ~on_error:(fun _ -> None)
        arg
        (fun s -> Some s)
    in
    fun loca ->
      texp_function_body __ (texp_apply_nolabelled (as__ (texp_ident drop)) __)
      |> map3 ~f:(fun formal_args fid real_args ->
        let formal_len = List.length formal_args in
        if formal_len <> List.length real_args
        then fail loca "Not for eta-expansion"
        else (
          let formal_pats =
            List.map
              (function
                | Asttypes.Nolabel, (id, _) -> id
                | _ -> fail loca "labels")
              formal_args
          in
          let formal_idents = List.filter_map is_ident_expr real_args in
          let idents_len = List.length formal_idents in
          if List.length formal_pats = idents_len && idents_len = formal_len
          then formal_pats, fid, formal_idents
          else fail loca "Not for eta-expansion"))
  in
  let open Tast_iterator in
  let check expr (ids, new_expr, args) () =
    let open Typedtree in
    let loc = expr.exp_loc in
    let extract_ident = function
      | Path.Pident id -> Some id
      | _ -> None
    in
    let idents = List.filter_map extract_ident args in
    let args_len = List.length args in
    let no_ident_shadowing ids =
      let compare a b = String.compare (Ident.name a) (Ident.name b) in
      not (Base.List.contains_dup ~compare ids)
    in
    if
      args_len > 0
      && args_len = List.length idents
      && List.equal (fun a b -> String.equal (Ident.name a) (Ident.name b)) ids idents
      && no_ident_shadowing ids
      && List.for_all (no_ident new_expr) idents
      && not (Collected_lints.has_tdecl_at loc)
    then
      Collected_lints.add
        ~loc
        (report loc.Location.loc_start.Lexing.pos_fname ~loc ~old_expr:expr new_expr)
  in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        Tast_pattern.parse
          (pat expr.exp_loc)
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (check expr)
          ();
        fallback.expr self expr)
  }
;;
