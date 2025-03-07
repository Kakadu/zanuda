(** Detection of possible eta-conversion.

    Initial implementation was contrivbuted by Github user jegorpopow *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu *)

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
  |> Stdlib.String.trim
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

let no_ident c ident = Utils.no_ident ident (fun it -> it.expr it c)

let run _ fallback =
  let pattern_cons_map f id = function
    | ids, func, args -> f (id :: ids, func, args)
  in
  let var_pattern_func = to_func (tpat_var __) in
  let extract_path = function
    | Asttypes.Nolabel, Some { Typedtree.exp_desc = Typedtree.Texp_ident (path, _, _) } ->
      Some path
    | _ -> None
  in
  let rec pat_func ctx lc e k =
    let open Tast_pattern in
    match e.Typedtree.exp_desc with
    | Texp_function
        { arg_label = Nolabel; cases = { c_lhs; c_guard = None; c_rhs } :: [] } ->
      pattern_cons_map k |> var_pattern_func ctx lc c_lhs |> pat_func ctx lc c_rhs
    | Texp_apply (({ Typedtree.exp_desc = Texp_ident _; _ } as body), args) ->
      let paths = List.filter_map extract_path args in
      if List.length args = List.length paths
      then k ([], body, paths)
      else fail lc "eta-reduction FC pattern"
    | _ -> fail lc "eta-reduction FC pattern"
  in
  let pat = of_func pat_func in
  let open Tast_iterator in
  let check expr (ids, new_expr, args) () =
    let open Typedtree in
    let loc = expr.exp_loc in
    let extract_ident = function
      | Path.Pident id -> Some id
      | _ -> None
    in
    (*              Format.printf "Expr: `%s`\nInner=`%s`\nFormal args=`%s`\nReal args=`%s`\nLengths: %d %d\n"
                    (expr2string expr)
                    (expr2string func)
                    (String.concat ~sep:", " ids)
                    (String.concat ~sep:", " (List.map ~f:ident2string args))
                    (List.length ids)
                    (List.length args); *)
    let idents = List.filter_map extract_ident args in
    let args_len = List.length args in
    if args_len > 0
       && args_len = List.length idents
       && List.equal String.equal ids (List.map Ident.name idents)
       && (not (Base.List.contains_dup ~compare:String.compare ids))
       && List.for_all (no_ident new_expr) idents
    then
      if not (Collected_lints.has_tdecl_at loc)
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
          pat
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (check expr)
          ();
        fallback.expr self expr)
  }
;;
