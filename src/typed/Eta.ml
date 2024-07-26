[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "eta_reduction"
let group = LINT.Suspicious
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Straightforward wrapper functions are excessive and may be reduced

#### Explanation

Let's look at the expression 'let f x = g x'. It may be simply replaced with an expression, `let f = g` which has the same semantics. In general, wrappers like this may be confusing, so it is recommended to get rid of them
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let expr2string e0 =
  let open Parsetree in
  let e = MyUntype.untype_expression e0 in
  let open Ast_helper in
  Format.asprintf "let (_: %a) = %a" Printtyp.type_expr e0.exp_type Pprintast.expression e
;;

let msg ppf e0 =
  let open Parsetree in
  let e = MyUntype.untype_expression e0 in
  let si =
    let open Ast_helper in
    Format.asprintf "%a" Pprintast.expression e
  in
  Caml.Format.fprintf
    ppf
    "Eta reduction proposed. It's recommended to rewrite it as '%s'%!"
    si
;;

let report filename ~loc e =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg e

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        e
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let no_ident ident c = Utils.no_ident ident (fun it -> it.expr it c)

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
    | Texp_apply (body, args) ->
      let paths = List.filter_map ~f:extract_path args in
      if List.length args = List.length paths
      then k ([], body, paths)
      else fail lc "eta_redex"
    | _ -> fail lc "eta-redex"
  in
  let pat = of_func pat_func in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        let extract_ident = function
          | Path.Pident id -> Some id
          | _ -> None
        in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun (ids, func, args) () ->
            (*              Format.printf "Expr: `%s`\nInner=`%s`\nFormal args=`%s`\nReal args=`%s`\nLengths: %d %d\n"
                            (expr2string expr)
                            (expr2string func)
                            (String.concat ~sep:", " ids)
                            (String.concat ~sep:", " (List.map ~f:ident2string args))
                            (List.length ids)
                            (List.length args); *)
            let idents = List.filter_map ~f:extract_ident args in
            if List.length args > 0
               && List.length args = List.length idents
               && List.equal String.equal ids (List.map idents ~f:Ident.name)
               && (not (Base.List.contains_dup ~compare:String.compare ids))
               && List.for_all idents ~f:(fun ident -> no_ident ident func)
            then
              Collected_lints.add
                ~loc
                (report loc.Location.loc_start.Lexing.pos_fname ~loc func))
          ();
        fallback.expr self expr)
  }
;;
