(** Copyright 2021-2023, . *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
    Format.asprintf
      "let (_: %a) = %a"
      Printtyp.type_expr
      e0.exp_type
      Pprintast.expression
      e 
;;

let msg ppf e0 =
  let open Parsetree in
  let e = MyUntype.untype_expression e0 in
  let si =
    let open Ast_helper in
    Format.asprintf
      "let (_: %a) = %a"
      Printtyp.type_expr
      e0.exp_type
      Pprintast.expression
      e
  in 
  Caml.Format.fprintf ppf "Eta reduction proposed. It's recommended to rewrite it as '%s'%!" si
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

let no_ident ident c =
  let exception Found in
  let open Tast_iterator in
  let open Typedtree in
  let it =
    { default_iterator with
      expr =
        (fun self e ->
          match e.exp_desc with
          | Texp_ident (Path.Pident id, _, _) when Ident.equal id ident -> raise Found
          | Texp_function { param } when Ident.equal ident param -> ()
          | _ -> default_iterator.expr self e)
    ; case =
        (fun (type a) self (c : a case) ->
          match c.c_lhs.pat_desc with
          | Tpat_value v ->
            (match (v :> pattern) with
             | { pat_desc = Tpat_var (id, _) } ->
               if Ident.equal ident id then () else default_iterator.case self c
             | _ -> default_iterator.case self c)
          | _ -> default_iterator.case self c)
    }
  in
  try
    it.expr it c;
    true
  with
  | Found -> false
;;

let run _ fallback =
  let pattern_base_map f func args = f ([], func, args) in 
  let pattern_cons_map f id rest = match rest with 
    | (ids, func, args) -> f (id::ids, func, args)
  in
  let rec pat_func = (
    let open Tast_pattern in
      let base_pattern = (Tast_pattern.map (
        texp_apply __ (many (nolabel ** some (texp_ident __))))
         ~f:pattern_base_map)
      in
      let base_pattern_func = to_func base_pattern in 
      let var_pattern_func = to_func (tpat_var __) in
      let none_pattern_func = to_func none in
      let nil_pattern_func = to_func nil in 
      let one_case_pattern_function = 
        (fun ctx loc { Typedtree.c_lhs; Typedtree.c_rhs; Typedtree.c_guard } k ->
          pat_func ctx loc c_rhs (none_pattern_func ctx loc c_guard (var_pattern_func ctx loc c_lhs k))
        ) 
      in
      let cases_pattern_function = 
        (
          (fun ctx loc x k ->
            match x with
            | x0 :: x1 ->
              incr_matched ctx;
              let k = one_case_pattern_function ctx loc x0 k in
              let k = nil_pattern_func ctx loc x1 k in
              k
            | _ ->
              fail loc "::")      
        )
      in  
      let cons_pattern_func = 
        (fun ctx loc e k ->
          match e.Typedtree.exp_desc with
          | Texp_function { cases } ->
            incr_matched ctx; 
            k |> cases_pattern_function ctx loc cases
          | _ -> fail loc "texp_function" )
      in
      let base_wrapped = (fun ctx loc x k -> base_pattern_func ctx loc x (pattern_base_map k)) in
      let cons_wrapped = (fun ctx loc x k -> cons_pattern_func ctx loc x (pattern_cons_map k)) in
      (fun ctx loc x k ->
        let backup = save_context ctx in
        try base_wrapped ctx loc x k with
        | e1 ->
          let m1 = save_context ctx in
          restore_context ctx backup;
          (try cons_wrapped ctx loc x k with
           | e2 ->
             let m2 = save_context ctx in
             if m1 >= m2
             then (
               restore_context ctx m1;
               raise e1)
             else raise e2)))
  in
  let pat = of_func pat_func in 
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        let ident2string ident = match ident with
          | Path.Pident id -> Ident.name id
          | _              -> ""
        in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun vals ->
            match vals with 
            | (ids, (_, func, args), _) -> (
(*              Format.printf "Expr: `%s`\nInner=`%s`\nFormal args=`%s`\nReal args=`%s`\nLengths: %d %d\n" 
                (expr2string expr)
                (expr2string func)
                (String.concat ~sep:", " ids)
                (String.concat ~sep:", " (List.map ~f:ident2string args))
                (List.length ids) 
                (List.length args);*)
              if List.length args > 0  
                && List.equal String.equal ids (List.map args ~f:ident2string) 
                && ( let no_id_in_func ident = 
                     match ident with 
                     | (Path.Pident id) -> no_ident id func
                     | _                -> false  
                  in 
                  List.for_all args ~f:no_id_in_func)
              then ( 
                CollectedLints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc (func))))
          )
          ();
        fallback.expr self expr)
  }
;;