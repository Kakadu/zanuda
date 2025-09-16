[@@@ocaml.text "/*"]

(** Copyright 2021-2025, .... *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "match_bool"
let lint_source = LINT.FPCourse
let group = LINT.Style
let level = LINT.Warn

let documentation =
  {|
### What it does
Proposes to rewrite 'match x with ... | true -> (1)  ... | false -> (2) ` to `if x then (1) else (2)`.

### Why?
Using `if` is more readable way to examine boolean value.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf e0 =
  let open Parsetree in
  let e = My_untype.expr e0 in
  Format.fprintf
    ppf
    "Match is redundant. It's recommended to rewrite it as @['%a'@]%!"
    My_pprintast.expression
    e
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

let expr2string e0 =
  let open Parsetree in
  let e = My_untype.expr e0 in
  let open Ast_helper in
  Format.asprintf "let (_: %a) = %a" Printtyp.type_expr e0.exp_type Pprintast.expression e
;;

type if_cases_info =
  | True_false
  | False_true
  | Other

let run _ fallback =
  let pat () =
    let open Tast_pattern in
    let cases_pat () =
      case (tpat_constructor __ nil) none __
      ^:: case (tpat_constructor __ nil) none __
      ^:: nil
      |> map4 ~f:(fun a b c d -> (a, b), (c, d))
    in
    texp_function_cases (__ ^:: nil) (cases_pat ())
    |> map2 ~f:(fun arg cases -> `Function arg, cases)
    ||| (texp_match __ drop (cases_pat ())
         |> map2 ~f:(fun scru cases -> `Match scru, cases))
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          (pat ())
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun (info, ((id1, rhs1), (id2, rhs2))) () ->
            let cases_shape k =
              match Longident.flatten id1, Longident.flatten id2 with
              | [ "true" ], [ "false" ] ->
                k True_false (fun scru ->
                  { expr with exp_desc = Texp_ifthenelse (scru, rhs1, Some rhs2) })
              | [ "false" ], [ "true" ] ->
                k False_true (fun scru ->
                  { expr with exp_desc = Texp_ifthenelse (scru, rhs2, Some rhs1) })
              | _ -> ()
            in
            cases_shape (fun _shape make_expr ->
              match info with
              | `Match scru ->
                Collected_lints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc (make_expr scru))
              | `Function (_, (_id, _idloc)) ->
                (* Format.printf "id = %a\n%!" Ident.print id;
                   Format.printf "idloc = %a\n%!" Location.print_loc idloc;
                   if Utils.no_ident id (fun it -> it.expr it expr)
                   then
                   Collected_lints.add
                   ~loc
                   (report loc.Location.loc_start.Lexing.pos_fname ~loc expr); *)
                ()))
          ();
        fallback.expr self expr)
  }
;;
