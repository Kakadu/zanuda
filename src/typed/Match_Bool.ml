[@@@ocaml.text "/*"]

(** Copyright 2021-2024, .... *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
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
  let e = MyUntype.untype_expression e0 in
  let si =
    let open Ast_helper in
    Format.asprintf "%a" Pprintast.expression e
  in
  Caml.Format.fprintf
    ppf
    "Match is redundant. It's recommended to rewrite it as '%s'%!"
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

let expr2string e0 =
  let open Parsetree in
  let e = MyUntype.untype_expression e0 in
  let open Ast_helper in
  Format.asprintf "let (_: %a) = %a" Printtyp.type_expr e0.exp_type Pprintast.expression e
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_match
      __
      (ccase (tpat_constructor __ nil |> tpat_value) none __
       ^:: ccase (tpat_constructor __ nil |> tpat_value) none __
       ^:: nil)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun scru id1 rhs1 id2 rhs2 () ->
            (*            Format.printf "%a -> %s\n%a -> %s\n"
                          Pprintast.longident
                          id1
                          (expr2string rhs1)
                          Pprintast.longident
                          id2
                          (expr2string rhs2); *)
            match Longident.flatten id1, Longident.flatten id2 with
            | [ "true" ], [ "false" ] ->
              Collected_lints.add
                ~loc
                (report
                   loc.Location.loc_start.Lexing.pos_fname
                   ~loc
                   { expr with exp_desc = Texp_ifthenelse (scru, rhs1, Some rhs2) })
            | [ "false" ], [ "true" ] ->
              Collected_lints.add
                ~loc
                (report
                   loc.Location.loc_start.Lexing.pos_fname
                   ~loc
                   { expr with exp_desc = Texp_ifthenelse (scru, rhs2, Some rhs1) })
            | _ -> ())
          ();
        fallback.expr self expr)
  }
;;
