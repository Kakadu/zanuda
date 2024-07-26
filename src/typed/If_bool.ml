[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

(* open Refactoring.If_bool *)
open Parsetree

type input = Tast_iterator.iterator

let lint_id = "if_bool"
let group = LINT.Style
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does?

Checks funny uses of boolean expressions, for example
* `if true ...`
* `if ... then false`
* `... && true`
* etc.

### Why it is important?

These unwise boolean expressions make code longer than it should be. For example, the expression `f x && false`
is semantically equivalent to false unless `f x` performs any effect (mutation, IO, exceptions, etc.).
The general rule of thumb is not to depend of the order of evaluation of this conjucts.
(The same idea as our functions should not depend on evaluation order of its' arguments.)
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf s = Caml.Format.fprintf ppf "%s\n%!" s

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

let do_check = ref true

let run _ fallback =
  let pat =
    let open Refactoring.If_bool in
    let open Tast_pattern in
    let ite =
      texp_ite ebool drop drop
      |> map1 ~f:(fun b -> Format.asprintf "Executing 'if %b' smells bad" b, ite_if b)
      ||| (texp_ite drop ebool drop
           |> map1 ~f:(fun b ->
             Format.asprintf "Executing 'if ... then %b' smells bad" b, ite_then b))
      ||| (texp_ite drop drop (some ebool)
           |> map1 ~f:(fun b ->
             Format.asprintf "Executing 'if ... then .. else %b' smells bad" b, ite_else b)
          )
    in
    let ops =
      texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) ebool drop
      ||| texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) drop ebool
      |> map1 ~f:(fun b -> Format.asprintf "Conjunction with boolean smells bad", conj b)
    in
    ite ||| ops
  in
  let is_merlin_hide attr =
    let open Parsetree in
    String.equal "merlin.hide" attr.attr_name.txt
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        (if !do_check
         then
           let open Typedtree in
           let __ _ = Format.eprintf "%a\n%!" My_printtyped.expr expr in
           let loc = expr.exp_loc in
           Tast_pattern.parse
             pat
             loc
             ~on_error:(fun _desc () -> ())
             expr
             (fun (s, unwise_type) () ->
               Collected_lints.add
                 ~loc
                 (report loc.Location.loc_start.Lexing.pos_fname ~loc s);
               Refactoring.If_bool.apply_fix expr unwise_type)
             ());
        fallback.expr self expr)
  ; structure_item =
      (fun self si ->
        match si.str_desc with
        | Tstr_include { incl_attributes; _ }
          when List.exists incl_attributes ~f:is_merlin_hide ->
          do_check := false;
          fallback.structure_item self si;
          do_check := true
        | _ -> fallback.structure_item self si)
  }
;;
