[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "forbid_many_nested_if_expressions"
let level = LINT.Warn
let lint_source = LINT.Camelot

let documentation =
  {|
### What it does

Check too many (>= 3 levels) nested if expressions, for example
* `if ... then (if ... then (if ... then (if ... then ... else ...) else ...) else ...) else ...`

### Why it is important

The big problem with nested conditions is that they confuse the control flow of the code:
they make it almost impossible to determine what code will be executed and when.

Adopted from camelot's lint list.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json ~group:LINT.Style lint_id ~docs:documentation
;;

let msg ppf () =
  Format.fprintf
    ppf
    "Using nested if expressions more than three layers deep is a bad practice. Use let \
     statements or helper methods or rethinking logic.%!"
;;

let report = Utils.make_reporter lint_id msg

let run _ fallback =
  let pat =
    let open Tast_pattern in
    let rec nst_ite depth f =
      match depth with
      | 3 -> f
      | _ -> texp_ite drop f drop ||| texp_ite drop drop (some f) |> nst_ite (depth + 1)
    in
    texp_ite drop drop drop |> nst_ite 0
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
          (fun () ->
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            Collected_lints.add ~loc (report ~filename ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
