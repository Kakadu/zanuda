[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "exc_error_swallowing"
let group = LINT.Suspicious
let level = LINT.Deny
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Catching all possible exceptions with wildcard considered as antipattern

See also https://en.wikipedia.org/wiki/Error_hiding
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf () = Format.fprintf ppf "Antipattern: error swallowing%!"
let report = Utils.make_reporter lint_id msg

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_try drop (case tpat_any none __' ^:: nil)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (* TODO: support exceptions during matching *)
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun { loc } () ->
            (* Reported location is a location of whole match and not of pattern
               TODO: understand how to fix it *)
            (* Format.printf "%a\n%!" Location.print_loc loc; *)
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            Collected_lints.add ~loc (report ~filename ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
