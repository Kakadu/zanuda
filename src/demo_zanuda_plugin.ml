[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** A Zanuda plugin demo.asdfadfasd

This is a typed lint that detects {!hardcoded} string constant in the source file. asdfasd
*)

open Zanuda_core

(** Hardcoded constant for the demo: ["VERY LONG TAINTED STRING FOR ZANUDA"] *)
let hardcoded = "VERY LONG TAINTED STRING FOR ZANUDA"

module Impl = struct
  type input = Tast_iterator.iterator

  let lint_source = LINT.Camelot
  let lint_id = "zanuda-demo-plugin"
  let level = LINT.Warn

  let documentation =
    {|
### What it does

This demo dynamically loaded plugin demonstrates and ability to define plugins outside Zanuda.
This detect only hardcoded string constant '%s'.

|}
    |> String.trim
  ;;

  let describe_as_json () = Utils.describe_as_clippy_json lint_id ~docs:documentation
  let msg ppf () = Format.fprintf ppf "Demo dynamically loadable plugin.%!"
  let report = Utils.make_reporter lint_id msg

  let run _ fallback =
    let open Tast_iterator in
    { fallback with
      expr =
        (fun self expr ->
          let () =
            match expr.Typedtree.exp_desc with
            | Texp_constant (Asttypes.Const_string (s, _, _)) ->
              if String.equal s hardcoded
              then (
                let loc = expr.Typedtree.exp_loc in
                let filename = loc.Location.loc_start.Lexing.pos_fname in
                Collected_lints.add ~loc (report ~filename ~loc ()))
            | _ -> ()
          in
          fallback.expr self expr)
    }
  ;;
end

let () = Zanuda_core.Dynamic_plugins.add (module Impl : LINT.TYPED)
