[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Utils

let apply_fix location expr =
  if Zanuda_core.Config.gen_replacements ()
  then (
    let content = Pprintast.string_of_expression expr in
    set_payload { location; payload = Padding content })
;;
