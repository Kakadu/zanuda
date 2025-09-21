[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val check : Dune_project.t list -> unit
val describe_as_json : unit -> Yojson.Safe.t
val lint_id : string
(* val process_switches : string list -> unit *)
