[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val check : Dune_project.t list -> unit
val describe_as_json : unit -> Yojson.Safe.t
