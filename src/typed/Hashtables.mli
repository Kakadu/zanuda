[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

include Zanuda_core.LINT.TYPED

(** Apply .zanuda config switches *)
val process_switches : string list -> unit
