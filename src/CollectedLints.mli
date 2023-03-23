(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val clear : unit -> unit
val is_empty : unit -> bool
val add : loc:Warnings.loc -> (module LINT.REPORTER) -> unit
val report : unit -> unit
