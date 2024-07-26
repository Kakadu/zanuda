(** Global storage for found defects.

    Mutable. Not thread safe. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val clear : unit -> unit
val add : loc:Warnings.loc -> (module LINT.REPORTER) -> unit

(** Report found lints

    - In RdJSONl format. Change {!Config.out_rdjsonl} to modify output file name
    - As plain text to stdout *)
val report : unit -> unit
