(** Global storage for found defects.

    Mutable. Not thread safe. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** {1 Collecting found lints} *)

val clear : unit -> unit
val add : loc:Warnings.loc -> (module LINT.REPORTER) -> unit

(** Report found lints

    - In RdJSONl format. Change {!Config.out_rdjsonl} to modify output file name
    - As plain text to stdout *)
val report : unit -> unit

(** {1 Collecting type declarations}

    We use information about type declarations to skip reporting lints in
    the code generated from a type declaration via `deriving`. *)

val add_tdecl : Warnings.loc -> unit
val has_tdecl_at : Warnings.loc -> bool
val clear_tdecls : unit -> unit
