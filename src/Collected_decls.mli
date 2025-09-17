(** Global storage for defined names. Is used to detect public but not used library fields.

    Mutable. Not thread safe.
    See also {!Unused_ML_logger} *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Report definition of a value. Usually is called from signature analyzer. *)
val add_just_decl : string -> unit

(** Report usage of a value. Usually is called from structure analyzer. *)
val add_used_decl : string -> unit

val print_all_decls : unit -> unit
val print_used_decls : unit -> unit

(* Remove used declarations from declared ones. *)
val collect_unused : unit -> unit

(* TODO: Where is the code to analyze structure? *)

(** Analyze signature. See also {!MLLogger.run} *)
val collect_from_mli_tree : Load_dune.w -> string -> Typedtree.signature -> unit
