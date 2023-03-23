(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val analyze_dir
  :  untyped:(string -> unit)
  -> cmt:(string -> Typedtree.structure -> unit)
  -> cmti:(string -> Typedtree.signature -> unit)
  -> string
  -> unit
