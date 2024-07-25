[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type w =
  | Wrapped of string
  | Non_wrapped

val pp_w : Format.formatter -> w -> unit

val analyze_dir
  :  untyped:(string -> unit)
  -> cmt:(w -> string -> Typedtree.structure -> unit)
  -> cmti:(w -> string -> Typedtree.signature -> unit)
  -> string
  -> unit

val load_description : string -> Dune_project.t list
