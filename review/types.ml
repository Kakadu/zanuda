[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type chunk_info =
  { old : int
  ; old_range : int
  ; fresh : int
  ; fresh_range : int
  }
[@@deriving show]

type pos = int [@@deriving show]

type kind =
  | Add
  | Del
  | Leave
[@@deriving show]

type chunk = chunk_info * (kind * string * pos) list [@@deriving show]

type file_info =
  { old_file : string
  ; new_file : string
  ; chunks : chunk list
  }
[@@deriving show]
