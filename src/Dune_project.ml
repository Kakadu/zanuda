[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

type module_ =
  { name : string
  ; impl : string option
  ; intf : string option
  ; cmt : string option
  ; cmti : string option
  }
[@@deriving sexp]

let module_ ?cmt ?cmti name = { name; cmt; cmti; impl = None; intf = None }

type executables =
  { names : string list
  ; modules : module_ list
  ; requires : string list
  ; include_dirs : string list
  }
[@@deriving of_sexp]

module Library = struct
  type t =
    { name : string
    ; uid : string
    ; local : bool
    ; requires : string list
    ; source_dir : string
    ; modules : module_ list
    ; include_dirs : string list
    }
  [@@deriving of_sexp]
end

type t =
  | Executables of executables
  | Library of Library.t
  | Root of string
  | Build_context of string
[@@deriving of_sexp]
