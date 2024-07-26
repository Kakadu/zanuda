(** Specialized data type to deserialize the output of 'dune describe' *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type module_ =
  { name : string
  ; impl : string option
  ; intf : string option
  ; cmt : string option
  ; cmti : string option
  }
[@@deriving sexp]

val module_ : ?cmt:string -> ?cmti:string -> string -> module_

type executables =
  { names : string list
  ; modules : module_ list
  ; requires : string list
  ; include_dirs : string list
  }
[@@deriving of_sexp]

module Library : sig
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
