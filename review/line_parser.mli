(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [run ?info p] trims line break in the input string and runs parser [p]. *)
val run : ?info:string -> 'a Angstrom.t -> 'a Angstrom.t

val diff_cmd : unit Angstrom.t
val file_mode : unit Angstrom.t
val similarity : unit Angstrom.t
val rename : unit Angstrom.t
val index : unit Angstrom.t
val remove_file : string Angstrom.t
val add_file : string Angstrom.t
val pos_num : int Angstrom.t
val chunk_head : Types.chunk_info Angstrom.t
val chunk_item : (Types.kind * string) Angstrom.t
val no_new_line_eof : unit Angstrom.t
val set_logging : bool -> unit
