(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Parser of the diff file. The grammar is similar too

    diff ::= ( head chunk* )*

    See also [file_head] and [a_chunk]. *)
val parse_whole_file : Types.file_info list Angstrom.t

(** Main entry point. Uses [parse_whole_file] under the hood *)
val parse_string : string -> (Types.file_info list, string) result

(** Parse header of the diff file *)
val file_head : (string * string) option Angstrom.t

(** Parses hunk for the file *)
val a_chunk : Types.chunk Angstrom.t

val recover_lines : string -> (int * (int * int)) list

(** The call [lookup db ~file ~line] searches in the list of file differences
    by a file name [file] and a file line [line]
    the corresponding line of in diff file (counting from the beginning of the information about [file]).

    Github API requires this information to submit a review *)
val lookup : Types.file_info list -> file:string -> line:int -> int option

(** Enable or disable trace logging *)
val set_logging : bool -> unit
