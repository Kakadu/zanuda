[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Tast_pattern
open Utils
open Typedtree

let first_case = function
  | h :: _ -> h
  | [] ->
    Format.eprintf "Error at %s %d\n%!" __FILE__ __LINE__;
    exit 1
;;

let get_match_constr_payload loc ematch_cases =
  let pat =
    let c = first_case ematch_cases in
    c.c_lhs
  in
  let point = Utils.{ loc; pos = Start } in
  let shift_point = shift_point_cnum point 5 in
  set_padding shift_point (pat_point pat Start) Space_padding;
  set_padding point shift_point (Padding "function")
;;

let get_propose_function_payload loc =
  set_empty_padding (make_point loc Start) (make_point loc End)
;;

(* TODO(Kakadu): describe difference between two locations *)
let register_fix ~loc scru_pat_loc cases =
  (* Format.printf "%s: %a\n%!" __FUNCTION__ My_printtyped.expr e; *)
  (* Format.printf "loc = %a\n%!" Location.print_loc loc; *)
  get_match_constr_payload loc cases;
  get_propose_function_payload scru_pat_loc
;;
