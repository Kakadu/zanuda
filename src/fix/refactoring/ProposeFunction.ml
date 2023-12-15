(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tast_pattern
open Utils
open Typedtree

let first_case cs = List.nth cs 0

let get_match_constr_payload ematch_case =
  let e =
    let c = first_case ematch_case in
    c.c_rhs
  in
  let pat = texp_match (texp_ident __) __ in
  parse
    pat
    e.exp_loc
    e
    (fun _ cs () ->
      let pat =
        let c = first_case cs in
        c.c_lhs
      in
      let point = exp_start e in
      let shift_point = shift_point_cnum point 5 in
      set_padding shift_point (pat_point pat Start) Space_padding;
      set_padding point shift_point (Padding "function"))
    ()
;;

let get_propose_function_payload ematch_case =
  let extra_arg =
    let c = first_case ematch_case in
    c.c_lhs
  in
  set_empty_padding (pat_point extra_arg Start) (pat_point extra_arg End)
;;

let apply_fix = function
  | Texp_function { cases } ->
    get_match_constr_payload cases;
    get_propose_function_payload cases
  | _ -> failwith "invalid_arg"
;;
