[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Tast_pattern
open Utils
open Typedtree

let first_case cs = List.nth cs 0

let get_match_constr_payload loc ematch_case =
  (* TODO: pass here starting point of lambda-abstraction
     actual body of the future function
  *)
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
      let point = Utils.{ loc; pos = Start } in
      (* let point = exp_start e in *)
      let shift_point = shift_point_cnum point 5 in
      set_padding shift_point (pat_point pat Start) Space_padding;
      set_padding point shift_point (Padding "function"))
    ()
;;

let get_propose_function_payload scru_pat =
  (* TODO: Pass here argument of function being abstracted *)
  (* let extra_arg =
     let c = first_case ematch_case in
     c.c_lhs
     in *)
  set_empty_padding (pat_point scru_pat Start) (pat_point scru_pat End)
;;

(* Tast_pattern.(parse (texp_function_cases drop __))
   e.exp_loc
   e
   ~on_error:(fun _ ->
   Format.eprintf "Error: expressio is '%a'\n%!" My_printtyped.expr e;
   failwith "invalid_arg")
   (fun cases ->
   get_match_constr_payload cases;
   get_propose_function_payload cases) *)
let register_fix ~loc scru_pat e cases =
  (* Format.printf "%s: %a\n%!" __FUNCTION__ My_printtyped.expr e; *)
  match e.exp_desc with
  | Texp_function { cases } ->
    get_match_constr_payload loc cases;
    get_propose_function_payload scru_pat
  | _ -> failwith "invalid_arg"
;;
