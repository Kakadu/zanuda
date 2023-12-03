(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Utils

type ite =
  | If of bool
  | Then of bool
  | Else of bool

let bool_value e =
  let open Tast_pattern in
  parse ebool e.exp_loc ~on_error:(fun _ () -> None) e (fun b () -> Some b) ()
;;

type fix_kind =
  | Unwise_conjuction of bool
  | Unwise_ite of ite

type msg_kind =
  | Ite
  | Conj

let msg = function
  | Conj ->
    Format.sprintf
      "(Fix `If_bool` lint)\n%s"
      "This boolean expression will be replaced by an equivalent with removing unwise \
       conjunction"
  | Ite ->
    Format.sprintf
      "(Fix `If_bool` lint):\n%s"
      "This boolean expression will be replaced by an equivalent with removing \
       unwise`if_then_else`"
;;

let check_bool args vbool =
  let _, val1 = List.nth args 0 in
  let _, val2 = List.nth args 1 in
  let open Tast_pattern in
  match val1, val2 with
  | Some e1, Some e2 ->
    parse
      ebool
      e1.exp_loc
      ~on_error:(fun _ () ->
        match vbool with
        | true -> set_empty_padding (exp_end e1) (exp_end e2)
        | false -> set_empty_padding (exp_start e1) (exp_start e2))
      e1
      (fun _ () ->
        match vbool with
        | true -> set_empty_padding (exp_start e1) (exp_start e2)
        | false -> set_empty_padding (exp_end e1) (exp_end e2))
      ()
  | _ -> failwith "invalid_arg"
;;

let get_ite_loc e ie te ee pbool_site =
  let match_ite = function
    | If true, _ ->
      (* if true then x else y --> x *)
      set_empty_padding (exp_start e) (exp_start te);
      set_empty_padding (exp_end te) (exp_end e)
    | If false, _ (* if false then x else y --> y *)
    | Then true, Some true (* if val then true else true --> true *)
    | Then false, Some false ->
      (* if val then false else false --> false*)
      set_empty_padding (exp_start e) (exp_start ee)
    | Then true, Some false (* if val then true else false --> val *) ->
      set_empty_padding (exp_start e) (exp_start ie);
      set_empty_padding (exp_end ie) (exp_end e)
    | Then false, Some true ->
      (* if val then false else true --> not val *)
      set_empty_padding (exp_start e) (exp_start ie);
      set_padding (exp_start ie) (exp_start ie) (Padding "not ");
      set_empty_padding (exp_end ie) (exp_end e)
    | _ ->
      (* previous p-m covers cases when ebool was parsed in then-e*)
      ()
  in
  match_ite (pbool_site, bool_value ee)
;;

let apply_fix exp = function
  | Unwise_conjuction ebool ->
    (match exp.exp_desc with
     | Texp_apply (_, args) -> check_bool args ebool
     | _ -> failwith "invalid_arg")
  | Unwise_ite ite_type ->
    (match exp.exp_desc with
     | Texp_ifthenelse (ie, te, ee) -> get_ite_loc exp ie te (Option.get ee) ite_type
     | _ -> failwith "invalid_arg")
;;
