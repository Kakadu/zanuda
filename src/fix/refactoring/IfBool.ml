(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Utils

type ite =
  | If of bool
  | Then of bool
  | Else of bool

type fix_kind =
  | Unwise_conjuction of bool
  | Unwise_ite of ite

let ite_if b = Unwise_ite (If b)
let ite_then b = Unwise_ite (Then b)
let ite_else b = Unwise_ite (Else b)
let conj b = Unwise_conjuction b

let bool_value e =
  let open Tast_pattern in
  parse ebool e.exp_loc ~on_error:(fun _ () -> None) e (fun b () -> Some b) ()
;;

(* Fix for unwise_conj assumes that the conj takes two arguments because at the time
   of implementation the linter can only detect the use of a conjuction with two arguments *)
let check_bool args vbool =
  let helper e e' f f' = 
    let func = if vbool then f else f' in
    set_empty_padding (func e) (func e')
  in
  match args with
  | f :: s :: _ ->
    let _, v = f in
    let _, v' = s in
    let open Tast_pattern in
    (match v, v' with
     | Some e, Some e' ->
       parse
         ebool
         e.exp_loc
         ~on_error:(fun _ () -> helper e e' exp_end exp_start)
         e
         (fun _ () -> helper e e' exp_start exp_end)
         ()
     | _ -> failwith "invalid_arg")
  | _ -> ()
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
