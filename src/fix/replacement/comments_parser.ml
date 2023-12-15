(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Lexing

let shift_pos point content =
  let lines = String.split_on_char '\n' content in
  let len = List.length lines in
  let shift = String.length (List.nth lines (len - 1)) in
  match len with
  | 1 -> { point with pos_cnum = point.pos_cnum + shift }
  | _ -> { point with pos_lnum = point.pos_lnum + len - 1; pos_bol = 0; pos_cnum = shift }
;;

open Base

type token =
  | Str
  | Quoted_str
  | Augth

type site =
  | Inside_str
  | Inside_quoted
  | Outside

let comm_inside_line tks =
  Stdlib.List.fold_left
    (fun site tk ->
      match site, tk with
      | Outside, Str | Inside_str, Quoted_str | Inside_str, Augth -> Inside_str
      | Outside, Quoted_str | Inside_quoted, Str | Inside_quoted, Augth -> Inside_quoted
      | Inside_str, Str | Inside_quoted, Quoted_str | Outside, Augth -> Outside)
    Outside
    tks
;;

let quoted_str ec sc =
  peek_char
  >>| function
  | Some c -> if Char.equal c ec then sc, Quoted_str else sc, Augth
  | None -> failwith "invalid_arg"
;;

let check_string =
  any_char
  >>= function
  | '"' -> return ('"', Str)
  | '{' -> quoted_str '|' '{'
  | '|' -> quoted_str '}' '|'
  | c -> return (c, Augth)
;;

let shift_str = function
  | Inside_quoted ->
    many_till any_char (string "|}") >>| fun l -> String.of_char_list l ^ "|}"
  | Inside_str ->
    many_till any_char (string "\"") >>| fun l -> String.of_char_list l ^ "\""
  | Outside -> return ""
;;

let lp pos =
  many_till check_string (string "(*")
  >>| fun p ->
  let s, tks = Stdlib.List.split p in
  shift_pos pos (String.of_char_list s), comm_inside_line tks
;;

let rp = string "*)"

let comm pos coms site =
  lift2
    (fun (spos, site) content ->
      let comm = "(*" ^ content ^ "*)" in
      let epos = shift_pos spos comm in
      let coms =
        match site with
        | Outside -> ({ spos with pos_cnum = spos.pos_cnum + 1 }, epos, comm) :: coms
        | _ -> coms
      in
      epos, coms, site)
    (shift_str site >>= fun content -> lp (shift_pos pos content))
    (many_till any_char rp >>| String.of_char_list)
;;

let comms pos =
  let rec go (pos, acc, site) = comm pos acc site >>= go <|> return acc in
  go (pos, [], Outside)
;;

let parse pos content =
  match parse_string ~consume:Consume.Prefix (comms pos) content with
  | Ok res -> List.rev res
  | _ -> []
;;
