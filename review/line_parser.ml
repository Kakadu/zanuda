(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Types

type +'a parser = 'a Angstrom.t

let use_logging = ref false
let set_logging flg = use_logging := flg

let log fmt =
  if !use_logging
  then Format.kasprintf (Format.eprintf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let log1 fmt = Format.kasprintf (Format.eprintf "%s\n%!") fmt

let diff_cmd : unit parser =
  let* _ = string "diff" in
  many any_char *> return ()
;;

let file_mode : unit parser =
  let* _ = string "new" <|> string "deleted" in
  many any_char *> return ()
;;

let similarity : unit parser =
  let* _ = string "similarity index" in
  many any_char *> return ()
;;

let rename : unit parser =
  let* _ = string "rename from" <|> string "rename to" in
  many any_char *> return ()
;;

let index : unit parser =
  let* _ = string "index" in
  many any_char *> return ()
;;

let remove_file : string parser =
  let* _ = string "--- " in
  let* _ = string "a/" <|> return "" in
  let* filename = take_while (fun c -> c <> ' ' && c <> '\t') in
  let* _ = many any_char in
  return filename
;;

let add_file : string parser =
  let* _ = string "+++ " in
  let* _ = string "b/" <|> return "" in
  let* filename = take_while (fun c -> c <> ' ' && c <> '\t') in
  let* _ = many any_char in
  return filename
;;

let pos_num : int parser =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  take_while1 is_digit >>| fun s -> int_of_string s
;;

let chunk_head : chunk_info parser =
  let pos_range = both pos_num (char ',' *> pos_num <|> return 1) in
  let* _ = string "@@ -" in
  let* old, old_range = pos_range in
  let* _ = string " +" in
  let* fresh, fresh_range = pos_range in
  let* _ = many any_char in
  return { old; old_range; fresh; fresh_range }
;;

let chunk_item : (kind * string) parser =
  let* k =
    char '+'
    >>| (fun _ -> Add)
    <|> (char '-' >>| fun _ -> Del)
    <|> (char ' ' >>| fun _ -> Leave)
  in
  let* rest = take_while (fun _ -> true) in
  return (k, rest)
;;

let no_new_line_eof : unit parser =
  let* s = string "\\ No newline at end of file" in
  log "%S skipped after eating %d chars" s (String.length s);
  return ()
;;

let run : ?info:string -> _ parser -> _ parser =
  fun ?info ppp ->
  (* log1 "inside run %s" (Option.fold ~none:"" ~some:Fun.id info); *)
  let* _ = many (char '\n') in
  let* str = take_while (fun c -> c <> '\n') in
  (* log1 "Going to parse %S from string %S" (Option.fold ~none:"" ~some:Fun.id info) str; *)
  match parse_string ~consume:Consume.All ppp str with
  | Result.Error e ->
    log "failed: %d" __LINE__;
    fail "Line parser failed"
  | Result.Ok rez ->
    (* log "forcing tail"; *)
    return rez
;;

let parse parser str = parse_string ~consume:Consume.All parser str
