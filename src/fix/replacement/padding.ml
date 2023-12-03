(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Lexing

type payload =
  | Void
  | Space_padding
  | Padding of string

let get_line_col loc =
  ( loc.loc_start.pos_lnum
  , loc.loc_start.pos_cnum - loc.loc_start.pos_bol
  , loc.loc_end.pos_lnum
  , loc.loc_end.pos_cnum - loc.loc_end.pos_bol )
;;

let check_loc loc lines =
  let sline, scol, eline, ecol = get_line_col loc in
  sline > 0
  && sline < Array.length lines
  && eline > 0
  && eline < Array.length lines
  && scol > 0
  && scol <= String.length lines.(sline - 1) + 1
  && ecol > 0
  && ecol <= String.length lines.(eline - 1) + 1
  && eline - sline >= 0
  && (eline != sline || ecol - scol >= 0)
;;

let space_padding loc flines =
  let sline, scol, eline, ecol = get_line_col loc in
  match sline = eline with
  | true ->
    let padding = String.make (ecol - scol) ' ' in
    padding
  | false ->
    let padding =
      String.make (String.length flines.(sline - 1) - scol) ' '
      |> fun s -> Format.sprintf "%s\n" s
    in
    let padding =
      Array.fold_left
        (fun pad s -> Format.sprintf "%s%s\n" pad (String.make (String.length s) ' '))
        padding
        (Array.sub flines sline (eline - sline - 1))
    in
    let padding = Format.sprintf "%s%s" padding (String.make ecol ' ') in
    padding
;;

let payload_between_repls (loc_end_buf, loc_start_repl) flines =
  let end_buf_line, end_buf_col =
    loc_end_buf.pos_lnum, loc_end_buf.pos_cnum - loc_end_buf.pos_bol
  in
  let repl_line, repl_col =
    loc_start_repl.pos_lnum, loc_start_repl.pos_cnum - loc_start_repl.pos_bol
  in
  let payload =
    match end_buf_line = repl_line with
    | true -> String.sub flines.(repl_line - 1) end_buf_col (repl_col - end_buf_col)
    | false ->
      let lines =
        String.sub
          flines.(end_buf_line - 1)
          end_buf_col
          (String.length flines.(end_buf_line - 1) - end_buf_col)
      in
      let lines =
        Array.fold_left
          (fun ls l -> Format.sprintf "%s%s\n" ls l)
          (Format.sprintf "%s\n" lines)
          (Array.sub flines end_buf_line (repl_line - end_buf_line - 1))
      in
      let lines = lines ^ String.sub flines.(repl_line - 1) 0 repl_col in
      lines
  in
  payload
;;

let payload_between_repls_buf locs flines buf =
  let payload = payload_between_repls locs flines in
  Buffer.add_string buf payload;
  buf
;;

let get_col pos = pos.pos_cnum - pos.pos_bol
let loc_from_pos spos epos = { loc_start = spos; loc_end = epos; loc_ghost = false }

let compare_pos pos1 pos2 =
  pos1.pos_lnum < pos2.pos_lnum
  || (pos1.pos_lnum = pos2.pos_lnum && get_col pos1 <= get_col pos2)
;;

let nesting_loc loc1 loc2 =
  compare_pos loc2.loc_start loc1.loc_start && compare_pos loc1.loc_end loc2.loc_end
;;

let comments_inside_loc comms fix_loc =
  let rec sort acc = function
    | ((spos, epos, _) as h) :: tl ->
      if nesting_loc (loc_from_pos spos epos) fix_loc
      then sort (h :: acc) tl
      else if compare_pos epos fix_loc.loc_end
      then sort acc tl
      else acc
    | _ -> acc
  in
  sort [] comms
;;

let relative_pos st_pos pos =
  let rel_lnum = pos.pos_lnum - st_pos.pos_lnum in
  let rel_bol, rel_cnum =
    match rel_lnum = 0 with
    | true -> pos.pos_bol - st_pos.pos_bol, pos.pos_cnum - st_pos.pos_cnum
    | false -> pos.pos_bol, pos.pos_cnum
  in
  { pos with pos_bol = rel_bol; pos_cnum = rel_cnum; pos_lnum = rel_lnum + 1 }
;;

let insert_comments ({ loc_start; loc_end; _ } as loc) flines fcoms =
  let coms = List.rev (comments_inside_loc fcoms loc) in
  let coms_padding =
    List.fold_left (fun s (_, _, c) -> Printf.sprintf "%s %s" s c) "" coms
  in
  function
  | Void -> coms_padding
  | Padding s -> Printf.sprintf "%s %s" s coms_padding
  | Space_padding ->
    let padding = space_padding loc flines in
    let lines = Array.of_list (String.split_on_char '\n' padding) in
    let cur = ref (relative_pos loc_start loc_start) in
    let payload =
      List.fold_left
        (fun acc (spos, epos, comm) ->
          let rel_spos = relative_pos loc_start spos in
          let rel_epos = relative_pos loc_start epos in
          let pre_padding = payload_between_repls (!cur, rel_spos) lines in
          let padding = pre_padding ^ comm in
          let () = cur := rel_epos in
          Printf.sprintf "%s%s" acc padding)
        ""
        coms
    in
    let len = List.length coms in
    (match len with
     | 0 -> padding
     | _ ->
       let _, epos, _ = List.nth coms (List.length coms - 1) in
       payload
       ^ payload_between_repls
           (relative_pos loc_start epos, relative_pos loc_start loc_end)
           lines)
;;
