[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Location
open Lexing
open Padding

module OrderedType = struct
  type t =
    { location : Location.t
    ; payload : payload
    }

  let compare
    { location = { loc_start = { pos_cnum = sc; _ }; loc_end = { pos_cnum = ec; _ }; _ }
    ; _
    }
    { location = { loc_start = { pos_cnum = sc'; _ }; loc_end = { pos_cnum = ec'; _ }; _ }
    ; _
    }
    =
    if sc > sc' && ec < ec' then 0 else if sc = sc' then ec - ec' else sc - sc'
  ;;
end

module Set = Set.Make (OrderedType)

module File = struct
  type t = string (* string --> file .ml ??*)

  let compare = String.compare
end

module FileRepl = Map.Make (File)
include OrderedType

let repls = ref FileRepl.empty

let add fname r =
  let frepls =
    match FileRepl.find_opt fname !repls with
    | Some rs -> Set.add r rs
    | None -> Set.singleton r
  in
  repls := FileRepl.add fname frepls !repls
;;

let apply_all repls fcontent =
  let flines = Array.of_list (String.split_on_char '\n' fcontent) in
  let start_pos = { dummy_pos with pos_lnum = 1; pos_cnum = 0; pos_bol = 0 } in
  let cur = ref start_pos in
  let coms = Comments_parser.parse start_pos fcontent in
  let apply_repl { location = { loc_start; loc_end; _ } as loc; payload } buf =
    if check_loc loc flines && check_pos loc_start !cur
    then (
      let buf = payload_between_repls_buf (!cur, loc_start) flines buf in
      Buffer.add_string buf (insert_comments loc flines coms payload);
      cur := loc_end)
    else Log.Error.wrong_loc loc;
    buf
  in
  let buf = Buffer.create (String.length fcontent) in
  let buf = Set.fold apply_repl repls buf in
  let file_end =
    { pos_lnum = Array.length flines
    ; pos_cnum = String.length flines.(Array.length flines - 1)
    ; pos_bol = 0
    ; pos_fname = !cur.pos_fname
    }
  in
  let buf = payload_between_repls_buf (!cur, file_end) flines buf in
  Buffer.contents buf
;;

open Log

let apply_all () =
  let () = prepare_env () in
  let new_payloads =
    FileRepl.fold
      (fun fname frepls fr_acc ->
        if Sys.file_exists fname
        then (fname, apply_all frepls (file_content fname)) :: fr_acc
        else fr_acc)
      !repls
      []
  in
  List.iter
    (fun (file, payload) ->
      let corrected_file = name_corrected_file file in
      gen_corrected_file corrected_file payload;
      diff_log file corrected_file)
    new_payloads
;;
