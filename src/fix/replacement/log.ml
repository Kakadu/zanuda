(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let fix_dir = "fix_gen/"
let fgen_suffix = ".corrected"
let diffs_file = fix_dir ^ "diffs.log"
let promote_script = fix_dir ^ "promote.sh"

module Error = struct
  let wrong_loc loc =
    let msg = "Wrong location for replacement" in
    let r = Location.error msg ~loc in
    Location.print_report Format.err_formatter r
  ;;

  let promote_missing =
    "Files required for promote are missing. Please run the linter with the -dir \
     [path_to_dune_project_root_dir] option and try again"
  ;;

  let err_promote code = Printf.sprintf "Fix failed with code %d" code
  let err_create_file name = Format.sprintf "Cannot create %s" name
  let diff_killed signal = Format.sprintf "diff: subcommand killed by signal %i" signal
  let diff_stoped signal = Format.sprintf "diff: subcommad stopped by signal %i" signal
end

let create_file name =
  match Sys.command (Format.sprintf "touch %s" name) with
  | 0 -> ()
  | _ -> failwith (Error.err_create_file name)
;;

let rec rm path =
  match Sys.is_directory path with
  | true ->
    Sys.readdir path |> Array.iter (fun f -> rm (fix_dir ^ f));
    Sys.rmdir path
  | false -> Sys.remove path
;;

let prepare_env () =
  if Sys.file_exists fix_dir then rm fix_dir;
  Sys.mkdir fix_dir 0o755;
  create_file promote_script;
  create_file diffs_file
;;

let add_promote f fgen =
  let oc = Out_channel.open_gen [ Open_append; Open_creat ] 0o666 promote_script in
  Printf.fprintf oc "cat %s > %s;\n" fgen f;
  close_out oc
;;

let promote path =
  Sys.chdir path;
  if Sys.file_exists promote_script
  then (
    match Sys.command (Printf.sprintf "./%s" promote_script) with
    | 0 -> rm fix_dir
    | e -> failwith (Error.err_promote e))
  else Printf.eprintf "%s" Error.promote_missing
;;

let diff f fgen =
  let in_channel = Unix.open_process_in (Format.sprintf {|diff %s %s |} f fgen) in
  let result = In_channel.input_all in_channel in
  match Unix.close_process_in in_channel with
  | WEXITED _ -> result
  | WSIGNALED signal -> failwith (Error.diff_killed signal)
  | WSTOPPED signal -> failwith (Error.diff_stoped signal)
;;

let write_diff fname diff =
  let oc = Out_channel.open_gen [ Open_append ] 0o666 diffs_file in
  Printf.fprintf oc "Diffs for file %s\n%s\n" fname diff;
  close_out oc
;;

let diff_log f fgen =
  let diff_result = diff f fgen in
  if not @@ String.equal diff_result ""
  then (
    write_diff f diff_result;
    add_promote f fgen)
;;

let name_corrected_file src_file =
  let sep_sections = String.split_on_char '/' src_file in
  let src_fname = List.nth sep_sections (List.length sep_sections - 1) in
  let fname = src_fname ^ fgen_suffix in
  let fpath = fix_dir ^ fname in
  fpath
;;

let gen_corrected_file fname content =
  let oc = open_out fname in
  Printf.fprintf oc "%s" content;
  close_out oc
;;

let file_content fname = In_channel.with_open_text fname In_channel.input_all
