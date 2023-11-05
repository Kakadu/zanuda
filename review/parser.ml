(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* let lookup line_of_pos parsed ~file ~line =
  (* Format.printf "Parsed stuff:@[%a@]\n%!" [%show: Types.file_info list] parsed; *)
  let open Types in
  match
    List.find
      (fun { new_file } ->
        print_endline new_file;
        new_file = file)
      parsed
  with
  | exception Not_found -> Format.eprintf "file not found\n%!"
  | { chunks } ->
    (match
       List.find
         (fun (ci, _) -> ci.fresh <= line && line < ci.fresh + ci.fresh_range)
         chunks
     with
     | exception Not_found -> ()
     | ci, lines ->
       let rec loop cur lines =
         match lines with
         | (k, _, pos) :: tl ->
           if cur = line
           then pos
           else (
             match k with
             | Leave | Add -> loop (cur + 1) tl
             | Del -> loop cur tl)
         | [] -> assert false
       in
       let pos = loop ci.fresh lines + 2 in
       (* kind of magic constant + 2*)
       Printf.printf
         "Got something. It should be at pos %d on line %s\n"
         pos
         (match line_of_pos pos with
          | None -> "<unknown>"
          | Some n -> string_of_int n))
;; *)

type cfg =
  { mutable from : string option
  ; mutable file : string option
  ; mutable line : int option
  }

let cfg = { from = None; file = None; line = None }

let () =
  Arg.parse
    [ "-", Arg.Unit (fun () -> cfg.from <- None), " use stdin"
    ; "-f", Arg.String (fun s -> cfg.file <- Some s), " lookup for file"
    ; "-l", Arg.Int (fun n -> cfg.line <- Some n), " lookup for line in a file"
    ; ( "-vdp"
      , Arg.Unit (fun () -> Diff_parser.set_logging true)
      , " Enable logging in the diff parser" )
    ; ( "-vlp"
      , Arg.Unit (fun () -> Line_parser.set_logging true)
      , " Enable logging in the line parser" )
    ]
    (fun _ -> assert false)
    " "
;;

(* A piece from stdio library *)
let input_all t =
  let chunk_size = 10000 in
  let buffer = Buffer.create chunk_size in
  let rec loop () =
    Stdlib.Buffer.add_channel buffer t 10000;
    loop ()
  in
  try loop () with
  | End_of_file -> Buffer.contents buffer
;;

let read_all filename =
  let ch = open_in filename in
  let rez = input_all ch in
  close_in ch;
  rez
;;

let () =
  let input =
    match cfg.from with
    | None -> input_all stdin
    | Some file -> read_all file
  in
  match Diff_parser.parse_string input with
  | Result.Ok parsed ->
    (* Format.printf "Parsed stuff:@[%a@]\n%!" [%show: Types.file_info list] parsed; *)
    (match cfg with
     | { file = Some file; line = Some line } ->
       (match Diff_parser.lookup parsed ~file ~line with
        | Some diff_pos ->
          Format.printf
            "Got something. It should be at %d lines below from the first chunk header \
             of file in diff\n"
            diff_pos
        | None -> Format.eprintf "Can't find '%s' line %d in the diff\n" file line)
     | _ -> Format.eprintf "File or line was not initialized\n")
  | Error s ->
    Format.eprintf "Parsing failed: %s\n" s;
    exit 1
;;
