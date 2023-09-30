(* https://git-scm.com/docs/diff-format *)

open Angstrom
open Types

type +'a parser = 'a Angstrom.t

let use_logging = ref false
let set_logging flg = use_logging := flg

let log fmt =
  if !use_logging
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let lookup parsed ~file ~line =
  match List.find (fun { new_file } -> new_file = file) parsed with
  | exception Not_found ->
    Printf.eprintf "No file %S in the list of parsed chunks" file;
    None
  | { chunks } ->
    let rec find_chunk acc = function
      | (ch_info, ch_items) :: tl ->
        if ch_info.fresh <= line && line < ch_info.fresh + ch_info.fresh_range
        then ch_info, ch_items, acc
        else find_chunk (acc + List.length ch_items + 1) tl
      | [] -> raise Not_found
    in
    (match find_chunk 1 chunks with
     | exception Not_found -> None
     | ci, lines, first_line_number ->
       let rec loop cur acc = function
         | (Leave, _, _) :: tl | (Add, _, _) :: tl ->
           if cur = line then Some acc else loop (cur + 1) (acc + 1) tl
         | (Del, _, _) :: tl -> loop cur (acc + 1) tl
         | [] -> None
       in
       loop ci.fresh first_line_number lines)
;;

let file_head : _ parser =
  log "%d: file_head" __LINE__;
  let* () = option () Line_parser.(run ~info:"diff_cmd" diff_cmd) in
  let* () =
    many
      (choice
         [ Line_parser.(run ~info:"similarity" similarity)
         ; Line_parser.(run ~info:"file_mode" file_mode)
         ; Line_parser.(run ~info:"rename" rename)
         ; Line_parser.(run ~info:"index" index)
         ])
    *> return ()
  in
  let* old_file = Line_parser.(run ~info:"remove_file" remove_file) in
  log "%d: old_file = %s" __LINE__ old_file;
  let* new_file = Line_parser.(run ~info:"new_file" add_file) in
  log "%d: new_file = %S" __LINE__ new_file;
  return (old_file, new_file)
;;

let a_chunk : chunk parser =
  log "%d: a_chunk" __LINE__;
  let* info = Line_parser.(run ~info:"chunk_head" chunk_head) in
  let* diffs =
    many
      (let* pos = pos in
       let* kind, s = Line_parser.(run ~info:"chunk_item" chunk_item) in
       return (kind, s, pos))
  in
  let* () = option () Line_parser.(run ~info:"no_new_line_eof" no_new_line_eof) in
  return (info, diffs)
;;

let parse_whole_file : file_info list parser =
  many
    (let* old_file, new_file = file_head in
     log "%d" __LINE__;
     let* chunks = many a_chunk in
     log "%d" __LINE__;
     let* _ = many (string "\n") in
     return { old_file; new_file; chunks })
;;

let parse_string str = parse_string ~consume:Consume.All parse_whole_file str

let is_correct_chunk info lines =
  let old, fresh =
    List.fold_left
      (fun (oacc, facc) -> function
        | Del, _, _ -> 1 + oacc, facc
        | Add, _, _ -> oacc, 1 + facc
        | Leave, _, _ -> 1 + oacc, 1 + facc)
      (0, 0)
      lines
  in
  log "@@@@ -%d,%d +%d,%d " info.old info.old_range info.fresh info.fresh_range;
  let pk = function
    | Add -> '+'
    | Del -> '-'
    | _ -> ' '
  in
  List.iteri (fun i (k, s, _) -> log "%d:\t%c%s" i (pk k) s) lines;
  assert (info.old_range = old);
  assert (info.fresh_range = fresh);
  true
;;

let pp_lines_db xs =
  Format.pp_print_list ~pp_sep:Format.pp_print_space [%show: int * (int * int)] xs
;;

let recover_lines input =
  let extend acc ~line start ~fin =
    if fin >= start then (line, (start + 1, fin)) :: acc else acc
  in
  let rec loop acc line idx =
    (* Format.printf "loop: idx=%d, curline=%d\n%!" idx line; *)
    match String.index_from input idx '\n' with
    | line_end -> loop (extend acc ~line idx ~fin:line_end) (line + 1) (line_end + 1)
    | exception Not_found ->
      List.rev (extend acc ~line idx ~fin:(String.length input - 1))
  in
  loop [] 1 0
;;

let make_lines_index input =
  let db = recover_lines input in
  (* Format.printf "db : %a\n%!" pp_lines_db db; *)
  fun pos ->
    List.find_map
      (fun (line, (start, fin)) -> if start <= pos && pos <= fin then Some line else None)
      db
;;

let%test "file head 1 " =
  let input =
    {|
diff -N -u old/changed.txt new/changed.txt
--- old/changed.txt  2022-09-18 16:48:36.487062439 +0300
+++ new/changed.txt  2022-09-18 16:48:36.487062439 +0300
@@ -1,3 +1,4 @@
|}
  in
  Angstrom.parse_string ~consume:Consume.Prefix file_head input
  = Result.ok ("old/changed.txt", "new/changed.txt")
;;

let%test "chunk item 1" =
  let input = "+      helper b (a+b) (n-1)" in
  match Angstrom.parse_string ~consume:Consume.All Line_parser.(run chunk_item) input with
  | Result.Error _ -> false
  | Ok (Add, str) when str = "      helper b (a+b) (n-1)" -> true
  | Ok (Add, _) -> false
  | _ -> false
;;

let%expect_test _ =
  let lst = recover_lines "aaaa\nbbbbb\ncccc\naaaaaaaaaaaaaa" in
  Format.printf "[%a]\n%!" pp_lines_db lst;
  [%expect {|
    [(1, (1, 4)) (2, (6, 10)) (3, (12, 15))
    (4, (17, 29))] |}]
;;

let%expect_test _ =
  let lst = recover_lines "a\nb\nc\nd" in
  Format.printf "[%a]\n%!" pp_lines_db lst;
  [%expect {|
    [(1, (1, 1)) (2, (3, 3)) (3, (5, 5))
    (4, (7, 6))] |}]
;;

let%expect_test _ =
  let lst = recover_lines "\n\n\n" in
  Format.printf "[%a]\n%!" pp_lines_db lst;
  [%expect {|
    [(1, (1, 0)) (2, (2, 1))
    (3, (3, 2))] |}]
;;
