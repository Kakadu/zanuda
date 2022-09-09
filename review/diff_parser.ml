open Angstrom
open Types

type +'a parser = 'a Angstrom.t

[@@@ocaml.warnerror "-26-32-69"]

let use_logging = true
let use_logging = false

let log fmt =
  if use_logging
  then Caml.Format.kasprintf (Caml.Format.printf "%s\n%!") fmt
  else Caml.Format.ifprintf Caml.Format.std_formatter fmt
;;

let is_correct_chunk info lines =
  let old, fresh =
    List.fold_left
      (fun (oacc, facc) -> function
        | `Del, _, _ -> 1 + oacc, facc
        | `Add, _, _ -> oacc, 1 + facc
        | `Leave, _, _ -> 1 + oacc, 1 + facc)
      (0, 0)
      lines
  in
  log "@@@@ -%d,%d +%d,%d " info.old info.old_range info.fresh info.fresh_range;
  let pk = function
    | `Add -> '+'
    | `Del -> '-'
    | _ -> ' '
  in
  List.iteri (fun i (k, s, _) -> log "%d:\t%c%s" i (pk k) s) lines;
  assert (info.old_range = old);
  assert (info.fresh_range = fresh);
  true
;;

let file_head : _ parser =
  log "%d" __LINE__;
  let* () = option () Line_parser.(run ~info:"diff_cmd" diff_cmd) in
  log "%d: file_head" __LINE__;
  let* old_file = Line_parser.(run ~info:"remove_file" remove_file) in
  log "%d: old_file = %s" __LINE__ old_file;
  let* new_file = Line_parser.(run ~info:"new_file" add_file) in
  log "%d: new_file = %S" __LINE__ new_file;
  return (old_file, new_file)
;;

let a_chunk : chunk parser =
  (* log "%d" __LINE__; *)
  let* start_pos = pos in
  let* info, first = Line_parser.(run ~info:"chunk_head" chunk_head) in
  (* log "%d" __LINE__; *)
  (* let* _ = return (Sys.command "notify-send a_chunk") in *)
  let* diffs =
    many
      (let* pos = pos in
       let* kind, s = Line_parser.(run ~info:"chunk_item" chunk_item) in
       return (kind, s, pos))
  in
  (* log "%d" __LINE__; *)
  return
    (info, Option.fold ~none:diffs ~some:(fun (k, s) -> (k, s, start_pos) :: diffs) first)
;;

let parse_whole_file : file_info list parser =
  many
    (let* () = return (log "%d" __LINE__) in
     let* old_file, new_file = file_head in
     log "%d" __LINE__;
     let* chunks = many a_chunk in
     log "%d" __LINE__;
     let* _ = many (string "\n") in
     return { old_file; new_file; chunks })
;;

let parse_file_head =
  let* _ = string "diff " in
  let* _ = take_while (( <> ) '\n') in
  return ()
;;

(* let parse_file strings =
  let lines_stream = LazyStream.of_stream (Stream.of_list strings) in
  parse_string ~consume:Consume.All parse_whole_file lines_stream
;; *)

let parse_string str = parse_string ~consume:Consume.All parse_whole_file str

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
  | Ok (`Add, str) when str = "      helper b (a+b) (n-1)" -> true
  | Ok (`Add, _) -> false
  | _ -> false
;;

let blob1 = [%blob "review/blob1.diff"]

let%test "chunk head 1 " =
  let input = [%blob "review/blob1.diff"] in
  match Angstrom.parse_string ~consume:Consume.All parse_whole_file input with
  | Result.Error _ ->
    log "%d" __LINE__;
    false
  | Ok files ->
    (* List.iteri (fun i -> Format.printf "%d: %a\n%!" i Types.pp_file_info) files; *)
    assert (List.length files = 1);
    (match (List.hd files).chunks with
     | [ (k1, s1); (k2, s2) ] -> is_correct_chunk k1 s1 && is_correct_chunk k2 s2
     | _ ->
       log "%d" __LINE__;
       print_endline "HERR";
       false)
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

let pp_lines_db xs =
  Format.pp_print_list ~pp_sep:Format.pp_print_space [%show: int * (int * int)] xs
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

let make_lines_index input =
  let db = recover_lines input in
  (* Format.printf "db : %a\n%!" pp_lines_db db; *)
  fun pos ->
    List.find_map
      (fun (line, (start, fin)) -> if start <= pos && pos <= fin then Some line else None)
      db
;;
