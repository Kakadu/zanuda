(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let pp_lines_db xs =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_space
    (fun ppf (a, (b, c)) -> Format.fprintf ppf "(%d, (%d, %d))" a b c)
    xs
;;

let%test_module "Line parsers" =
  (module struct
    open Line_parser

    let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

    let%test "diff cmd" =
      let input = "diff -N -u old/added.txt new/added.txt" in
      parse diff_cmd input = Result.Ok ()
    ;;

    let%test "file mode new" =
      let input = "new file mode 100644" in
      parse file_mode input = Result.Ok ()
    ;;

    let%test "file mode deleted" =
      let input = "deleted file mode 100644" in
      parse file_mode input = Result.Ok ()
    ;;

    let%test "index" =
      let input = "index 000000000..a71382926" in
      parse index input = Result.Ok ()
    ;;

    let%test "remove file" =
      let input = "--- a/main.ml" in
      parse remove_file input = Result.Ok "main.ml"
    ;;

    let%test "add file" =
      let input = "+++ b/main.ml" in
      parse add_file input = Result.Ok "main.ml"
    ;;

    let%test "pos num" =
      let input = "123" in
      parse pos_num input = Result.Ok 123
    ;;

    let%test "chunk head 1" =
      let input = "@@ -1,3 +1,4 @@" in
      parse chunk_head input
      = Result.Ok { old = 1; old_range = 3; fresh = 1; fresh_range = 4 }
    ;;

    let%test "chunk head 2" =
      let input = "@@ -1 +1 @@" in
      parse chunk_head input
      = Result.Ok { old = 1; old_range = 1; fresh = 1; fresh_range = 1 }
    ;;

    let%test "chunk item add" =
      let input = "+input" in
      parse chunk_item input = Result.Ok (Add, "input")
    ;;

    let%test "chunk item del" =
      let input = "-input" in
      parse chunk_item input = Result.Ok (Del, "input")
    ;;

    let%test "chunk item leave" =
      let input = " input" in
      parse chunk_item input = Result.Ok (Leave, "input")
    ;;
  end)
;;

let%test_module "Diff parsers" =
  (module struct
    open Diff_parser

    let parse parser = Angstrom.parse_string ~consume:Angstrom.Consume.All parser

    let%test "file head 1 " =
      let input =
        {|
diff -N -u old/changed.txt new/changed.txt
--- old/changed.txt  2022-09-18 16:48:36.487062439 +0300
+++ new/changed.txt  2022-09-18 16:48:36.487062439 +0300
@@ -1,3 +1,4 @@
|}
      in
      Angstrom.parse_string ~consume:Angstrom.Consume.Prefix file_head input
      = Result.ok (Option.some ("old/changed.txt", "new/changed.txt"))
    ;;

    let%test "chunk item 1" =
      let input = "+      helper b (a+b) (n-1)" in
      match parse Line_parser.(run chunk_item) input with
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
  end)
;;
