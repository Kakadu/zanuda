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

let diff_cmd : unit parser =
  let* _ = string "diff" in
  many any_char *> return ()
;;

let file_mode : unit parser =
  let* _ = string "new" <|> string "deleted" in
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

let no_new_line_eof : unit parser = string "\\ No newline at end of file" *> return ()

let run : ?info:string -> _ parser -> _ parser =
  fun ?info ppp ->
  log "inside run %s" (Option.fold ~none:"" ~some:Fun.id info);
  let* _ = many (char '\n') in
  let* str = take_while (fun c -> c <> '\n') in
  log "Going to parse from string %S" str;
  match parse_string ~consume:Consume.All ppp str with
  | Result.Error e ->
    log "failed: %d" __LINE__;
    fail "Line parser failed"
  | Result.Ok rez ->
    log "forcing tail";
    return rez
;;

let parse parser str = parse_string ~consume:Consume.All parser str

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
