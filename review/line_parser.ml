open Opal
open Types

let use_logging = true
let use_logging = false

let log fmt =
  if use_logging
  then Caml.Format.kasprintf (Caml.Format.eprintf "%s\n%!") fmt
  else Caml.Format.ifprintf Caml.Format.std_formatter fmt
;;

let ( let* ) = ( >>= )
let ( *> ) a b = a >>= fun _ -> b
let ( <* ) a b = a >>= fun x -> b >>= fun _ -> return x
let char c = satisfy (( = ) c)

let ( >>| ) : (_, 'a) parser -> ('a -> 'b) -> (_, 'b) parser =
 fun x f stream ->
  match x stream with
  | None -> None
  | Some (rez, tl) -> Some (f rez, tl)
;;

let a_string s : (_, _) parser =
  let l = String.length s in
  let rec loop i =
    if i >= l
    then return ()
    else
      let* _ = satisfy (Char.equal s.[i]) in
      loop (i + 1)
  in
  loop 0
;;

let diff_cmd : (_, _) parser =
  let* _ = a_string "diff " in
  let* _ = many (satisfy (fun _ -> true)) in
  return ()
;;

let add_file : (_, _) parser =
  let* _ = a_string "+++ " in
  (* log "%d" __LINE__; *)
  let* filename = many (satisfy (fun c -> (* log "c = %c" c; *)
                                          c <> ' ' && c <> '\t')) in
  let* _ = many (satisfy (fun _ -> true)) in
  return (implode filename)
;;

let remove_file : (_, _) parser =
  let* _ = a_string "--- " in
  (* log "%d" __LINE__; *)
  let* filename = many (satisfy (( <> ) ' ')) in
  let* _ = many (satisfy (fun _ -> true)) in
  return (implode filename)
;;

let pos_num : (char, int) parser =
  let* xs = many1 digit in
  return (List.fold_left (fun acc x -> (acc * 10) + (Char.code x - Char.code '0')) 0 xs)
;;

let notify fmt = Format.kasprintf (fun s -> Sys.command ("notify-send " ^ s)) fmt

let chunk_item : (char, _ * string) parser =
  (* let* _ = return (notify "chunk_item") in *)
  let* h =
    char '+'
    (* <* return (notify "A") *)
    >>| (fun _ ->
          (* print_endline "A"; *)
          `Add)
    <|> (char '-' (*<* return (notify "D")*) >>| fun _ -> `Del)
    <|> (char ' '
        (* <* return (notify "L") *)
        >>| fun _ ->
        (* print_endline "L"; *)
        `Leave)
    (* <|> eof `Leave *)
  in
  (* let* _ = return (notify "chunk_item h = smth") in *)
  let* rest = many any >>| implode in
  (* let* _ = return (notify "chunk_item rest gotten") in *)
  return (h, rest)
;;

let chunk_head : (_, _) parser =
  let* _ = a_string "@@ " in
  let* _ = satisfy (( = ) '-') in
  let* old = pos_num <* char ',' in
  let* old_range = pos_num <* char ' ' in
  let* fresh = char '+' *> pos_num <* char ',' in
  let* fresh_range = pos_num <* a_string " @@" in
  let* imm_item = eof None <|> option None (chunk_item >>| Option.some) in
  return ({ old; old_range; fresh; fresh_range }, imm_item)
;;

let run : ?info:string -> (char, _) parser -> _ Angstrom.t =
 fun ?info ppp ->
  (* log "inside run %s" (Option.fold ~none:"" ~some:Fun.id info); *)
  let open Angstrom in
  let* _ = many (string "\n") in
  let* avai = available in
  (* log "avai = %d" avai; *)
  if avai = 0
  then fail "end reached"
  else
    let* str = take_while (fun c -> c <> '\n') <|> (many any_char >>| implode) in
    (* log "Going to parse from string %S" str; *)
    match parse ppp (LazyStream.of_string str) with
    | None ->
      (* log "failed: %d" __LINE__; *)
      fail "Line parser failed"
    | Some rez ->
      (* log "forcing tail"; *)
      return rez
;;

let%test _ =
  parse diff_cmd (LazyStream.of_string "diff -N -u old/added.txt new/added.txt") = Some ()
;;

let%test _ = parse add_file (LazyStream.of_string "+++ asdf.ml aqwer") = Some "asdf.ml"

let%test " xxx " =
  match parse chunk_head (LazyStream.of_string "@@ -1,3 +1,4 @@") with
  | None -> false
  | Some _ -> true
;;

let%test " xxx " =
  match parse chunk_item (LazyStream.of_string "+      helper b (a+b) (n-1)") with
  | None -> false
  | Some (k, _) -> k = `Add
;;
