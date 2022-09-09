[@@@ocaml.warnerror "-26-32-33-69"]

let lookup line_of_pos parsed ~file ~line =
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
             | `Leave | `Add -> loop (cur + 1) tl
             | `Del -> loop cur tl)
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
;;

type cfg =
  { mutable from : string option
  ; mutable file : string
  ; mutable line : int option
  }

let cfg = { from = None; file = ""; line = None }

let () =
  Arg.parse
    [ "-", Arg.Unit (fun () -> cfg.from <- None), " use stdin"
    ; "-f", Arg.String (fun s -> cfg.file <- s), " lookup for file"
    ; "-l", Arg.Int (fun n -> cfg.line <- Some n), " lookup for line in a file"
    ]
    (fun _ -> assert false)
    " "
;;

let () =
  let input =
    match cfg.from with
    | None -> Stdio.(In_channel.input_all stdin)
    | Some file -> Stdio.(In_channel.read_all file)
  in
  match Diff_parser.parse_string input with
  | Result.Ok parsed ->
    (match cfg with
     | { line = None } -> ()
     | { file; line = Some line } ->
       lookup (Diff_parser.make_lines_index input) parsed ~file ~line)
  | Error s -> Printf.eprintf "parsing failed: %s\n" s
;;
