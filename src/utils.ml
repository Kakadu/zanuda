(** Various helper functions. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format

let printfn fmt = kfprintf (fun ppf -> fprintf ppf "\n%!") std_formatter fmt

module ErrorFormat = struct
  let pp ppf ~filename ~line ~col:_ msg x =
    fprintf ppf "%s:%d:%d:%a\n%!" filename line (* col *) 0 msg x
  ;;
end

type rdjsonl_code = string * string option

module RDJsonl : sig
  val pp
    :  formatter
    -> filename:string
    -> line:int
    -> ?code:rdjsonl_code
    -> (formatter -> 'a -> unit)
    -> 'a
    -> unit
end = struct
  let pp ppf ~filename ~line ?code msg x =
    let location file ~line ~col =
      `Assoc
        [ "path", `String file
        ; "range", `Assoc [ "start", `Assoc [ "line", `Int line; "column", `Int col ] ]
        ]
    in
    let j =
      `Assoc
        ([ "message", `String (asprintf "%a" msg x)
         ; "location", location filename ~line ~col:1
         ; "severity", `String "INFO"
         ]
         @
         match code with
         | None -> []
         | Some (desc, None) -> [ "code", `Assoc [ "value", `String desc ] ]
         | Some (desc, Some url) ->
           [ "code", `Assoc [ "value", `String desc; "url", `String url ] ])
    in
    fprintf ppf "%s\n%!" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

let cut_build_dir s =
  let prefix = "_build/default/" in
  if String.starts_with ~prefix s
  then Base.String.drop_prefix s (String.length prefix)
  else s
;;

include (
struct
  open Location
  open Lexing

  type input_line =
    { text : string
    ; start_pos : int
    }

  let infer_line_numbers (lines : (int option * input_line) list)
    : (int option * input_line) list
    =
    let _, offset, consistent =
      List.fold_left
        (fun (i, offset, consistent) (lnum, _) ->
          match lnum, offset with
          | None, _ -> i + 1, offset, consistent
          | Some n, None -> i + 1, Some (n - i), consistent
          | Some n, Some m -> i + 1, offset, consistent && n = m + i)
        (0, None, true)
        lines
    in
    match offset, consistent with
    | Some m, true -> List.mapi (fun i (_, line) -> Some (m + i), line) lines
    | _, _ -> lines
  ;;

  module ISet : sig
    type 'a bound = 'a * int
    type 'a t

    (* bounds are included *)
    val of_intervals : ('a bound * 'a bound) list -> 'a t
    val mem : 'a t -> pos:int -> bool
    val find_bound_in : 'a t -> range:int * int -> 'a bound option
    val is_start : 'a t -> pos:int -> 'a option
    val is_end : 'a t -> pos:int -> 'a option
    val extrema : 'a t -> ('a bound * 'a bound) option
  end = struct
    type 'a bound = 'a * int

    (* non overlapping intervals *)
    type 'a t = ('a bound * 'a bound) list

    let of_intervals intervals =
      let pos =
        List.map
          (fun ((a, x), (b, y)) -> if x > y then [] else [ (a, x), `S; (b, y), `E ])
          intervals
        |> List.flatten
        |> List.sort (fun ((_, x), k) ((_, y), k') ->
          (* Make `S come before `E so that consecutive intervals get merged
             together in the fold below *)
          let kn = function
            | `S -> 0
            | `E -> 1
          in
          compare (x, kn k) (y, kn k'))
      in
      let nesting, acc =
        List.fold_left
          (fun (nesting, acc) (a, kind) ->
            match kind, nesting with
            | `S, `Outside -> `Inside (a, 0), acc
            | `S, `Inside (s, n) -> `Inside (s, n + 1), acc
            | `E, `Outside -> assert false
            | `E, `Inside (s, 0) -> `Outside, (s, a) :: acc
            | `E, `Inside (s, n) -> `Inside (s, n - 1), acc)
          (`Outside, [])
          pos
      in
      assert (nesting = `Outside);
      List.rev acc
    ;;

    let mem iset ~pos = List.exists (fun ((_, s), (_, e)) -> s <= pos && pos <= e) iset

    let find_bound_in iset ~range:(start, end_) =
      List.find_map
        (fun ((a, x), (b, y)) ->
          if start <= x && x <= end_
          then Some (a, x)
          else if start <= y && y <= end_
          then Some (b, y)
          else None)
        iset
    ;;

    let is_start iset ~pos =
      List.find_map (fun ((a, x), _) -> if pos = x then Some a else None) iset
    ;;

    let is_end iset ~pos =
      List.find_map (fun (_, (b, y)) -> if pos = y then Some b else None) iset
    ;;

    let extrema iset =
      if iset = [] then None else Some (fst (List.hd iset), snd (List.hd (List.rev iset)))
    ;;
  end

  (* The number of lines already printed after input.

     This is used by [highlight_terminfo] to identify the current position of the
     input in the terminal. This would not be possible without this information,
     since printing several warnings/errors adds text between the user input and
     the bottom of the terminal.

     We also use for {!is_first_report}, see below.
  *)
  let num_loc_lines = ref 0

  (* We use [num_loc_lines] to determine if the report about to be
     printed is the first or a follow-up report of the current
     "batch" -- contiguous reports without user input in between, for
     example for the current toplevel phrase. We use this to print
     a blank line between messages of the same batch.
  *)
  let is_first_message () = !num_loc_lines = 0

  (* This is used by the toplevel to reset [num_loc_lines] before each phrase *)
  let reset () = num_loc_lines := 0

  (* This is used by the toplevel *)
  let echo_eof () =
    print_newline ();
    incr num_loc_lines
  ;;

  (* Code printing errors and warnings must be wrapped using this function, in
     order to update [num_loc_lines].

     [print_updating_num_loc_lines ppf f arg] is equivalent to calling [f ppf
   arg], and additionally updates [num_loc_lines]. *)
  let print_updating_num_loc_lines ppf f arg =
    let open Format in
    let out_functions = pp_get_formatter_out_functions ppf () in
    let out_string str start len =
      let rec count i c =
        if i = start + len
        then c
        else if String.get str i = '\n'
        then count (succ i) (succ c)
        else count (succ i) c
      in
      num_loc_lines := !num_loc_lines + count start 0;
      out_functions.out_string str start len
    in
    pp_set_formatter_out_functions ppf { out_functions with out_string };
    f ppf arg;
    pp_print_flush ppf ();
    pp_set_formatter_out_functions ppf out_functions
  ;;

  (* let setup_tags () = Misc.Style.setup !Clflags.color *)

  (* module Fmt = Format_doc *)
  module Fmt = Format

  let pp_two_columns ?(sep = "|") ?max_lines ppf (lines : (string * string) list) =
    let left_column_size =
      List.fold_left (fun acc (s, _) -> Int.max acc (String.length s)) 0 lines
    in
    let lines_nb = List.length lines in
    let ellipsed_first, ellipsed_last =
      match max_lines with
      | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in
        (* the ellipsis uses one line *)
        let lines_before = (printed_lines / 2) + (printed_lines mod 2) in
        let lines_after = printed_lines / 2 in
        lines_before, lines_nb - lines_after - 1
      | _ -> -1, -1
    in
    Format.fprintf ppf "@[<v>";
    List.iteri
      (fun k (line_l, line_r) ->
        if k = ellipsed_first then Format.fprintf ppf "...@,";
        if ellipsed_first <= k && k <= ellipsed_last
        then ()
        else Format.fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r)
      lines;
    Format.fprintf ppf "@]"
  ;;

  (* [get_lines] must return the lines to highlight, given starting and ending
     positions.

     See [lines_around_from_current_input] below for an instantiation of
     [get_lines] that reads from the current input.
  *)
  let highlight_quote
    ppf
    ~(get_lines : start_pos:position -> end_pos:position -> input_line list)
    ?(max_lines = 10)
    highlight_tag
    locs
    =
    let iset =
      ISet.of_intervals
      @@ List.filter_map
           (fun loc ->
             let s, e = loc.loc_start, loc.loc_end in
             if s.pos_cnum = -1 || e.pos_cnum = -1
             then None
             else Some ((s, s.pos_cnum), (e, e.pos_cnum - 1)))
           locs
    in
    match ISet.extrema iset with
    | None -> ()
    | Some ((leftmost, _), (rightmost, _)) ->
      let lines =
        get_lines ~start_pos:leftmost ~end_pos:rightmost
        |> List.map (fun ({ text; start_pos } as line) ->
          let end_pos = start_pos + String.length text - 1 in
          let line_nb =
            match ISet.find_bound_in iset ~range:(start_pos, end_pos) with
            | None -> None
            | Some (p, _) -> Some p.pos_lnum
          in
          line_nb, line)
        |> infer_line_numbers
        |> List.map (fun (lnum, { text; start_pos }) ->
          text, Option.fold ~some:Int.to_string ~none:"" lnum, start_pos)
      in
      Fmt.fprintf ppf "@[<v>";
      (match lines with
       | [] | [ ("", _, _) ] -> ()
       | [ (line, line_nb, line_start_cnum) ] ->
         (* Single-line error *)
         Fmt.fprintf ppf "%s | %s@," line_nb line;
         Fmt.fprintf ppf "%*s   " (String.length line_nb) "";
         (* Iterate up to [rightmost], which can be larger than the length of
            the line because we may point to a location after the end of the
            last token on the line, for instance:
            {[
              token
                        ^
              Did you forget ...
            ]} *)
         for i = 0 to rightmost.pos_cnum - line_start_cnum - 1 do
           let pos = line_start_cnum + i in
           if ISet.is_start iset ~pos <> None then Fmt.fprintf ppf "@{<%s>" highlight_tag;
           if ISet.mem iset ~pos
           then Fmt.pp_print_char ppf '^'
           else if i < String.length line
           then
             (* For alignment purposes, align using a tab for each tab in the
                source code *)
             if line.[i] = '\t'
             then Fmt.pp_print_char ppf '\t'
             else Fmt.pp_print_char ppf ' ';
           if ISet.is_end iset ~pos <> None then Fmt.fprintf ppf "@}"
         done;
         Fmt.fprintf ppf "@}@,"
       | _ ->
         (* Printf.eprintf "%s. %s %d\n%!" __FUNCTION__ __FILE__ __LINE__; *)
         (* Multi-line error *)
         pp_two_columns ~sep:"|" ~max_lines ppf
         @@ List.map
              (fun (line, line_nb, line_start_cnum) ->
                let line =
                  String.mapi
                    (fun i car ->
                      if ISet.mem iset ~pos:(line_start_cnum + i) then car else '.')
                    line
                in
                line_nb, line)
              lines);
      Fmt.fprintf ppf "@]"
  ;;

  let lines_around
    ~(start_pos : position)
    ~(end_pos : position)
    ~(seek : int -> unit)
    ~(read_char : unit -> char option)
    : input_line list
    =
    seek start_pos.pos_bol;
    let lines = ref [] in
    let bol = ref start_pos.pos_bol in
    let cur = ref start_pos.pos_bol in
    let b = Buffer.create 80 in
    let add_line () =
      if !bol < !cur
      then (
        let text = Buffer.contents b in
        Buffer.clear b;
        lines := { text; start_pos = !bol } :: !lines;
        bol := !cur)
    in
    let rec loop () =
      if !bol >= end_pos.pos_cnum
      then ()
      else (
        match read_char () with
        | None ->
          (* end of input *)
          add_line ()
        | Some c ->
          incr cur;
          (match c with
           | '\r' -> loop ()
           | '\n' ->
             add_line ();
             loop ()
           | _ ->
             Buffer.add_char b c;
             loop ()))
    in
    loop ();
    List.rev !lines
  ;;

  (* Attempt to get lines from the lexing buffer. *)
  let lines_around_from_lexbuf ~(start_pos : position) ~(end_pos : position) (lb : lexbuf)
    : input_line list
    =
    (* Printf.eprintf "%s lexbuf.len = %d\n%!" __FUNCTION__ lb.Lexing.lex_buffer_len; *)
    (* Converts a global position to one that is relative to the lexing buffer *)
    let rel n = n - lb.lex_abs_pos in
    if rel start_pos.pos_bol < 0
    then
      (* Do nothing if the buffer does not contain the input (because it has been
         refilled while lexing it) *)
      []
    else (
      let pos = ref 0 in
      (* relative position *)
      let seek n = pos := rel n in
      let read_char () =
        if !pos >= lb.lex_buffer_len
        then (* end of buffer *) None
        else (
          let c = Bytes.get lb.lex_buffer !pos in
          incr pos;
          Some c)
      in
      lines_around ~start_pos ~end_pos ~seek ~read_char)
  ;;

  (* Attempt to get lines from the phrase buffer *)
  let lines_around_from_phrasebuf
    ~(start_pos : position)
    ~(end_pos : position)
    (pb : Buffer.t)
    : input_line list
    =
    let pos = ref 0 in
    let seek n = pos := n in
    let read_char () =
      if !pos >= Buffer.length pb
      then None
      else (
        let c = Buffer.nth pb !pos in
        incr pos;
        Some c)
    in
    lines_around ~start_pos ~end_pos ~seek ~read_char
  ;;

  (* A [get_lines] function for [highlight_quote] that reads from the current
     input. *)
  let lines_around_from_current_input ~start_pos ~end_pos =
    match !Location.input_lexbuf, !Location.input_phrase_buffer, !Location.input_name with
    | _, Some pb, "//toplevel//" -> lines_around_from_phrasebuf pb ~start_pos ~end_pos
    | Some lb, _, _ ->
      let xs = lines_around_from_lexbuf lb ~start_pos ~end_pos in
      (* Printf.eprintf "lines_around_from_lexbuf return list len %d\n%!" (List.length xs); *)
      xs
    | None, _, _ -> []
  ;;

  let is_dummy_loc loc =
    (* Fixme: this should be just [loc.loc_ghost] and the function should be
       inlined below. However, currently, the compiler emits in some places ghost
       locations with valid ranges that should still be printed. These locations
       should be made non-ghost -- in the meantime we just check if the ranges are
       valid. *)
    loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1
  ;;

  let is_quotable_loc loc =
    (not (is_dummy_loc loc))
    && loc.loc_start.pos_fname = !input_name
    && loc.loc_end.pos_fname = !input_name
  ;;

  let report_printer ppf loc f x =
    let highlight ppf loc =
      if is_quotable_loc loc
      then
        highlight_quote ppf ~get_lines:lines_around_from_current_input "warning" [ loc ]
    in
    Format.fprintf ppf "@[<v>%a:@ %a@]" print_loc loc highlight loc;
    Format.fprintf ppf "@[Alert zanuda-linter: @[%a@]@]@," f x;
    Format.fprintf ppf "%!"
  ;;
end :
sig
  val report_printer : formatter -> Location.t -> (formatter -> 'a -> unit) -> 'a -> unit
end)

module Report = struct
  let txt ~loc ~filename ppf msg msg_arg =
    Location.input_name := cut_build_dir filename;
    Clflags.error_style := Some Misc.Error_style.Contextual;
    let file_contents = In_channel.with_open_text filename In_channel.input_all in
    Location.input_lexbuf := Some (Lexing.from_string file_contents);
    let loc =
      let open Location in
      { loc with
        loc_start = { loc.loc_start with pos_fname = !input_name }
      ; loc_end = { loc.loc_end with pos_fname = !input_name }
      }
    in
    report_printer ppf loc msg msg_arg
  ;;

  let rdjsonl ~loc ~filename ~code ppf msg msg_arg =
    let code = code, Some "https://kakadu.github.io/zanuda/" in
    RDJsonl.pp ppf ~filename ~line:loc.Location.loc_start.pos_lnum ~code msg msg_arg
  ;;
end

let string_of_group : LINT.group -> string = function
  | LINT.Correctness -> "correctness"
  | Style -> "style"
  | Perf -> "perf"
  | Restriction -> "restriction"
  | Deprecated -> "deprecated"
  | Pedantic -> "pedantic"
  | Complexity -> "complexity"
  | Suspicious -> "suspicious"
  | Nursery -> "nursery"
;;

let string_of_level : LINT.level -> string = function
  | LINT.Allow -> "allow"
  | Warn -> "warn"
  | Deny -> "deny"
  | Deprecated -> "deprecated"
;;

let string_of_impl = function
  | LINT.Typed -> "typed"
  | _ -> "untyped"
;;

let describe_as_clippy_json
  ?(group = LINT.Correctness)
  ?(level = LINT.Deny)
  ?(impl = LINT.Typed)
  id
  ~docs
  : Yojson.Safe.t
  =
  (* List if clippy lints https://github.com/rust-lang/rust-clippy/blob/gh-pages/master/lints.json *)
  `Assoc
    [ "id", `String id
    ; "group", `String (string_of_group group)
    ; "level", `String (string_of_level level)
    ; "impl", `String (string_of_impl impl)
    ; "docs", `String docs
    ; ( "applicability"
      , `Assoc
          [ "is_multi_part_suggestion", `Bool false
          ; "applicability", `String "Unresolved"
          ] )
    ]
;;

exception Ident_is_found

let no_ident_iterator ident =
  let open Tast_iterator in
  let open Typedtree in
  { default_iterator with
    expr =
      (fun self e ->
        let rec ident_in_list = function
          | [] -> false
          | (_, (id, _)) :: _ when Ident.equal id ident -> true
          | _ :: tl -> ident_in_list tl
        in
        Tast_pattern.(
          let p1 =
            map2 (texp_function_body __ __) ~f:(fun args rhs -> `Function (args, rhs))
          in
          let p2 = map1 (texp_ident __) ~f:(fun x -> `Ident x) in
          parse
            (p1 ||| p2)
            (* TODO: should we check other patterns? *)
            e.exp_loc
            e
            ~on_error:(fun _ -> default_iterator.expr self e))
          (function
          | `Function (args, _rhs) when ident_in_list args -> ()
          | `Function (_, rhs) -> self.expr self rhs
          | `Ident (Pident id) when Ident.same id ident -> raise_notrace Ident_is_found
          | _ -> default_iterator.expr self e))
  ; case =
      (fun (type a) self (c : a case) ->
        match c.c_lhs.pat_desc with
        | Tpat_value v ->
          (match (v :> pattern) with
           | p ->
             Tast_pattern.(
               parse
                 (tpat_id __)
                 Location.none
                 p
                 ~on_error:(fun _ -> default_iterator.case self c)
                 (fun id ->
                   if Ident.equal ident id then () else default_iterator.case self c)))
        | _ -> default_iterator.case self c)
  }
;;

(* Checks that identifier is not used *)
let no_ident ident f =
  try
    f (no_ident_iterator ident);
    true
  with
  | Ident_is_found -> false
;;

let has_ident ident f = not (no_ident ident f)

[%%if ocaml_version < (5, 0, 0)]

type intf_or_impl =
  | Intf
  | Impl

let with_info _kind ~source_file f =
  Compile_common.with_info
    ~native:false
    ~source_file
    ~tool_name:"asdf" (* TODO: pass right tool name *)
    ~output_prefix:"asdf"
    ~dump_ext:"asdf"
    f
;;

[%%else]

type intf_or_impl = Unit_info.intf_or_impl

let with_info kind ~source_file =
  Compile_common.with_info
    ~native:false
    ~tool_name:"asdf" (* TODO: pass right tool name *)
    ~dump_ext:"asdf"
    (Unit_info.make ~source_file kind "")
;;

[%%endif]
[%%if ocaml_version < (5, 0, 0)]

let pp_path = Path.print

[%%else]

let pp_path = Format_doc.compat Path.print

[%%endif]
