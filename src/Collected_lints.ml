[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type config = { mutable files_to_skip : string list }

let config = { files_to_skip = [] }

let process_switches = function
  | files ->
    config.files_to_skip <- Stdlib.String.split_on_char ',' files @ config.files_to_skip
;;

open Utils

include struct
  let found_Lints : (Location.t * (module LINT.REPORTER)) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints

  let add ~loc m =
    if
      List.exists
        (fun prefix ->
          let fname = loc.Warnings.loc_start.Lexing.pos_fname in
          String.starts_with ~prefix fname
          || String.starts_with ~prefix:("_build/default/" ^ prefix) fname)
        config.files_to_skip
    then ()
    else Queue.add (loc, m) found_Lints
  ;;

  let loc_lints f = Queue.fold (fun acc x -> f x :: acc) [] found_Lints
end

let make_sarif_json results : Utils.json =
  let tool =
    `Assoc
      [ "driver", `Assoc [ "name", `String "zanuda"; "semanticVersion", `String "1.0.0" ]
      ]
  in
  `Assoc
    [ "version", `String "2.1.0"
    ; "runs", `List [ `Assoc [ "tool", tool; "results", `List results ] ]
    ]
;;

let report () =
  let iter_lints =
    let arr = Queue.to_seq found_Lints |> Array.of_seq in
    let cmp : Location.t * _ -> _ -> _ = fun (a, _) (b, _) -> Stdlib.compare a b in
    Array.sort cmp arr;
    fun f ->
      if Array.length arr = 0
      then ()
      else (
        let n = ref 0 in
        f !n arr.(0);
        incr n;
        for i = 1 to Array.length arr - 1 do
          if cmp arr.(i) arr.(i - 1) <> 0
          then (
            f !n arr.(i);
            incr n)
        done)
  in
  iter_lints (fun _ (_loc, (module M : LINT.REPORTER)) -> M.txt Format.std_formatter ());
  Format.pp_print_flush Format.std_formatter ();
  Config.out_rdjsonl ()
  |> Option.iter (fun filename ->
    if String.equal filename Filename.null
    then ()
    else (
      let () = Unix.close (Unix.openfile filename [ Unix.O_CREAT ] 0o640) in
      Out_channel.with_open_text filename (fun ch ->
        let ppf = Format.formatter_of_out_channel ch in
        iter_lints (fun _ (_loc, (module M : LINT.REPORTER)) -> M.rdjsonl ppf ());
        Format.fprintf ppf "%!")));
  Config.out_sarif ()
  |> Option.iter (fun filename ->
    if String.equal filename Filename.null
    then ()
    else (
      let () = Unix.close (Unix.openfile filename [ Unix.O_CREAT ] 0o640) in
      let json =
        let jsons = ref [] in
        iter_lints (fun _ (_loc, (module M : LINT.REPORTER)) ->
          match M.sarif () with
          | None ->
            Format.eprintf
              "Sarif output is not impelemnted for something. txt is\n%a"
              M.txt
              ();
            ()
          | Some j -> if M.is_valid () then jsons := j :: !jsons);
        make_sarif_json (List.rev !jsons)
      in
      Out_channel.with_open_text filename (fun ch ->
        let ppf = Format.formatter_of_out_channel ch in
        Yojson.Safe.pretty_print ppf json;
        Format.fprintf ppf "\n%!")))
;;

let tdecls : (Location.t, unit) Hashtbl.t = Hashtbl.create 123
let clear_tdecls () = Hashtbl.clear tdecls
let add_tdecl key = Hashtbl.add tdecls key ()
let has_tdecl_at key = Hashtbl.mem tdecls key
