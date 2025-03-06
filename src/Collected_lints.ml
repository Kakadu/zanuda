[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Utils

include struct
  let found_Lints : (Location.t * (module LINT.REPORTER)) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc m = Queue.add (loc, m) found_Lints
  let loc_lints f = Queue.fold (fun acc x -> f x :: acc) [] found_Lints
  let iter_lints f = Queue.iter f found_Lints
end

let report () =
  iter_lints (fun (_loc, (module M : LINT.REPORTER)) -> M.txt Format.std_formatter ());
  Format.pp_print_flush Format.std_formatter ();
  Config.out_rdjsonl ()
  |> Option.iter (fun filename ->
    let (_ : int) = Sys.command (Format.asprintf "touch %s" filename) in
    Out_channel.with_open_text filename (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      iter_lints (fun (_loc, (module M : LINT.REPORTER)) -> M.rdjsonl ppf ());
      Format.fprintf ppf "%!"))
;;

let tdecls : (Location.t, unit) Hashtbl.t = Hashtbl.create 123
let clear_tdecls () = Hashtbl.clear tdecls
let add_tdecl key = Hashtbl.add tdecls key ()
let has_tdecl_at key = Hashtbl.mem tdecls key
