[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Format = Stdlib.Format
open Utils

include struct
  open Base

  let found_Lints : (Location.t * (module LINT.REPORTER)) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc m = Queue.enqueue found_Lints (loc, m)
  let loc_lints f = Queue.map found_Lints ~f
  let iter_lints f = Queue.iter found_Lints ~f
end

let report () =
  (* let mdfile =
     match Config.Options.outfile () with
     | Some s ->
     (* Format.printf "Opening file '%s'...\n%!" s; *)
     let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
     let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
     [ ( (fun (module M : LINT.REPORTER) ppf -> M.md ppf)
        , Format.formatter_of_out_channel ch
        , ch )
      ]
     | None -> []
     in *)
  (*   let golint_files =
       match Config.Options.out_golint () with
       | Some s ->
       let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
       (* By some reason on CI Open_creat is not enough to create a file *)
       let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
       [ ( (fun (module M : LINT.REPORTER) ppf -> M.golint ppf)
        , Format.formatter_of_out_channel ch
        , ch )
      ]
       | None -> []
       in *)
  let rdjsonl_files =
    match Config.out_rdjsonl () with
    | Some s ->
      let (_ : int) = Sys.command (Format.asprintf "touch %s" s) in
      (* By some reason on CI Open_creat is not enough to create a file *)
      let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
      [ ( (fun (module M : LINT.REPORTER) -> M.rdjsonl)
        , Caml.Format.formatter_of_out_channel ch
        , ch )
      ]
    | None -> []
  in
  let all_files =
    List.concat
      [ rdjsonl_files
        (* golint_files  *)
        (* mdfile *)
      ]
  in
  Base.Exn.protect
    ~f:(fun () ->
      iter_lints (fun (_loc, ((module M : LINT.REPORTER) as m)) ->
        M.txt Format.std_formatter ();
        ListLabels.iter all_files ~f:(fun (f, ppf, _) -> f m ppf ())))
    ~finally:(fun () ->
      let f (_, ppf, ch) =
        Format.fprintf ppf "%!";
        Caml.close_out ch
      in
      List.iter f all_files)
;;

let tdecls : (Location.t, unit) Hashtbl.t = Hashtbl.create 123
let clear_tdecls () = Hashtbl.clear tdecls
let add_tdecl key = Hashtbl.add tdecls key ()
let has_tdecl_at key = Hashtbl.mem tdecls key
