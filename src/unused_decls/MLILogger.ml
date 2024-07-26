[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* TODO: remove this file *)

open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator
(*
   let lint_source = LINT.FPCourse
   let group = LINT.Style
   let level = LINT.Warn *)

(* let documentation =
  {|
Technical Lint for collecting all declarations from MLI's
|} |> Stdlib.String.trim
;; *)

let run _ fallback =
  let _ = assert false in
  let extract_module_name loc =
    loc.Location.loc_start.Lexing.pos_fname
    |> String.split_on_char '/'
    |> List.rev
    |> List.hd
    |> String.split_on_char '.'
    |> List.hd
  in
  let pat =
    let open Tast_pattern in
    tsig_val_name __
  in
  let open Tast_iterator in
  { fallback with
    signature_item =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.sig_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun id () ->
            let to_add = extract_module_name loc ^ Ident.unique_toplevel_name id in
            printfn "right: %s%!" (Ident.unique_toplevel_name id);
            CollectedDecls.add_just_decl to_add)
          ();
        fallback.signature_item self expr)
  }
;;
