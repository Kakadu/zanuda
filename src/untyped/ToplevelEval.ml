(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "no_toplevel_eval"
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Adding toplevel evaluation statements is not recommended because it forces to add `;;`. Rewrite using `let () = ...`
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt
let msg ppf () = fprintf ppf "Toplevel eval not recommended"

let report ~filename ~loc =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg ()

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        ()
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run { Compile_common.source_file; _ } (fallback : Ast_iterator.iterator) =
  { fallback with
    structure_item =
      (fun self si ->
        (* TODO: convert ASTs and use Ast_pattern *)
        match si.pstr_desc with
        | Pstr_eval (_, _) ->
          let loc = si.pstr_loc in
          CollectedLints.add ~loc (report ~filename:source_file ~loc);
          fallback.structure_item self si
        | _ -> fallback.structure_item self si)
  ; expr =
      (fun self e ->
        match e.pexp_desc with
        | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (ein, _) } ]) ->
          self.expr self ein
        | _ -> ())
  ; attribute =
      (fun _ attr ->
        (* we don't check inside attributes *)
        fallback.attribute fallback attr)
  }
;;
