(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Stdlib.Format
open Zanuda_core
open Utils

let is_camel_case s = String.(lowercase s <> s)
let is_good_name s = String.is_prefix s ~prefix:"_menhir_cell1_" || not (is_camel_case s)
let lint_id = "camel_cased_types"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
Checks that type names are using snake case (`very_useful_typ`) and not using camel case (`veryUsefulTyp`) popular in Python and Haskell.

### Why is this bad?
Wrong casing is not exactly bad but OCaml tradition says that types' and module types' names should be snake case. Modules names' in standard library are in camel case but in most Janestreet libraries (ppxlib, base) they are in snake case too.
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

type input = Ast_iterator.iterator

open Ast_iterator

let msg ppf name = fprintf ppf "Type name `%s` should be in snake case" name

let report ~loc ~filename typ_name =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg typ_name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        typ_name
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  { fallback with
    type_declaration =
      (fun self tdecl ->
        let open Parsetree in
        let tname = tdecl.ptype_name.txt in
        let loc = tdecl.ptype_loc in
        if not (is_good_name tname)
        then (
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~loc ~filename tname));
        fallback.type_declaration self tdecl)
  }
;;
