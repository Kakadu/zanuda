[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
module Format = Caml.Format
open Format
open Zanuda_core
open Utils

(* let is_camel_case s = String.(lowercase s <> s) *)
let lint_id = "camel_extra_dollar"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
The `@@` operator is used for writing less parentheses in expression.
Code like `f (g (h x))` could be rewritten as `f @@ g (h x)`.
But is some cases it is not required, like `print_int @@ 1`.
Some of these cases are reported by this lint.

  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~impl:LINT.Untyped ~docs:documentation
;;

type input = Ast_iterator.iterator

let msg ppf () = fprintf ppf "Extranous `@@@@`."

let report ~loc ~filename info =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg info

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        info
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let open Ast_iterator in
  { fallback with
    expr =
      (fun self e ->
        let () =
          match e.pexp_desc with
          | Parsetree.Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "@@" } }
              , [ _; (Nolabel, { pexp_desc = Pexp_ident _ }) ] )
          (* ... @@ 42 *)
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "@@" } }
              , [ _; (Nolabel, { pexp_desc = Pexp_constant _ }) ] )
          (* ... @@ (1,2,...) *)
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "@@" } }
              , [ _; (Nolabel, { pexp_desc = Pexp_tuple _ }) ] )
          (* ... @@ None *)
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "@@" } }
              , [ _; (Nolabel, { pexp_desc = Pexp_construct (_, None) }) ] )
          (* ... @@ { ... } *)
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "@@" } }
              , [ _; (Nolabel, { pexp_desc = Pexp_record _ }) ] ) ->
            let loc = e.pexp_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            Collected_lints.add ~loc (report ~loc ~filename ())
          | _ -> ()
        in
        fallback.expr self e)
  }
;;
