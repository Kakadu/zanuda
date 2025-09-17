[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Utils
open Parsetree

type input = Tast_iterator.iterator

let lint_id = "no_docs_parsetree"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
It checks that file `Parsetree.mli` has documentation comments for all constructors.
Usually files like this are used to describe abstract syntax tree (AST) of a language.
In this case it's recommended to annotate every constructor with a documentation about meaning of the constructors, for example, which real syntax if supposed to be parsed to this part of AST.

As example of this kind of documentation you can consult [OCaml 4.14.2 parse tree](https://github.com/ocaml/ocaml/blob/4.14.2/parsing/parsetree.mli#L286)
  |}
  |> String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~group:LINT.Style ~docs:documentation
;;

let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt
let msg ppf = Format.fprintf ppf "Constructor '%s' has no documentation attribute"

let report ~filename cname ~loc =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg cname

    let rdjsonl ppf () =
      Report.rdjsonl
        ~code:lint_id
        ~loc
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        cname
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

open Typedtree

let run info (fallback : Tast_iterator.iterator) =
  let source_file = Utils.source_of_info info in
  if Config.verbose () then printfn "Trying lint '%s' on file '%s'" lint_id source_file;
  if
    String.ends_with ~suffix:"arsetree.mli" source_file
    || String.ends_with ~suffix:"ast.mli" source_file
  then
    { fallback with
      type_kind =
        (fun self cd ->
          fallback.type_kind self cd;
          match cd with
          | Ttype_variant cds ->
            List.iter
              (fun cd ->
                let loc = cd.Typedtree.cd_loc in
                let filename = loc.Location.loc_start.Lexing.pos_fname in
                if not (List.exists is_doc_attribute cd.cd_attributes)
                then Collected_lints.add ~loc (report ~filename cd.cd_name.txt ~loc))
              cds
          | _ -> ())
    }
  else fallback
;;
