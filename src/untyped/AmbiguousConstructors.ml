(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Ast_iterator.iterator

let lint_id = "ambiguous_constructors"
let group = LINT.Nursery
let level = LINT.Warn
let lint_source = LINT.Other

let documentation =
  {|
### What it does

Checks if there are constructor names that hide default constructor names,
such as `Some`, `None`, `Error`, `Ok`. 

### Why it is important

Shadowing names of default constructor leads to name clashes within toplevel.
Using custom constructors is recommended.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~impl:LINT.Untyped ~docs:documentation
;;

let msg ppf name =
  Caml.Format.fprintf
    ppf
    "Constructor names of type `%s` should not look like defaults"
    name
;;

let report ~loc ~filename tname =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg tname

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        tname
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let default_names = [ "Some"; "None"; "Error"; "Ok" ]

let names (cdecls : Parsetree.constructor_declaration list) =
  List.map cdecls ~f:(fun decl -> decl.pcd_name.txt)
;;

let are_bad_names names =
  List.exists names ~f:(fun name -> List.mem default_names name ~equal:String.equal)
;;

let run _ fallback =
  let open Ast_iterator in
  { fallback with
    type_declaration =
      (fun self tdecl ->
        let open Parsetree in
        let tname = tdecl.ptype_name.txt in
        let loc = tdecl.ptype_loc in
        let names =
          match tdecl.ptype_kind with
          | Ptype_variant cdecls -> names cdecls
          | _ -> []
        in
        if are_bad_names names
        then (
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~loc ~filename tname));
        fallback.type_declaration self tdecl)
  }
;;
