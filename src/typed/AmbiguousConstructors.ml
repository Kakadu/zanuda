(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "ambiguous_constructors"
let group = LINT.Nursery
let level = LINT.Warn
let lint_source = LINT.Other

let documentation =
  {|
### What it does

Checks if there are constructor names that hide default constructor names
from `Stdlib`, such as `Some`, `None`, `Error`, `Ok`. 

### Why it is important

Shadowing names of default constructors may lead to name clashes within toplevel.
Using custom constructors is recommended.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf names =
  let show_names =
    List.map names ~f:(Format.asprintf "`%s`") |> String.concat ~sep:", "
  in
  Format.fprintf
    ppf
    "Constructor%s %s of this type should not look like defaults"
    (if List.length names = 1 then "" else "s")
    show_names
;;

let report ~loc ~filename names =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg names

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        names
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    core_typ (typ_constr __ drop)
  in
  let get_bad_names tdecl =
    let default_names = [ "Some"; "None"; "Error"; "Ok" ] in
    let open Typedtree in
    let names =
      match tdecl.typ_kind with
      | Ttype_variant cdecls -> List.map cdecls ~f:(fun decl -> decl.cd_name.txt)
      | _ -> []
    in
    List.filter names ~f:(fun name -> List.mem default_names name ~equal:String.equal)
  in
  let open Typedtree in
  let open Tast_iterator in
  { fallback with
    type_declaration =
      (fun self tdecl ->
        let tmfest = tdecl.typ_manifest in
        let loc = tdecl.typ_loc in
        let is_stdlib_alias =
          match tmfest with
          | None -> false
          | Some ctyp ->
            Tast_pattern.parse
              pat
              ctyp.ctyp_loc
              ctyp
              (fun p ->
                match Path.name p with
                | "Stdlib.result" | "option" -> true
                | _ -> false)
              ~on_error:(fun _ -> false)
        in
        let names = get_bad_names tdecl in
        if (not is_stdlib_alias) && not (List.is_empty names)
        then (
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~loc ~filename names));
        fallback.type_declaration self tdecl)
  }
;;
