[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "mutually_rec_types"
let lint_source = LINT.FPCourse
let group = LINT.Style
let level = LINT.Warn

let documentation =
  {|
### What it does
Using `and` where there is no mutual recursion is discouraged.

#### Explanation
The keyword `and` should be used only to declare mutually recursive types or functions.
Incorrect use of `and` can lead to confusion.
It's recommended to rewrite type declarations without `and` where mutual recursion is not required.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf strct_items =
  let decl_names =
    let open Parsetree in
    strct_items
    |> List.concat_map ~f:(fun s ->
      match s.pstr_desc with
      | Pstr_type (_, decls) ->
        List.map decls ~f:(fun d -> Format.asprintf "'%s'" d.ptype_name.txt)
      | _ -> [])
    |> String.concat ~sep:", "
  in
  Format.fprintf
    ppf
    "Unneeded mutual recursion detected in these type declarations. It's recommended to \
     rewrite %s as follows:@ %a"
    decl_names
    Pprintast.structure
    strct_items
;;

let report msg ~loc strct_items =
  let module M = struct
    let txt ppf () =
      Utils.Report.txt ~loc ~filename:loc.Location.loc_start.pos_fname ppf msg strct_items
    ;;

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.Location.loc_start.pos_fname)
        ~line:loc.Location.loc_start.pos_lnum
        msg
        strct_items
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

module SCC = Strongly_connected_components.Make (Ident)

let pp_graph ppf : Ident.Set.t Ident.Map.t -> unit =
  Ident.Map.iter (fun key set ->
    Format.fprintf ppf "%s ~~> %s\n%!" (Ident.name key) (Ident.Set.to_string set))
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    core_typ (typ_constr __ drop)
  in
  let open Typedtree in
  let open Tast_iterator in
  { fallback with
    type_declarations =
      (fun self typ_decls ->
        let _, decls = typ_decls in
        let graph =
          List.fold_left decls ~init:Ident.Map.empty ~f:(fun acc decl ->
            let extrnl_typs = Queue.create () in
            let cr_typ_it =
              { default_iterator with
                typ =
                  (fun self cr_typ ->
                    let loc = cr_typ.ctyp_loc in
                    Tast_pattern.parse
                      pat
                      loc
                      cr_typ
                      (fun p () ->
                        default_iterator.typ self cr_typ;
                        match p with
                        | Path.Pident id
                          when List.exists decls ~f:(fun d -> Ident.same d.typ_id id) ->
                          Queue.enqueue extrnl_typs id
                        | _ -> ())
                      ~on_error:(fun _desc () -> default_iterator.typ self cr_typ)
                      ())
              }
            in
            cr_typ_it.type_declaration cr_typ_it decl;
            let extrnl_typs = Ident.Set.of_list @@ Queue.to_list extrnl_typs in
            Ident.Map.add decl.typ_id extrnl_typs acc)
        in
        (* Format.printf "%a\n%!" pp_graph graph; *)
        let comps = SCC.connected_components_sorted_from_roots_to_leaf graph in
        if Array.length comps > 1
        then (
          let first_dec = List.hd_exn decls in
          let first_loc = first_dec.typ_loc in
          let last_loc = (List.last_exn decls).typ_loc in
          let loc = { first_loc with loc_end = last_loc.loc_end } in
          let correct_strcts =
            comps
            |> Array.rev
            |> Array.filter_map ~f:(function
              | SCC.No_loop id ->
                List.find_map decls ~f:(fun dec ->
                  if Stdlib.(dec = first_dec)
                  then None
                  else if Ident.same id dec.typ_id
                  then
                    Some
                      (Ast_helper.Str.type_
                         Recursive
                         [ Untypeast.(default_mapper.type_declaration default_mapper dec)
                         ])
                  else None)
              | Has_loop loop ->
                if List.mem loop first_dec.typ_id ~equal:Ident.same
                then None
                else (
                  let mtly_decls =
                    List.filter_map decls ~f:(fun dec ->
                      if List.mem loop dec.typ_id ~equal:Ident.same
                      then
                        Some
                          Untypeast.(default_mapper.type_declaration default_mapper dec)
                      else None)
                  in
                  Some (Ast_helper.Str.type_ Recursive mtly_decls)))
            |> Array.to_list
          in
          Collected_lints.add ~loc (report msg ~loc correct_strcts));
        fallback.type_declarations self typ_decls)
  }
;;
