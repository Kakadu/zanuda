[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.FPCourse
let lint_id = "mutability_check"
let level = LINT.Warn

let documentation =
  {|
### What it does
Using mutable data structures for teaching purposes is usually discouraged. Replace \
Hashtables by standard tree-like maps or consider Hash-Array Mapped Tries (HAMT).

##### How to fix?
Use mutable `ref`erences and mutable record fields only if it is really required.
Usually, mutability is added for performance reasons. For example,

  * [Effective generalization](https://okmij.org/ftp/ML/generalization.html) in OCaml type checher
  * Functions, that count number of invocations and/or generate unique names.
  * Implementing memoization or laziness
|}
  |> Stdlib.String.trim
;;

type config = { mutable files_to_skip : string list }

let config = { files_to_skip = [] }

let process_switches = function
  | [ "ignore"; files ] ->
    config.files_to_skip <- Stdlib.String.split_on_char ',' files @ config.files_to_skip
  | other ->
    Stdlib.Printf.eprintf
      "Lint %s: Unsuported switches: %s\n"
      lint_id
      (String.concat " " other)
;;

let describe_as_json () =
  describe_as_clippy_json ~group:LINT.Style lint_id ~docs:documentation
;;

let msg ppf () =
  Format.fprintf
    ppf
    "Using mutable data structures for teaching purposes is usually discouraged. Replace \
     Hashtables by standard tree-like maps or consider Hash-Array Mapped Tries (HAMT). \
     Use mutable `ref`erences and mutable structure fields only if it is really \
     required. In all places where it is needed indeed, describe in a comment why it is \
     needed there.%!"
;;

let report filename ~loc kind =
  let module M = struct
    let txt ppf () =
      if not (Base.List.mem config.files_to_skip filename ~equal:String.equal)
      then Utils.Report.txt ~filename ~loc ppf msg kind
    ;;

    let rdjsonl ppf () =
      if not (Base.List.mem config.files_to_skip filename ~equal:String.equal)
      then
        RDJsonl.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          kind
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat_expr =
    let open Tast_pattern in
    let typ_hashtbl =
      typ_constr (path [ "Stdlib"; "Hashtbl"; "t" ]) (drop ^:: drop ^:: nil)
    in
    let typ_hashtbl_base =
      typ_constr (path [ "Base"; "Hashtbl"; "t" ]) (drop ^:: drop ^:: nil)
    in
    let typ_ref = typ_constr (path [ "Stdlib"; "ref" ]) (drop ^:: nil) in
    let typ_ref_base = typ_constr (path [ "Base"; "ref" ]) (drop ^:: nil) in
    texp_ident_typ drop (typ_hashtbl ||| typ_hashtbl_base ||| typ_ref ||| typ_ref_base)
  in
  let pat_typ =
    let open Tast_pattern in
    let typ_ref = typ_constr (path [ "Stdlib"; "ref" ]) (drop ^:: nil) in
    let typ_ref_base = typ_constr (path [ "Base"; "ref" ]) (drop ^:: nil) in
    core_typ (typ_ref ||| typ_ref_base)
  in
  let kind_pat =
    let open Tast_pattern in
    map0 typ_kind_open ~f:`Skip
    ||| map0 typ_kind_abstract ~f:`Skip
    ||| map0 typ_kind_variant ~f:`Variant
    ||| map1 (typ_kind_record __) ~f:(fun labels -> `Check_labels labels)
  in
  let check loc pat x =
    Tast_pattern.parse
      pat
      loc
      ~on_error:(fun _desc () -> ())
      x
      (fun () ->
        Collected_lints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
      ()
  in
  let open Tast_iterator in
  { fallback with
    typ =
      (fun self typ ->
        let loc = typ.Typedtree.ctyp_loc in
        check loc pat_typ typ;
        fallback.typ self typ)
  ; type_declaration =
      (fun self td ->
        fallback.type_declaration self td;
        Tast_pattern.(parse kind_pat) td.Typedtree.typ_loc td.Typedtree.typ_kind (function
          | `Skip -> ()
          | `Variant ->
            ()
            (* TODO(Kakadu): Algebraic constuctors could have mutable record arguments (issue #59) *)
          | `Check_labels labels ->
            ListLabels.iter labels ~f:(function
              | { Typedtree.ld_mutable = Mutable; ld_loc = loc; _ } ->
                Collected_lints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc ())
              | _ -> ())))
  ; expr =
      (fun self expr ->
        let loc = expr.Typedtree.exp_loc in
        Tast_pattern.parse
          pat_expr
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun () ->
            Collected_lints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
