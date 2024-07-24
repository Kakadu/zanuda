(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
module Format = Stdlib.Format
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.FPCourse
let lint_id = "mutable_hashtables"

(* TODO: rename that it checks mutability in general *)
let level = LINT.Warn

let documentation =
  {|
### What it does
Using mutable data structures for teaching purposes is usually discouraged. Replace \
Hashtables by standard tree-like maps or consider Hash-Array Mapped Tries (HAMT).

##### How to fix?
Use mutable `ref`erences and mutable structure fields only if it is really required. Usually, mutability is added for performance reasons. For example,

  * [Effective generalization](https://okmij.org/ftp/ML/generalization.html) in OCaml type checher
  * Functions, that count number of invocations and/or generate unique names.
  * Implementing memoization or laziness
|}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

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
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg kind

    let rdjsonl ppf () =
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
  let check loc pat x =
    Tast_pattern.parse
      pat
      loc
      ~on_error:(fun _desc () -> ())
      x
      (fun () ->
        CollectedLints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
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
        match td.Typedtree.typ_type.Types.type_kind with
        | Types.Type_abstract | Type_open -> ()
        | Type_record (labels, _) ->
          List.iter labels ~f:(function
            | { ld_mutable = Mutable; ld_loc = loc; _ } ->
              CollectedLints.add
                ~loc
                (report loc.Location.loc_start.Lexing.pos_fname ~loc ())
            | _ -> ())
        | Type_variant _ ->
          (* TODO(Kakadu): Algbraic constuctors could have mutable record arguments *)
          ())
  ; expr =
      (fun self expr ->
        let loc = expr.Typedtree.exp_loc in
        Tast_pattern.parse
          pat_expr
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun () ->
            CollectedLints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
