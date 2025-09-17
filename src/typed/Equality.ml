[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.Camelot
let lint_id = "use_match_instead_of_equality"
let level = LINT.Warn

let documentation =
  {|
### What it does
For most algebraic datatypes it's better to use pattern matching then equality `(=)`. This lint reports that for
standard lists, options and bools.

Adopted from camelot's lint list.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

type kind =
  | Option
  | List
  | Bool

let pp_kind ppf = function
  | Option -> Format.fprintf ppf "option"
  | List -> Format.fprintf ppf "list"
  | Bool -> Format.fprintf ppf "bool"
;;

let msg ppf kind =
  Format.fprintf
    ppf
    "Using generic equality for type %a and other algebraic data types is not \
     recommended. Use pattern matching%!"
    pp_kind
    kind
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

(* TODO: Maybe forbid an equality on algebraic data types too ? *)
let run _ fallback =
  let pat =
    let open Tast_pattern in
    let typ_option =
      map1 ~f:(fun _ -> Option) @@ typ_constr (path [ "option" ]) (drop ^:: nil)
    in
    let typ_list =
      map1 ~f:(fun _ -> List) @@ typ_constr (path [ "list" ]) (drop ^:: nil)
    in
    let typ_bool = map1 ~f:(fun _ -> Bool) @@ typ_constr (path [ "bool" ]) nil in
    texp_ite
      (texp_apply2
         (texp_ident (path [ "Stdlib"; "=" ]))
         (texp_ident_typ drop (typ_option ||| typ_bool ||| typ_list))
         drop)
      drop
      drop
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (*         if String.is_substring loc.loc_start.pos_fname ~substring:"Equality"
                   then
                   Format.printf
                   "%a\n%!"
                   Pprintast.expression
                   Untypeast.(default_mapper.expr default_mapper expr); *)
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun k ->
            Collected_lints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc k))
          ();
        fallback.expr self expr)
  }
;;
