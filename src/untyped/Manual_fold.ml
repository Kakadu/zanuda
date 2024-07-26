[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Caml.Format
open Zanuda_core
open Utils

type input = Ast_iterator.iterator

let lint_id = "manual_fold"
let lint_source = LINT.Camelot
let group = LINT.Style
let level = LINT.Warn

let documentation =
  {|
### What it does
Proposes to use `List.fold_left` or `List.fold_right` instead of manual
implementations, such as:

```ocaml
  let rec fold_left f acc l = match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc a) l
```

### Why?
It is too verbose and reduces readability.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~group:LINT.Style ~docs:documentation
;;

open Parsetree
open Ast_iterator

type kind =
  | Fold_left
  | Fold_right

let msg ppf (kind, name) =
  let s =
    match kind with
    | Fold_left -> "List.fold_left"
    | Fold_right -> "List.fold_right"
  in
  Format.fprintf ppf "Consider using `%s` instead of `%s`%!" s name
;;

let report ~loc ~filename k n =
  let module M = struct
    let txt ppf () = Report.txt ~filename ~loc ppf msg (k, n)

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        (k, n)
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let has_arg x args =
  List.exists
    ~f:(fun (_, arg) ->
      match arg.pexp_desc with
      | Pexp_ident { txt = Lident a; _ } -> String.equal a x
      | _ -> false)
    args
;;

let is_fold fun_name tail f args =
  if String.equal fun_name f && has_arg tail args
  then Some Fold_left
  else if List.exists
            ~f:(fun (_, arg) ->
              match arg.pexp_desc with
              | Pexp_apply (exp, args) ->
                (match exp.pexp_desc with
                 | Pexp_ident { txt = Lident f; _ } ->
                   String.equal fun_name f && has_arg tail args
                 | _ -> false)
              | _ -> false)
            args
  then Some Fold_right
  else None
;;

let rec fun_body expr =
  let result = expr in
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, expr) -> fun_body expr
  | _ -> result
;;

let run _ fallback =
  let open Ppxlib.Ast_pattern in
  let cases =
    let empty_case () =
      case
        ~lhs:(ppat_construct (lident (string "[]")) drop)
        ~guard:none
        ~rhs:(pexp_ident (lident drop))
    in
    let cons_case () =
      case
        ~lhs:
          (ppat_construct
             (lident (string "::"))
             (some (drop ** ppat_tuple (drop ^:: ppat_var __ ^:: nil))))
        ~guard:none
        ~rhs:(pexp_apply (pexp_ident (lident __)) __)
    in
    cons_case () ^:: empty_case () ^:: nil ||| empty_case () ^:: cons_case () ^:: nil
  in
  let pat_main = value_binding ~pat:(ppat_var __) ~expr:(pexp_fun drop drop drop __) in
  let pat_exp = pexp_match drop cases ||| pexp_function cases in
  let parse vb =
    (* We hide attributes. Don't know why it is really needed.
       TODO: Rewrite to typed tree and see what will happen.
    *)
    let vb = { vb with pvb_attributes = [] } in
    let loc = vb.pvb_loc in
    Ppxlib.Ast_pattern.parse
      pat_main
      loc
      ~on_error:(fun _desc () -> ())
      vb
      (fun fun_name expr () ->
        let body = fun_body expr in
        Ppxlib.Ast_pattern.parse
          pat_exp
          body.pexp_loc
          ~on_error:(fun _desc () -> ())
          body
          (fun tail f args () ->
            match is_fold fun_name tail f args with
            | Some kind ->
              Collected_lints.add
                ~loc
                (report
                   ~filename:loc.Location.loc_start.Lexing.pos_fname
                   ~loc
                   kind
                   fun_name)
            | None -> ())
          ())
      ()
  in
  { fallback with
    structure_item =
      (fun self si ->
        fallback.structure_item self si;
        match si.pstr_desc with
        | Pstr_value (Asttypes.Recursive, vbl) -> List.iter vbl ~f:parse
        | _ -> ())
  ; expr =
      (fun self e ->
        fallback.expr self e;
        match e.pexp_desc with
        | Pexp_let (Asttypes.Recursive, vbl, _) -> List.iter vbl ~f:parse
        | _ -> ())
  }
;;
