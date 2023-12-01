(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Caml.Format
open Zanuda_core
open Utils

type input = Ast_iterator.iterator

let lint_id = "manual_map"
let lint_source = LINT.Camelot
let group = LINT.Style
let level = LINT.Warn

let documentation =
  {|
### What it does
Proposes to use `List.map` instead of manual implementation, such as

```ocaml
  let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
```

### Why?
It is too verbose and most likely less perfomant.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

open Parsetree
open Ast_iterator

let msg ppf name = Format.fprintf ppf "Consider using `List.map` instead of `%s`%!" name

let report ~loc ~filename name =
  let module M = struct
    let txt ppf () = Report.txt ~filename ~loc ppf msg name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        name
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let is_applied_to_tail fun_name tail f args =
  match f.pexp_desc with
  | Pexp_ident { txt = Lident f; _ } ->
    String.equal f fun_name
    && List.exists
         ~f:(fun (_, arg) ->
           match arg.pexp_desc with
           | Pexp_ident { txt = Lident a; _ } -> String.equal a tail
           | _ -> false)
         args
  | _ -> false
;;

let run _ fallback =
  let pat =
    let open Ppxlib.Ast_pattern in
    let cases =
      alt
        (case
           ~lhs:
             (ppat_construct
                (lident (string "::"))
                (some (drop ** ppat_tuple (drop ^:: ppat_var __ ^:: nil))))
           ~guard:none
           ~rhs:
             (pexp_construct
                (lident (string "::"))
                (some (pexp_tuple (drop ^:: pexp_apply __ __ ^:: nil))))
         ^:: case
               ~lhs:(ppat_construct (lident (string "[]")) drop)
               ~guard:none
               ~rhs:(pexp_construct (lident (string "[]")) drop)
         ^:: nil)
        (case
           ~lhs:(ppat_construct (lident (string "[]")) drop)
           ~guard:none
           ~rhs:(pexp_construct (lident (string "[]")) drop)
         ^:: case
               ~lhs:
                 (ppat_construct
                    (lident (string "::"))
                    (some (drop ** ppat_tuple (drop ^:: ppat_var __ ^:: nil))))
               ~guard:none
               ~rhs:
                 (pexp_construct
                    (lident (string "::"))
                    (some (pexp_tuple (drop ^:: pexp_apply __ __ ^:: nil))))
         ^:: nil)
    in
    value_binding
      ~pat:(ppat_var __)
      ~expr:(pexp_fun drop drop drop (pexp_function cases) ||| pexp_function cases)
  in
  let parse vb =
    let loc = vb.pvb_loc in
    Ppxlib.Ast_pattern.parse
      pat
      loc
      ~on_error:(fun _desc () -> ())
      vb
      (fun fun_name tail f args () ->
        if is_applied_to_tail fun_name tail f args
        then
          CollectedLints.add
            ~loc
            (report ~filename:loc.Location.loc_start.Lexing.pos_fname ~loc fun_name))
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
