[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Utils

type input = Tast_iterator.iterator

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
    (fun (_, arg) ->
      match arg.pexp_desc with
      | Pexp_ident { txt = Lident a; _ } -> String.equal a x
      | _ -> false)
    args
;;

let is_fold fun_name tail f args =
  if String.equal fun_name f && has_arg tail args
  then Some Fold_left
  else if
    List.exists
      (fun (_, arg) ->
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
  Tast_pattern.(parse (texp_function_body drop __))
    expr.Typedtree.exp_loc
    expr
    ~on_error:(fun _ -> expr)
    Fun.id
;;

let vb_pattern () =
  let open Tast_pattern in
  let empty_case () =
    case (tpat_constructor (lident (string "[]")) drop) none (texp_ident (pident drop))
  in
  let cons_case () =
    case
      (tpat_constructor (lident (string "::")) (drop ^:: tpat_id __ ^:: nil))
      none
      (as__ (texp_apply_nolabelled drop drop))
    (* |> map3 ~f:(fun tlpat rec_ident last_arg ->
       match last_arg with
       | Path.Pident tl2 when Ident.same tlpat tl2 -> rec_ident
       | _ -> fail Location.none "cons_case") *)
  in
  value_binding
    (tpat_var __)
    (texp_function_cases
       (__ ^:: __ ^:: nil)
       (empty_case () ^:: cons_case () ^:: nil ||| cons_case () ^:: empty_case () ^:: nil))
  |> map5 ~f:(fun fold_ (_, (f_, _)) (_, (init_, _)) _ long_expr ->
    fold_, f_, init_, long_expr)
  ||| (value_binding
         (tpat_var __)
         (texp_function_body
            (__ ^:: __ ^:: __ ^:: nil)
            (texp_match
               (texp_ident __)
               drop
               (empty_case () ^:: cons_case () ^:: nil
                ||| cons_case () ^:: empty_case () ^:: nil)))
       |> map7
            ~f:
              (fun
                fold_
                (_, (f_, _))
                (_, (init_, _))
                (_, (list_path, _))
                list_scru
                _
                long_expr
              ->
            match list_path, list_scru with
            | list_path, Path.Pident list_scru when Ident.same list_path list_scru ->
              fold_, f_, init_, long_expr
            | _ -> fail Location.none "Something"))
;;

let rhs_parser pseudo_fold pseudo_f _pseudo_init =
  let open Tast_pattern in
  (* fold f (f acc _) tl *)
  texp_apply_nolabelled
    (texp_ident __)
    (texp_ident __ ^:: texp_apply2 (texp_ident __) __ drop ^:: __ ^:: nil)
  |> map5 ~f:(fun fold f f2 _init _ ->
    match fold, f, f2 with
    | Path.Pident fold, Path.Pident f, Path.Pident f2
      when Ident.name fold = pseudo_fold && Ident.same f pseudo_f && Ident.same f f2 ->
      Fold_left
    | _ -> fail Location.none "Not the right rhs for fold_left")
  |||
  (* f _ (fold f acc tl) *)
  (texp_apply_nolabelled
     (texp_ident __)
     (drop
      ^:: texp_apply_nolabelled (texp_ident __) (texp_ident __ ^:: __ ^:: __ ^:: nil)
      ^:: nil)
   |> map5 ~f:(fun f2 fold f _init _ ->
     match fold, f, f2 with
     | Path.Pident fold, Path.Pident f, Path.Pident f2
       when Ident.name fold = pseudo_fold && Ident.same f pseudo_f && Ident.same f f2 ->
       Fold_right
     | _ -> fail Location.none "Not the right rhs for fold_left"))
;;

let run _ (fallback : Tast_iterator.iterator) =
  let parse vb =
    let vb = { vb with Typedtree.vb_attributes = [] } in
    let loc = vb.Typedtree.vb_loc in
    Tast_pattern.parse
      (vb_pattern ())
      loc
      ~on_error:(fun _desc () -> ())
      vb
      (fun (fun_name, _f, _init, long_expr) () ->
        (* Format.printf
           "Got %s and '%a'\n%!"
           fun_name
           Pprintast.expression
           (My_untype.expr long_expr); *)
        Tast_pattern.parse
          (rhs_parser fun_name _f _init)
          long_expr.Typedtree.exp_loc
          long_expr
          ~on_error:(fun _ () -> ())
          (fun kind () ->
            Collected_lints.add
              ~loc
              (report
                 ~filename:loc.Location.loc_start.Lexing.pos_fname
                 ~loc
                 kind
                 fun_name))
          ())
      ()
  in
  { fallback with
    structure_item =
      (fun self si ->
        fallback.structure_item self si;
        match si.Typedtree.str_desc with
        | Tstr_value (Asttypes.Recursive, vbl) -> List.iter parse vbl
        | _ -> ())
  ; expr =
      (fun self e ->
        fallback.expr self e;
        match e.exp_desc with
        | Texp_let (Asttypes.Recursive, vbl, _) -> List.iter parse vbl
        | _ -> ())
  }
;;
