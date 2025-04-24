[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Utils
open Parsetree

type input = Tast_iterator.iterator

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
It is too verbose and most likely less performant.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~group:LINT.Style ~docs:documentation
;;

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
  match f.Typedtree.exp_desc with
  | Texp_ident (Pident fid, _, _) ->
    Ident.same fid fun_name
    && ListLabels.exists
         ~f:(fun arg ->
           match arg.Typedtree.exp_desc with
           | Texp_ident (Pident aid, _, _) -> Ident.same aid tail
           | _ -> false)
         args
  | _ -> false
;;

let run _ (fallback : Tast_iterator.iterator) =
  let pat () =
    let open Tast_pattern in
    let cases =
      let empty_case () =
        case
          (tpat_constructor (lident (string "[]")) drop)
          none
          (texp_construct (lident (string "[]")) drop nil)
      in
      let cons_case () =
        case
          (tpat_constructor (lident (string "::")) (drop ^:: tpat_id __ ^:: nil))
          none
          (texp_construct
             (lident (string "::"))
             drop
             (drop ^:: texp_apply_nolabelled __ __ ^:: nil))
      in
      cons_case () ^:: empty_case () ^:: nil ||| empty_case () ^:: cons_case () ^:: nil
    in
    value_binding (tpat_id __) (texp_function_cases (drop ^:: nil) cases)
    |> map4 ~f:(fun a b c d -> a, b, c, d)
    ||| (value_binding
           (tpat_id __)
           (texp_function_body
              (drop ^:: __ ^:: nil)
              (texp_match (texp_ident __) drop cases))
         |> map6 ~f:(fun a (_, (id2, _)) id1 b c d ->
           match id1 with
           | Path.Pident id1 when Ident.same id1 id2 -> a, b, c, d
           | _ -> fail Location.none "Some shadowing happenned"))
  in
  let parse vb =
    (* TODO: We hide attributes. Don't know why it is really needed.
       TODO: Rewrite to typed tree and see what will happen.
    *)
    let vb = { vb with Typedtree.vb_attributes = [] } in
    let loc = vb.vb_loc in
    Tast_pattern.parse
      (pat ())
      loc
      ~on_error:(fun _desc () -> 
        (* Format. printf "Skipping because of '%s'\n@[%a@]\n" _desc Pprintast.binding (My_untype.value_binding vb); *)
        ())
      vb
      (fun (fun_name, tail, f, args) () ->
        if is_applied_to_tail fun_name tail f args
        then
          Collected_lints.add
            ~loc
            (report
               ~filename:loc.Location.loc_start.Lexing.pos_fname
               ~loc
               (Ident.name fun_name)))
      ()
  in
  { fallback with
    structure_item =
      (fun self si ->
        fallback.structure_item self si;
        match si.str_desc with
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
