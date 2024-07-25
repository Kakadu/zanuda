[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "list_fusion"
let group = LINT.Perf
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does

Performs List fusion (a.k.a. deforestation) for OCaml lists.

#### Explanation

When you performing bunch of list operations, for example `List.map f (List.map g ...)` they has a performance problem:
the intermediate lists that are created glut a lot of memory. It's recommended to rewrite the code using 'free thorems'.
See the original paper [P.Wadler "Deforestation: transforming programs to eliminate trees" (1990)](https://homepages.inf.ed.ac.uk/wadler/topics/deforestation.html) for more details.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

type kind =
  | MapMap
  | FilterMap
  | ConcatMap

let msg ppf kind =
  let s1, s2 =
    match kind with
    | MapMap -> "List.map f (List.map g xs)", "List.map (fun y -> f (g y)) xs"
    | FilterMap ->
      ( "List.filter f (List.map g xs)"
      , "List.filter_map (fun x -> let r = g x in if f r then Some y else None) xs" )
    | ConcatMap -> "List.concat (List.map f xs)", "List.concat_map f xs"
  in
  Caml.Format.fprintf
    ppf
    "Performance issue. It's recommended to rewrite\n\t'%s'\nas\n\t'%s'\n%!"
    s1
    s2
;;

let report filename ~loc e =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg e

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        e
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    let list_map =
      texp_ident (path [ "List"; "map" ])
      ||| texp_ident (path [ "Stdlib"; "List"; "map" ])
    in
    let list_filter =
      texp_ident (path [ "List"; "filter" ])
      ||| texp_ident (path [ "Stdlib"; "List"; "filter" ])
    in
    let list_concat =
      texp_ident (path [ "List"; "concat" ])
      ||| texp_ident (path [ "Stdlib"; "List"; "concat" ])
    in
    texp_apply_nolabelled list_map (drop ^:: texp_apply list_map drop ^:: nil)
    |> map0 ~f:MapMap
    ||| (texp_apply_nolabelled list_filter (drop ^:: texp_apply list_map drop ^:: nil)
         |> map0 ~f:FilterMap)
    ||| (texp_apply list_concat ((nolabel ** some (texp_apply list_map drop)) ^:: nil)
         |> map0 ~f:ConcatMap)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (* if String.is_substring loc.loc_start.pos_fname ~substring:"Fusion"
           then (
           let u = Untypeast.(default_mapper.expr default_mapper expr) in
           Format.printf "%a\n%a\n%!" Pprintast.expression u (Printast.expression 0) u); *)
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun detected () ->
            CollectedLints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc detected))
          ();
        fallback.expr self expr)
  }
;;
