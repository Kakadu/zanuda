[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Zanuda_core
open Utils

let lint_id = "use_guard_instead_of_if"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
Pattern matching guards are not very common in mainstream languages so it easy to forget about them for OCaml wannabies.
This lint looks for if-then-else expressions in right hand sides of pattern matching, and recommends to use pattern guards.

### Why is this bad?
Sometimes guards allow you to write less error-prone code. For example, you are matching three values and want to
. if 1st fits predicate then do something and return, check other components otherwise.
. if 2nd fits predicate then do something and return, check other components otherwise.
. if 3rd ..., do something else otherwise.

The implementation with if-then-else could be like this.
```ocaml
match ... with
| (a,b,c) ->
    if pred1 a then ...
    else if pred2 b then ...
    else if pred3 c then ...
    else ... something_else ...
| ...
```
In this case all three bindings are in scope in the right hand side of matching, you can by mistake use them for something. And you can't use wildcards because all three bindings are required in right hand side.

Let's rewrite it with guards:
```ocaml
match ... with
| (a,_,_) when pred1 a -> ...
| (_,b,_) when pred2 b -> ...
| (_,_,c) when pred3 c -> ...
| ...
```

In this variant you have less potential for copy-paste mistake
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~impl:LINT.Untyped ~docs:documentation
;;

open Parsetree
open Ast_iterator

type input = Tast_iterator.iterator

let msg = "Prefer guard instead of if-then-else in case construction"

let report ~filename ~loc () =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf pp_print_string msg

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        pp_print_string
        msg
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pm =
    let open Tast_pattern in
    texp_ite __' drop drop
  in
  let oncase case =
    try
      Tast_pattern.parse
        pm
        case.Typedtree.c_rhs.exp_loc
        case.Typedtree.c_rhs
        ~on_error:(fun _ -> ())
        (fun { Location.loc } ->
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          Collected_lints.add ~loc (report ~loc ~filename ()))
    with
    | Location.Error e -> Format.printf "%a\n%!" Location.print_report e
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self e ->
        let () =
          Tast_pattern.(
            parse
              (texp_function_cases drop __)
              ~on_error:(fun _ -> ())
              e.Typedtree.exp_loc
              e
              (List.iter oncase))
        in
        fallback.expr self e)
  }
;;
