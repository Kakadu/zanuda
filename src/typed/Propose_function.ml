[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "propose_function"
let lint_source = LINT.FPCourse
let group = LINT.Style
let level = LINT.Warn

let documentation =
  {|
### What it does
Proposes to rewrite 'fun x -> match x with ...' to `function`.

### Why?
The `function` keyword allows more shorter syntax for pattern matching on last argument.
The lint should not be raised if scrutinee variable is used later in the code.

The following code is recommended:

```ocaml
  let f = function
    | [] -> ...
    | (x::xs) as arg -> ... x ... xs ... arg
```

And this piece of code is discouraged:

```ocaml
  let f arg  = match arg with
    | [] -> ...
    | (x::xs) -> ... x ... xs ... arg
```
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf () = Format.fprintf ppf "Using `function` is recommended%!"

let report filename ~loc =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg ()

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        ()
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let no_ident ident c = Utils.no_ident ident (fun it -> it.case it c)

let pat () =
  let open Tast_pattern in
  texp_function_body
    ((nolabel ** __) ^:: nil)
    (as__ (texp_match (texp_ident_loc __) drop __))
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          (pat ())
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun (scru_ident, scru_loc) match_expr _scru_loc scru_pat cases () ->
            match scru_pat with
            | Path.Pident id ->
              if
                String.equal (Ident.name scru_ident) (Ident.name id)
                && List.for_all cases ~f:(no_ident scru_ident)
              then (
                Collected_lints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc);
                Refactoring.Propose_function.register_fix
                  ~loc:match_expr.exp_loc
                  scru_loc
                  cases)
            | _ -> ())
          ();
        fallback.expr self expr)
  }
;;
