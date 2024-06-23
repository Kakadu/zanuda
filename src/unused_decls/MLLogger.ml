(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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

let run _ fallback =
  let rec get_ident_string path =
    match path with
    | Path.Pident id -> Some (Ident.name id)
    | Path.Pdot (lhs, rhs) ->
      get_ident_string lhs |> Option.map ~f:(fun str -> str ^ "." ^ rhs)
    | _ -> None
  in
  let pat =
    let open Tast_pattern in
    texp_ident __
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path () ->
            (*Format.printf "path: %s\n" (String.concat ~sep:", " (List.map ~f:Ident.unique_toplevel_name (Path.heads path)));*)
            (*Format.printf
              "ident in %s: %s\n"
              loc.Location.loc_start.Lexing.pos_fname
              (CollectedDecls.print_path path);*)
            match path, get_ident_string path with
            | Pdot (_, _), Some str ->
              CollectedDecls.add_used_decl (Path.head path |> Ident.name) str
            | _, _ -> ()
            (*Format.printf "%s\n" (Ident.unique_toplevel_name (Path.head path))*))
          ();
        fallback.expr self expr)
  }
;;
