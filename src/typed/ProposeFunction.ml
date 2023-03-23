(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "propose_function"
let lint_source = LINT.FPCourse
let group = LINT.Style
let level = LINT.Allow

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

let msg ppf () = Caml.Format.fprintf ppf "Using `function` is recommended%!"

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

let no_ident ident c =
  let exception Found in
  let open Tast_iterator in
  let open Typedtree in
  let it =
    { default_iterator with
      expr =
        (fun self e ->
          (* TODO: rewrite with FCPM *)
          match e.exp_desc with
          | Texp_ident (Path.Pident id, _, _) when Ident.equal id ident -> raise Found
          | Texp_function { param } when Ident.equal ident param -> ()
          | _ -> default_iterator.expr self e)
    ; case =
        (fun (type a) self (c : a case) ->
          match c.c_lhs.pat_desc with
          | Tpat_value v ->
            (match (v :> pattern) with
             | { pat_desc = Tpat_var (id, _) } ->
               if Ident.equal ident id then () else default_iterator.case self c
             | _ -> default_iterator.case self c)
          | _ -> default_iterator.case self c)
    }
  in
  try
    it.case it c;
    true
  with
  | Found -> false
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_function (case (tpat_var __) none (texp_match (texp_ident __) __) ^:: nil)
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
          (fun argname ident cases () ->
            match ident with
            | Path.Pident id ->
              if String.equal argname (Ident.name id)
                 && List.for_all cases ~f:(no_ident id)
              then
                CollectedLints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc)
            | _ -> ())
          ();
        fallback.expr self expr)
  }
;;
