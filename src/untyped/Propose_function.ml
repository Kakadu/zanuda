(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Ast_iterator.iterator

let lint_id = "propose_function_untyped"
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

let msg ppf () = Stdlib.Format.fprintf ppf "Using `function` is recommended%!"

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
  let open Ast_iterator in
  let open Parsetree in
  let it =
    { default_iterator with
      expr =
        (fun self e ->
          (* TODO: rewrite with FCPM *)
          match e.pexp_desc with
          | Pexp_ident { txt = Lident id } when String.equal id ident -> raise Found
          | Pexp_fun (_, _, { ppat_desc = Ppat_var { txt } }, _)
            when String.equal ident txt -> ()
          | _ -> default_iterator.expr self e)
    ; case =
        (fun self c ->
          match c.pc_lhs.ppat_desc with
          | Ppat_var { txt = id } ->
            if String.equal ident id then () else default_iterator.case self c
          | _ -> default_iterator.case self c)
    }
  in
  try
    it.case it c;
    true
  with
  | Found -> false
;;

let run _ fallback : input =
  let pat =
    let open Ppxlib.Ast_pattern in
    pexp_fun drop drop (ppat_var __) (pexp_match (pexp_ident __) __)
  in
  let open Ast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        (* Format.printf "@[%a@]\n%!" Pprintast.expression expr; *)
        let loc = expr.pexp_loc in
        Ppxlib.Ast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun argname ident cases () ->
            match ident with
            | Lident id ->
              (* Stdlib.Printf.printf "argname = %S, id = %S\n%!" argname id; *)
              if String.equal argname id && List.for_all cases ~f:(no_ident id)
              then
                CollectedLints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc)
            | _ -> ())
          ();
        fallback.expr self expr)
  }
;;
