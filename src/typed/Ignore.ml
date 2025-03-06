[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "wrong_ignoring"
let group = LINT.Suspicious
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Using 'Stdlib.ignore' is discouraged. It's better to rewrite it with let.

#### Explanation

Let's look at expression 'ignore (f x)'. If in the future the function 'f' will accept one more argument or change return type the code above may become buggy, because the function will not be fully applied and executead (although the warning may be raised here if this warning is not masked). It's recommended to rewrite the code as 'let (_ : int) = f x in ...` where 'int' is an example of return type of the function 'f'.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf e0 =
  let open Parsetree in
  let e = My_untype.expr e0 in
  let si =
    let open Ast_helper in
    Format.asprintf
      "let (_: %a) = %a"
      Printtyp.type_expr
      e0.exp_type
      Pprintast.expression
      e
  in
  Format.fprintf ppf "Unsafe ingore. It's recommended to rewrite it as '%s'%!" si
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
    (* TODO: check that 'asdf |> ignore' is reported properly *)
    texp_apply
      (texp_ident (path [ "Stdlib"; "ignore" ]) ||| texp_ident (path [ "Base"; "ignore" ]))
      ((nolabel ** some __) ^:: nil)
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
          (fun e () ->
            Collected_lints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc e))
          ();
        fallback.expr self expr)
  }
;;
