(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
module Format = Stdlib.Format
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.Camelot
let lint_id = "physical_equality"
let level = LINT.Warn

let documentation =
  {|
### What it does
Warns about using of physical equality (of pointers) vs. structural equality (of values)

### Why is is important?
The newcomers from C++ and C# may automatically write == to test for equality, 
and get unexpected results for complex values.

If you do low level performance hacking, this lint could give false positives.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Stdlib.Format.fprintf
    ppf
    "Do you really need physical equality? Physical means the equality of pointers.%!"
;;

let report filename ~loc =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg ()

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        (fun _ _ -> ())
        ()
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_apply2
      (texp_ident (path [ "Stdlib!"; "==" ] ||| path [ "Stdlib"; "==" ]))
      drop
      drop
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let loc = expr.Typedtree.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _msg () -> ())
          expr
          (fun _ ->
            CollectedLints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc))
          ();
        fallback.expr self expr)
  }
;;
