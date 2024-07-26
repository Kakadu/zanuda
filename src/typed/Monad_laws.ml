[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.FPCourse
let level = LINT.Warn
let lint_id = "monad_laws_simplify"

let documentation =
  {|
### What it does?
Warns if monadic code could be simplified.

### Monad laws
  1) **return x >>= f === f x** for any  f and x
  2) **m >>= return === m** for any monadic value m
  3) **(m >>= g) >>= k  ===  m >>= fun x -> ((g x) >>= k)** for any monadic values m,g,k

|}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Caml.Format.fprintf
    ppf
    "Applying monad laws allows to write monadic code in more compact way.%!"
;;

let report filename ~loc kind =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg kind

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        kind
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_apply2 (texp_ident (pident (string ">>="))) drop
    @@ texp_function
         (case
            (tpat_var __)
            none
            (texp_apply1 (texp_ident (pident (string "return"))) (texp_ident (pident __)))
          ^:: nil)
    (* TODO: invent monads to be able to check two identifiers during the matching *)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        (* let __ _ = Format.printf "%a\n%!" MyPrinttyped.expr expr in *)
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun id1 id2 () ->
            if String.equal id1 id2
            then
              Collected_lints.add
                ~loc
                (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
