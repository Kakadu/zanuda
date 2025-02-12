[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

(* TODO(Kakadu): This is copy-paste from Hashtables.ml *)
type config = { mutable files_to_skip : string list }

let config = { files_to_skip = [] }

let process_switches = function
  | [ "ignore"; files ] ->
    config.files_to_skip <- Stdlib.String.split_on_char ',' files @ config.files_to_skip
  | other ->
    Stdlib.Printf.eprintf
      "Lint %s: Unsuported switches: %s\n"
      lint_id
      (String.concat " " other)
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Stdlib.Format.fprintf
    ppf
    "Do you really need physical equality? Physical means the equality of pointers.%!"
;;

let report filename ~loc =
  let module M = struct
    let txt ppf () =
      if not (List.mem filename config.files_to_skip)
      then Utils.Report.txt ~filename ~loc ppf msg ()
    ;;

    let rdjsonl ppf () =
      if not (List.mem filename config.files_to_skip)
      then
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
            Collected_lints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc))
          ();
        fallback.expr self expr)
  }
;;
