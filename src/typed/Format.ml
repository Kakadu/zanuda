[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
module Format = Stdlib.Format
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.Camelot
let lint_id = "format_module_usage"
let level = LINT.Warn

let documentation =
  {|
### What it does
OCaml formatted string are more powerful than C counterpart. 
You should be aware of available features.

### Why is is important?
Shorter code is more readable. Rewrite

*  `\"%s\"` to `%S`
|}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Stdlib.Format.fprintf
    ppf
    "The format string is too much verbose (rewrite \"%%s\" -> %%S)%!"
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
    texp_construct
      (elongident Longident.(Ldot (Lident "CamlinternalFormatBasics", "Format")))
      drop
      (drop ^:: estring ^:: nil)
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
          (fun fmt_string () ->
            if String.is_substring fmt_string ~substring:"\"%s\""
            then
              CollectedLints.add
                ~loc
                (report loc.Location.loc_start.Lexing.pos_fname ~loc))
          ();
        fallback.expr self expr)
  }
;;
