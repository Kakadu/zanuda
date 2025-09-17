[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Format = Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "string_concat_fold"
let group = LINT.Perf
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
Concatenating multiple strings at once (`a^b^c`) has a perfomance issue. OCaml needs to allocate
memory for the result of
`a^b` and after that it needs to allocate memory for a result of concatenation `a^b` and `c`,
i.e. it allocates unneeded memory for intermediate results.
For concatenations of unbound length situation could be even worse

### How to fix
We can rewrite using `List.fold_left (^)` and similar folds of containers using
[function](https://github.com/ocaml/ocaml/blob/4.14/stdlib/string.ml#L72) `List.concat: string -> string list -> string`,
which calculates the size of final string ahead of time and avoid unneeded allocations.

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group ~level ~docs:documentation
;;

let msg ppf () =
  Format.fprintf
    ppf
    "Concatenating a container of strings via fold-like iteration may lead to \
     performance issues."
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
    let list_fold =
      texp_ident_typ
        (path [ "Stdlib"; "List"; "fold_left" ]
         ||| path [ "Stdlib!"; "List"; "fold_left" ]
         ||| path [ "Stdlib"; "Array"; "fold_left" ]
         ||| path [ "Stdlib!"; "Array"; "fold_left" ])
        (typ_arrow drop (typ_arrow drop drop))
    in
    let list_fold_labelled =
      texp_ident_typ
        (path [ "Stdlib"; "ListLabels"; "fold_left" ]
         ||| path [ "Stdlib!"; "ListLabels"; "fold_left" ])
        (typ_arrow drop (typ_arrow drop drop))
    in
    let concat_op =
      let typ_str = typ_constr (path [ "Stdlib"; "string" ] ||| path [ "string" ]) nil in
      texp_ident_typ
        (path [ "Stdlib"; "^" ] ||| path [ "Stdlib!"; "^" ])
        (typ_arrow typ_str (typ_arrow typ_str typ_str))
    in
    texp_apply list_fold ((nolabel ** some concat_op) ^:: drop)
    ||| texp_apply list_fold_labelled ((labelled (string "f") ** some concat_op) ^:: drop)
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
          (fun () ->
            Collected_lints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
