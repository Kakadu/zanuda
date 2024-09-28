[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Zanuda_core
open Utils

type input = Ast_iterator.iterator

let lint_id = "expect_tests_no_names"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
Warns about expect tests without descriptions: `let%expect_test _ = ...`

### Why?
For purposes of refactoring we want to know why a certain test was written.
It allows us to decide easily if this test is still needed.
Better version is `let%expect_test "decent name" = ...`
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~impl:LINT.Untyped ~docs:documentation
;;

let msg ppf () =
  fprintf ppf "A test without description. Try `let%%expect_test %S = ..." "name"
;;

let report ~loc ~filename info =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg info

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        info
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let open Ast_iterator in
  { fallback with
    extension =
      (fun self (({ txt }, payload) as ext) ->
        fallback.extension self ext;
        if String.equal txt "expect_test"
        then (
          match payload with
          | PStr
              [ { pstr_desc =
                    Pstr_value
                      (Nonrecursive, [ { pvb_loc; pvb_pat = { ppat_desc = Ppat_any } } ])
                }
              ] ->
            let loc = pvb_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            Collected_lints.add ~loc (report ~loc ~filename ())
          | _ -> ()))
  }
;;
