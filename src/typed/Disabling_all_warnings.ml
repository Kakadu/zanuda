[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.Other
let lint_id = "dont_disable_all_warnings"
let level = LINT.Warn

let documentation =
  {|
### What it does
Warns on use of `[@@@warning "-A"]`.

### Why is is important?
Disabling **all** warnings is a bad idea.
|}
  |> String.trim
;;

(* TODO(Kakadu): This is copy-paste from Hashtables.ml *)
type config = { mutable files_to_skip : string list }

let config = { files_to_skip = [] }
(*
   let process_switches = function
  | [ "ignore"; files ] ->
    config.files_to_skip <- Stdlib.String.split_on_char ',' files @ config.files_to_skip
  | other ->
    Stdlib.Printf.eprintf
      "Lint %s: Unsuported switches: %s\n"
      lint_id
      (String.concat " " other)
;; *)

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Stdlib.Format.fprintf ppf "Disabling *all* warnings is usually a bad idea.%!"
;;

let report ~filename ~loc =
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

let pat =
  let open Tast_pattern in
  (* TODO: support 'ocaml.warning' too *)
  tstr_attribute
    (attribute
       (string "warning")
       (payload_str (pstr_eval (pexp_constant (pconst_string __)) ^:: nil)))
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    structure_item =
      (fun self si ->
        let loc = si.str_loc in
        let () =
          Tast_pattern.parse
            pat
            loc
            ~on_error:(fun _ -> ())
            si
            (function
              | "-A" ->
                let filename = loc.Location.loc_start.Lexing.pos_fname in
                Collected_lints.add ~loc (report ~loc ~filename)
              | _ -> ())
        in
        fallback.structure_item self si)
  }
;;
