(** Aggregate all types defined in a file.
    Not really a lint but a preparation for skipping false-positives *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "misc_aggregate_defs"
let level = LINT.Warn
let lint_source = LINT.FPCourse

let documentation =
  {|
It is not really a lint. It collects locations in the file where types are declared and saves them.
If PPX-expanded expressions have issues and locations corresponding type declarations,
we don't report these false-positive lints (at the moment, only about possible eta-conversion)

|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~group:LINT.Style ~level ~docs:documentation
;;

let has_deriving_attribute (attrs : Typedtree.attributes) =
  try
    let open Parsetree in
    let _ : attribute =
      List.find
        (function
          | { attr_name = { txt = "deriving" }; _ } -> true
          | _ -> false)
        attrs
    in
    true
  with
  | Not_found -> false
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    typ =
      (fun self typ ->
        Collected_lints.add_tdecl typ.ctyp_loc;
        fallback.typ self typ)
  ; type_declaration =
      (fun self tdecl ->
        (match tdecl.typ_kind, tdecl.Typedtree.typ_manifest with
         | Ttype_variant cds, _ when has_deriving_attribute tdecl.typ_attributes ->
           (* Adding locations of constructor definitions is kind of misuse of
              [Collected_lints.add_tdecl] but is required for ppx_deriving.eq *)
           List.iter (fun cd -> Collected_lints.add_tdecl cd.Typedtree.cd_loc) cds
         | Ttype_abstract, Some t when has_deriving_attribute tdecl.typ_attributes ->
           Collected_lints.add_tdecl t.ctyp_loc
         | (Ttype_variant _ | Ttype_open | Ttype_record _), _ | Ttype_abstract, None | _
           -> ());
        fallback.type_declaration self tdecl)
  }
;;
