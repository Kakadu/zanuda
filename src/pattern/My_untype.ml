(** These extra functions are an extension of {!Untypeast} module from compiler libs.
    Maybe in new versions of compiler they will be available out of box. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@coverage off]

(* This are helper functions related to OCaml compiler. Doesn't make sense to seriously test them *)

include Untypeast

(** {2 New functions} *)

[%%if ocaml_version < (4, 11, 0)]

let untype_expression = default_mapper.expr default_mapper

[%%endif]

let expr = untype_expression

let untype_stru_item si =
  match
    untype_structure
      Typedtree.{ str_items = [ si ]; str_type = Obj.magic 1; str_final_env = si.str_env }
  with
  | [ si ] -> si
  | _ -> failwith "A bug"
;;
