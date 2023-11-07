(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

include Untypeast

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
