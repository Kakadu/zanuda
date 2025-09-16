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

let default_mapper =
  { Untypeast.default_mapper with
    expr =
      (fun self e ->
        match e.exp_desc with
        | Typedtree.Texp_construct
            ( _
            , cd
            , [ _
              ; ({ exp_desc = Texp_constant (Asttypes.Const_string (_str_fmt, _, None)) }
                 as fmt_str_expr)
              ] )
          when String.equal cd.Types.cstr_name "Format" ->
          default_mapper.expr self fmt_str_expr
        | _ -> default_mapper.expr self e)
  }
;;

let expr = default_mapper.expr default_mapper

let untype_stru_item si =
  match
    untype_structure
      Typedtree.{ str_items = [ si ]; str_type = Obj.magic 1; str_final_env = si.str_env }
  with
  | [ si ] -> si
  | _ -> failwith "A bug"
;;

let value_binding = default_mapper.value_binding default_mapper
