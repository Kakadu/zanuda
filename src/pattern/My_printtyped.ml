(** These extra functions are an extension of {!Printtyped} module from compiler libs.
    Maybe in new versions of compiler they will be available out of box. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

include Printtyped

[%%if ocaml_version < (4, 11, 0)]

let untype_expression = default_mapper.expr default_mapper

[%%endif]

(** {2 New functions} *)

let expr ppf e =
  let open Typedtree in
  implementation
    ppf
    { str_items =
        [ { str_desc = Tstr_eval (e, []); str_loc = e.exp_loc; str_env = e.exp_env } ]
    ; str_final_env = e.exp_env
    ; str_type = []
    }
;;

let me ppf me =
  let open Typedtree in
  implementation
    ppf
    { str_items =
        [ { str_desc =
              Tstr_module
                { mb_id = None
                ; mb_name = Location.mknoloc None
                ; mb_expr = me
                ; mb_attributes = []
                ; mb_presence = Types.Mp_present
                ; mb_loc = me.mod_loc
                }
          ; str_loc = me.mod_loc
          ; str_env = me.mod_env
          }
        ]
    ; str_final_env = me.mod_env
    ; str_type = []
    }
;;

let attrs ppf attrs =
  Pprintast.expression ppf (Ast_helper.Exp.constant (Pconst_integer ("1", None)) ~attrs)
;;
