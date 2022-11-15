include Printtyped

[%%if ocaml_version < (4, 11, 0)]

let untype_expression = default_mapper.expr default_mapper

[%%endif]

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
