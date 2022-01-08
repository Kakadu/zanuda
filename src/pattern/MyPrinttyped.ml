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
