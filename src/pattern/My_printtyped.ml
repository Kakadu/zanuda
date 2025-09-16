(** These extra functions are an extension of {!Printtyped} module from compiler libs.
    Maybe in new versions of compiler they will be available out of box. *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@coverage off]

(* This are helper functions related to OCaml compiler. Doesn't make sense to seriously test them *)

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

[%%if ocaml_version < (5, 0, 0)]

let make_Tstr_module mb_name _presence ~loc me =
  Typedtree.Tstr_module
    { mb_id = None
    ; mb_name
    ; mb_expr = me
    ; mb_attributes = []
    ; mb_presence = Types.Mp_present
    ; mb_loc = loc
    }
;;

[%%else]

let make_Tstr_module mb_name presence ~loc me =
  Typedtree.Tstr_module
    { mb_id = None
    ; mb_uid = Shape.Uid.internal_not_actually_unique
    ; mb_name
    ; mb_expr = me
    ; mb_attributes = []
    ; mb_presence = presence
    ; mb_loc = loc
    }
;;

[%%endif]

let me ppf me =
  let open Typedtree in
  implementation
    ppf
    { str_items =
        [ { str_desc =
              make_Tstr_module (Location.mknoloc None) Types.Mp_present ~loc:me.mod_loc me
          ; str_loc = me.mod_loc
          ; str_env = me.mod_env
          }
        ]
    ; str_final_env = me.mod_env
    ; str_type = []
    }
;;

[%%if ocaml_version < (5, 0, 0)]

let dummy_vb vb_pat vb_expr =
  { Typedtree.vb_pat; vb_loc = Location.none; vb_attributes = []; vb_expr }
;;

[%%else]

let dummy_vb vb_pat vb_expr =
  { Typedtree.vb_pat
  ; vb_loc = Location.none
  ; vb_rec_kind = Dynamic
  ; vb_attributes = []
  ; vb_expr
  }
;;

[%%endif]

let pattern ppf pat =
  let open Typedtree in
  let dummy_expr =
    { exp_desc = Texp_variant ("Dummy", None)
    ; exp_loc = Location.none
    ; exp_env = pat.pat_env
    ; exp_type = pat.pat_type
    ; exp_extra = []
    ; exp_attributes = []
    }
  in
  implementation
    ppf
    { str_items =
        [ { str_desc = Tstr_value (Nonrecursive, [ dummy_vb pat dummy_expr ])
          ; str_loc = pat.pat_loc
          ; str_env = pat.pat_env
          }
        ]
    ; str_final_env = pat.pat_env
    ; str_type = []
    }
;;

[%%if ocaml_version < (5, 0, 0)]

let make_int_const ~loc:_ n = Parsetree.Pconst_integer (string_of_int n, None)

[%%else]

let make_int_const ~loc n =
  { Parsetree.pconst_desc = Pconst_integer (string_of_int n, None); pconst_loc = loc }
;;

[%%endif]

let attrs ppf attrs =
  Pprintast.expression
    ppf
    (Ast_helper.Exp.constant (make_int_const ~loc:Location.none 1) ~attrs)
;;
