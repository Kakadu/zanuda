(** Cyclomatic complexity of a function body.

    Decision points are [if], [while], [for], each [match] / [function] /
    [try] case, each [when] guard and the short-circuit operators [&&] / [||].
    A function without decisions has complexity 1. Decisions of local bindings
    stay charged to the enclosing function, matching the reference crapper's
    rule for nested functions. *)

let short_circuit (op : Ppxlib.Parsetree.expression) =
  match op.Ppxlib.Parsetree.pexp_desc with
  | Pexp_ident { txt = Lident ("&&" | "||"); _ } -> true
  | _ -> false
;;

let rec visit_case count (c : Ppxlib.Parsetree.case) =
  (* every case is a branch of its own; [when] adds another decision *)
  incr count;
  Option.iter
    (fun guard ->
      incr count;
      visit_expr count guard)
    c.Ppxlib.Parsetree.pc_guard;
  visit_expr count c.Ppxlib.Parsetree.pc_rhs

and visit_expr count (e : Ppxlib.Parsetree.expression) =
  match e.Ppxlib.Parsetree.pexp_desc with
  | Pexp_constant _ | Pexp_ident _ | Pexp_new _ | Pexp_unreachable -> ()
  | Pexp_function cases -> List.iter (visit_case count) cases
  | Pexp_fun (_, default, _, body) ->
    Option.iter (visit_expr count) default;
    visit_expr count body
  | Pexp_match (e, cases) | Pexp_try (e, cases) ->
    visit_expr count e;
    List.iter (visit_case count) cases
  | Pexp_ifthenelse (cond, then_, else_) ->
    incr count;
    visit_expr count cond;
    visit_expr count then_;
    Option.iter (visit_expr count) else_
  | Pexp_while (cond, body) ->
    incr count;
    visit_expr count cond;
    visit_expr count body
  | Pexp_for (_, from, to_, _, body) ->
    incr count;
    visit_expr count from;
    visit_expr count to_;
    visit_expr count body
  | Pexp_apply (op, args) ->
    if short_circuit op then incr count;
    visit_expr count op;
    List.iter (fun (_, arg) -> visit_expr count arg) args
  | Pexp_let (_, bindings, body) ->
    List.iter (fun vb -> visit_expr count vb.Ppxlib.Parsetree.pvb_expr) bindings;
    visit_expr count body
  | Pexp_sequence (e1, e2) ->
    visit_expr count e1;
    visit_expr count e2
  | Pexp_tuple es | Pexp_array es -> List.iter (visit_expr count) es
  | Pexp_record (fields, default) ->
    Option.iter (visit_expr count) default;
    List.iter (fun (_, e) -> visit_expr count e) fields
  | Pexp_construct (_, e) | Pexp_variant (_, e) -> Option.iter (visit_expr count) e
  | Pexp_field (e, _)
  | Pexp_setfield (e, _, _)
  | Pexp_lazy e
  | Pexp_send (e, _)
  | Pexp_constraint (e, _)
  | Pexp_coerce (e, _, _)
  | Pexp_poly (e, _) -> visit_expr count e
  | Pexp_open (open_decl, e) ->
    visit_module_expr count open_decl.Ppxlib.Parsetree.popen_expr;
    visit_expr count e
  | Pexp_object cs -> List.iter (visit_class_field count) cs.Ppxlib.Parsetree.pcstr_fields
  | Pexp_letop { let_; ands; body; _ } ->
    visit_expr count let_.Ppxlib.Parsetree.pbop_exp;
    List.iter (fun bop -> visit_expr count bop.Ppxlib.Parsetree.pbop_exp) ands;
    visit_expr count body
  | Pexp_setinstvar (_, e) | Pexp_letexception (_, e) | Pexp_assert e | Pexp_newtype (_, e)
    -> visit_expr count e
  | Pexp_override bindings -> List.iter (fun (_, e) -> visit_expr count e) bindings
  | Pexp_letmodule (_, me, e) ->
    visit_module_expr count me;
    visit_expr count e
  | Pexp_pack me -> visit_module_expr count me
  | Pexp_extension (_, payload) -> visit_payload count payload

and visit_module_expr count (me : Ppxlib.Parsetree.module_expr) =
  match me.Ppxlib.Parsetree.pmod_desc with
  | Pmod_structure items -> List.iter (visit_structure_item count) items
  | _ -> ()

and visit_class_field count (cf : Ppxlib.Parsetree.class_field) =
  match cf.Ppxlib.Parsetree.pcf_desc with
  | Pcf_method (_, _, Cfk_concrete (_, e)) -> visit_expr count e
  | Pcf_initializer e -> visit_expr count e
  | Pcf_val (_, _, Cfk_concrete (_, e)) -> visit_expr count e
  | _ -> ()

and visit_payload count (payload : Ppxlib.Parsetree.payload) =
  match payload with
  | PStr items -> List.iter (visit_structure_item count) items
  | PPat (pat, guard) ->
    (* pattern payloads cannot contain expressions other than the guard *)
    ignore pat;
    Option.iter (visit_expr count) guard
  | PTyp _ | PSig _ -> ()

and visit_structure_item count (item : Ppxlib.Parsetree.structure_item) =
  match item.Ppxlib.Parsetree.pstr_desc with
  | Pstr_eval (e, _) -> visit_expr count e
  | Pstr_value (_, bindings) ->
    List.iter (fun vb -> visit_expr count vb.Ppxlib.Parsetree.pvb_expr) bindings
  | _ -> ()
;;

let of_expression (e : Ppxlib.Parsetree.expression) =
  let count = ref 1 in
  visit_expr count e;
  !count
;;
