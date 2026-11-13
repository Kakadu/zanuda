(** Cyclomatic complexity of a function body.

    Decision points are [if], [while], [for], each [match] / [function] /
    [try] case, each [when] guard and the short-circuit operators [&&] / [||].
    A function without decisions has complexity 1. Decisions of local bindings
    stay charged to the enclosing function, matching the reference crapper's
    rule for nested functions.

    The traversal uses ppxlib's generated iterator (the analog of
    [Ast_iterator] from the compiler), so it follows the AST shape on its own. *)

let short_circuit (op : Ppxlib.Parsetree.expression) =
  match op.Ppxlib.Parsetree.pexp_desc with
  | Pexp_ident { txt = Lident ("&&" | "||"); _ } -> true
  | _ -> false
;;

let of_expression (e : Ppxlib.Parsetree.expression) =
  let count = ref 1 in
  let visitor =
    object
      inherit Ppxlib.Ast_traverse.iter as super

      (* every match / function / try case is a branch of its own;
         a [when] guard adds another decision *)
      method! case c =
        incr count;
        (match c.Ppxlib.Parsetree.pc_guard with
         | Some _ -> incr count
         | None -> ());
        super#case c

      method! expression e =
        let decision =
          match e.Ppxlib.Parsetree.pexp_desc with
          | Pexp_ifthenelse _ | Pexp_while _ | Pexp_for _ -> true
          | Pexp_apply (op, _) -> short_circuit op
          | _ -> false
        in
        if decision then incr count;
        super#expression e
    end
  in
  visitor#expression e;
  !count
;;
