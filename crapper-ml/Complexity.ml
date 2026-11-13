(** Cyclomatic complexity of a function body.

    Decision points are [if], [while], [for], each [match] / [function] /
    [try] case, each [when] guard and the short-circuit operators [&&] / [||].
    A function without decisions has complexity 1. Decisions of local bindings
    stay charged to the enclosing function, matching the reference crapper's
    rule for nested functions.

    Counting cases can be switched off with [~count_cases:false] (the
    [--no-match-cases] command line switch): [match] then contributes only
    through its guards and its body. This deviates from McCabe's path
    counting and from every reference implementation; it makes dispatch-heavy
    OCaml code look simpler than it is.

    The traversal uses ppxlib's generated iterator (the analog of
    [Ast_iterator] from the compiler), so it follows the AST shape on its own. *)

let short_circuit (op : Ppxlib.Parsetree.expression) =
  match op.Ppxlib.Parsetree.pexp_desc with
  | Pexp_ident { txt = Lident ("&&" | "||"); _ } -> true
  | _ -> false
;;

let of_expression ~count_cases (e : Ppxlib.Parsetree.expression) =
  let count = ref 1 in
  let visitor =
    object
      inherit Ppxlib.Ast_traverse.iter as super

      (* every match / function / try case is a branch of its own;
         a [when] guard adds another decision *)
      method! case c =
        if count_cases then incr count;
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
