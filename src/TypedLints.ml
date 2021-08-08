open Base
open Location
open Utils

module L1 : LINT.TYPED = struct
  type input = Tast_iterator.iterator

  let lint_id = "list_length_comparisons"

  let describe_itself () =
    UntypedLints.describe_as_clippy_json
      lint_id
      ~docs:
        {|
### What it does
The function `Stdlib.List.length` evaluated length of standart OCaml linked lists (`'a list`). There return values supposed to be non-negative, so all code like `List.length .. <= 0` smells bad. If they need to check that list is empty it is more recommended to use pattern matching instead of calculating length, because for large list we will do full iteration, and it will not be too efficient.
  |}
  ;;

  let msg ppf () =
    Caml.Format.fprintf ppf "Bad measurement of a list (with non-negative size)%!"
  ;;

  let report filename ~loc =
    let module M = struct
      let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg ()

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          ()
      ;;
    end
    in
    (module M : LINT.REPORTER)
  ;;

  let run _ fallback =
    let pat =
      let open Tast_pattern in
      let ops = [ ">=", "<=", 0; "<=", ">=", 0; ">", "<", 0; "=", "=", 0 ] in
      let single (op, dualop, n) =
        let open Tast_pattern in
        let pat_list_length =
          texp_ident (path [ "Stdlib"; "List"; "length" ])
          ||| texp_ident (path [ "Stdlib!"; "List"; "length" ])
          ||| texp_ident (path [ "Base"; "List"; "length" ])
          ||| texp_ident (path [ "Base!"; "List"; "length" ])
        in
        (* TODO: understand difference between Stdlib and Stdlib! *)
        let make_pat_op op =
          texp_ident (path [ "Stdlib"; op ])
          ||| texp_ident (path [ "Stdlib!"; op ])
          ||| texp_ident (path [ "Base!"; op ])
          ||| texp_ident (path [ "Base"; op ])
        in
        let pat_op = make_pat_op op in
        let pat_op2 = make_pat_op dualop in
        texp_apply2 pat_op (texp_apply1 pat_list_length __) (eint @@ int n)
        ||| texp_apply2 pat_op2 (eint @@ int n) (texp_apply1 pat_list_length __)
      in
      List.fold
        ~init:(single @@ List.hd_exn ops)
        (List.tl_exn ops)
        ~f:(fun acc x -> acc ||| single x)
    in
    let open Tast_iterator in
    { fallback with
      expr =
        (fun self expr ->
          let open Typedtree in
          let loc = expr.exp_loc in
          (* if String.is_substring loc.loc_start.pos_fname ~substring:"exec"
          then
            Format.printf
              "%a\n%!"
              Pprintast.expression
              Untypeast.(default_mapper.expr default_mapper expr); *)
          Tast_pattern.parse
            pat
            loc
            ~on_error:(fun _desc -> ())
            expr
            (fun _list_to_be_measured ->
              CollectedLints.add
                ~loc
                (report loc.Location.loc_start.Lexing.pos_fname ~loc));
          fallback.expr self expr)
        (* ; structure =
        (fun self stru ->
          (*           if String.is_substring
               (List.hd_exn stru.str_items).str_loc.loc_start.pos_fname
               ~substring:"exec"
          then Printtyped.implementation Format.std_formatter stru; *)
          fallback.structure self stru) *)
    }
  ;;
end
