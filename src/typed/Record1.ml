open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "record_1"
let group = LINT.Suspicious
let level = LINT.Allow
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~group
    ~level
    ~docs:
      {|
### What it does
Simplyfies record construction using OCaml-specfic field punning.

(Chapter)[https://dev.realworldocaml.org/records.html] in 'Real World OCaml'.

##### Examples

```ocaml
{ x = r.x; y = r.y; z = 15}
```
vs
```ocaml
{ r with z = 15 }
```
|}
;;

let msg ppf expr =
  Caml.Format.fprintf ppf "Rewrite record as '%a'%!" Pprintast.expression expr
;;

let report filename ~loc expr =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg expr

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        expr
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

exception NotApplicable

let pp_texpr ppf expr =
  let u = Untypeast.(default_mapper.expr default_mapper expr) in
  Pprintast.expression ppf u
;;

module State = struct
  type t =
    { st_kept : Types.label_description list
    ; st_over_self : (string * Path.t) list (* label_name * record_ident *)
    ; st_over_other : (string * Typedtree.expression) list
    }

  let empty = { st_kept = []; st_over_self = []; st_over_other = [] }
  let add_kept s ld = { s with st_kept = ld :: s.st_kept }
  let add_over_self s x y = { s with st_over_self = (x, y) :: s.st_over_self }
  let add_over_other s x y = { s with st_over_other = (x, y) :: s.st_over_other }

  let get_result { st_over_self; st_over_other } =
    let sorted =
      List.sort st_over_self ~compare:(fun (_, path0) (_, path1) ->
          Path.compare path0 path1)
      |> List.group ~break:(fun (_, p1) (_, p2) -> not (Path.same p1 p2))
    in
    let max_seq =
      List.max_elt sorted ~compare:(fun xs ys ->
          compare (List.length xs) (List.length ys))
    in
    match max_seq with
    | None -> None
    | Some xs ->
      assert (List.length xs <> 0);
      let record_path = List.hd_exn xs |> snd in
      (* all fields that are kept should be kept *)
      let loc = Location.none in
      let mk_loc txt = { Location.txt; loc } in
      let make_lident s = mk_loc (Longident.Lident s) in
      let fields =
        List.concat
          [ List.filter_map st_over_self ~f:(function fname, ident_path ->
                if Path.same ident_path record_path
                then None
                else
                  Some
                    ( mk_loc (Longident.Lident fname)
                    , Ast_helper.Exp.(
                        field
                          (ident (mk_loc @@ Untypeast.lident_of_path ident_path))
                          (make_lident fname)) ))
          ; List.map st_over_other ~f:(function fname, expr ->
                make_lident fname, Untypeast.untype_expression expr)
          ]
      in
      let record_id_untyped =
        Ast_helper.Exp.ident (mk_loc (Untypeast.lident_of_path record_path))
      in
      Option.some
        (match fields with
        | [] -> record_id_untyped
        | _ -> Ast_helper.Exp.(record ~loc fields (Some record_id_untyped)))
  ;;

  let pp ppf { st_kept; st_over_self; st_over_other } =
    let open Format in
    fprintf
      ppf
      "{ kept=[%a]; over_self=[%a]; over_other=[%a] }%!"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf " ")
         (fun ppf ld -> fprintf ppf "%s" ld.Types.lbl_name))
      st_kept
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf " ")
         (fun ppf (s, path) -> fprintf ppf "(%s,%a)" s Path.print path))
      st_over_self
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf " ")
         (fun ppf (s, e) -> fprintf ppf "(%s,'%a')" s pp_texpr e))
      st_over_other
  ;;
end

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (* if String.is_substring loc.loc_start.pos_fname ~substring:"Record1"
        then (
          let u = Untypeast.(default_mapper.expr default_mapper expr) in
          Format.printf "%a\n%a\n%!" Pprintast.expression u (Printast.expression 0) u); *)
        Tast_pattern.(parse (texp_record none __))
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun arr () ->
            try
              let ans =
                Array.fold arr ~init:State.empty ~f:(fun acc (lab_desc, lab_def) ->
                    Tast_pattern.(
                      parse
                        (__ ** rld_kept
                        |> map1 ~f:(State.add_kept acc)
                        ||| (label_desc __
                             ** rld_overriden
                                  (lident __)
                                  (as__ (texp_field (texp_ident __) __))
                            |> map5 ~f:(fun _ field_lhs expr_rhs stru field_rhs ->
                                   if String.equal field_lhs field_rhs.Types.lbl_name
                                   then State.add_over_self acc field_rhs.lbl_name stru
                                   else State.add_over_other acc field_lhs expr_rhs))
                        ||| (label_desc __ ** rld_overriden (lident __) __
                            |> map3 ~f:(fun _ (field_lhs : string) field_rhs ->
                                   State.add_over_other acc field_lhs field_rhs))))
                      loc
                      ~on_error:(fun _ () -> acc)
                      (lab_desc, lab_def)
                      (fun st () -> st)
                      ())
              in
              (* Format.printf "State: %a\n%!" State.pp ans; *)
              match State.get_result ans with
              | None -> ()
              | Some expr ->
                CollectedLints.add
                  ~loc
                  (report loc.Location.loc_start.Lexing.pos_fname ~loc expr)
            with
            | NotApplicable -> ())
          ();
        fallback.expr self expr)
  }
;;
