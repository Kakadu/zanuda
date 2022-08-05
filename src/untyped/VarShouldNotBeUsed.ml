open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "var_should_not_be_used"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Report identifier starting with '_' and used later

### Why is this bad?
OCaml compiler has a tendency to report warning 26 about unused variables. Usually this warning could be supressed by adding '_' in the beginning of identifier to make it look like wildcard variable. But if that identifier is used later it contradicts the purpose of adding undescore in the beginnning.
  |}
;;

type input = Ast_iterator.iterator

open Ast_iterator

let msg ppf name =
  fprintf ppf "Identifier `%s` used somewhere else but supposed to be unused." name
;;

let report ~loc ~filename typ_name =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg typ_name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        typ_name
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

exception Found

let occurs_check name =
  { Ast_iterator.default_iterator with
    expr =
      (fun self e ->
        let open Parsetree in
        match e.pexp_desc with
        | Pexp_ident { txt = Lident s } when String.equal s name -> raise Found
        | _ -> Ast_iterator.default_iterator.expr self e)
  }
;;

let run { Compile_common.source_file; _ } fallback =
  { fallback with
    structure =
      (fun self x ->
        let open Parsetree in
        let loop_vb wher vb =
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt; loc } when String.is_prefix txt ~prefix:"_" ->
            (try
               let it = occurs_check txt in
               it.expr it vb.pvb_expr;
               List.iter ~f:(it.structure_item it) wher
             with
             | Found -> CollectedLints.add ~loc (report ~loc ~filename:source_file txt))
          | _ ->
            (* TODO: support Ppat_as ... *)
            ()
        in
        let rec loop_str = function
          | [] -> ()
          | h :: tl ->
            let () =
              match h.pstr_desc with
              | Pstr_value (_, vbs) -> List.iter ~f:(loop_vb tl) vbs
              | _ -> ()
            in
            loop_str tl
        in
        loop_str x;
        fallback.structure self x)
  }
;;
