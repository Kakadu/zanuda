open Base
open Caml.Format
open Zanuda_core
open Utils

let is_camel_case s = String.(lowercase s <> s)
let lint_id = "var_should_not_be_used"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Checks that type names are using snake case (`very_useful_typ`) and not using camel case (`veryUsefulTyp`) popular in Python and Haskell.

### Why is this bad?
Wrong casing is not exactly bad but OCaml tradition says that types' and module types' names should be snake case. Modules names' in standard library are in camel case but in most Janestreet libraries (ppxlib, base) they are in snake case too.
  |}
;;

type input = Ast_iterator.iterator

open Ast_iterator

let msg ppf name = fprintf ppf "Type name `%s` should be in snake case" name

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

let run _ fallback =
  { fallback with
    type_declaration =
      (fun self tdecl ->
        let open Parsetree in
        let tname = tdecl.ptype_name.txt in
        let loc = tdecl.ptype_loc in
        if is_camel_case tname
        then (
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~loc ~filename tname));
        fallback.type_declaration self tdecl)
  ; structure =
      (fun self x ->
        let open Parsetree in
        let loop_vb wher vb =
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt } when String.is_prefix txt ~prefix:"_" -> ()
          | _ -> ()
        in
        let rec loop_str = function
          | [] -> ()
          | h :: tl ->
            let _ =
              match h.pstr_desc with
              | Pstr_value (_, vbs) -> List.iter ~f:(loop_vb ()) vbs
              | _ -> ()
            in
            loop_str tl
        in
        fallback.structure self x)
  }
;;
