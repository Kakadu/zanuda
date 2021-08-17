open Base
open Caml.Format
open Zanuda_core
open Utils

let is_camel_case s = String.(lowercase s <> s)

let describe_itself () =
  describe_as_clippy_json
    "camel_cased_types"
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

(*
    Option.iter !Location.input_lexbuf ~f:Lexing.flush_input;
    Location.input_name := cut_build_dir filename;
    let loc =
      let open Location in
      { loc with
        loc_start = { loc.loc_start with pos_fname = !input_name }
      ; loc_end = { loc.loc_end with pos_fname = !input_name }
      }
    in
    (*     let () =
      let open Location in
      printfn
        "loc = { ghost=%b, start = { fname=%S, lnum=%d, cnum=%d }, end = { fname = %s, \
         lnum = %d, cnum=%d } }"
        loc.loc_ghost
        loc.loc_start.pos_fname
        loc.loc_start.pos_lnum
        loc.loc_start.pos_cnum
        loc.loc_end.pos_fname
        loc.loc_end.pos_lnum
        loc.loc_end.pos_cnum
    in *)
    if Config.Options.verbose ()
    then printf "Location.input_name = %s\n%!" !Location.input_name;
    let main = Location.mkloc (fun ppf -> msg ppf typ_name) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
    *)
(*
  let report_md ~loc ~filename name ppf =
    fprintf ppf "* %a\n%!" msg name;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt ~filename name ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;; *)

let report ~loc ~filename typ_name =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg typ_name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
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
  }
;;
