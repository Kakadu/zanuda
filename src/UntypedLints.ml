open Base
open Format
open Utils

let describe_as_clippy_json id ~docs : Yojson.Safe.t =
  `Assoc
    [ "id", `String id
    ; "group", `String "correctness"
    ; "level", `String "deny"
    ; "docs", `String docs
    ; ( "applicability"
      , `Assoc
          [ "is_multi_part_suggestion", `Bool false
          ; "applicability", `String "Unresolved"
          ] )
    ]
;;

module Casing : LINT.UNTYPED = struct
  let is_camel_case s = String.(lowercase s <> s)

  let describe_itself () =
    describe_as_clippy_json
      "camel_cased_types"
      ~docs:
        {|
### What it does
Checks that type names are using snake case (`very_useful_typ`) and not using camel case (`veryUsefulTyp) popular in Python and Haskell.

### Why is this bad?
Wrong casing is not exactly bad but OCaml tradition says that types' and module types' names should be snake case. Modules names' in standard library are in camel case but in most Janestreet libraries (ppxlib, base) they are in snake case too.
  |}
  ;;

  type input = Ast_iterator.iterator

  open Ast_iterator

  let msg ppf name = fprintf ppf "Type name `%s` should be in snake case" name

  let report_txt typ_name ~loc ~filename ppf =
    Location.input_name := filename;
    cut_build_dir ();
    let main = Location.mkloc (fun ppf -> msg ppf typ_name) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let report_md ~loc ~filename name ppf =
    fprintf ppf "* %a\n%!" msg name;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt ~filename name ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;;

  let report ~loc ~filename name =
    let module M = struct
      let md ppf () = report_md name ~filename ~loc ppf
      let txt ppf () = report_txt name ~filename ~loc ppf

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          name
      ;;

      let golint ppf () =
        ErrorFormat.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          msg
          name
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
            CollectedLints.add ~loc (report ~loc ~filename tname);
            fallback.type_declaration self tdecl))
    }
  ;;
end

module GuardInsteadOfIf : LINT.UNTYPED = struct
  let describe_itself () =
    describe_as_clippy_json
      "use_guard_instead_of_if"
      ~docs:
        {|
### What it does
Pattern matching guards are not very common in mainstream languages so it easy to forget about them for OCaml wannabies.
This lint looks for if-then-else expressions in right hand sides of pattern matching, and recommends to use pattern guards.

### Why is this bad?
Sometimes guards allow you to write less error-prone code. For example, you are matching three values and want to
. if 1st fits predicate then do something and return, check other components otherwise.
. if 2nd fits predicate then do something and return, check other components otherwise.
. if 3rd ..., do something else otherwise.

The implementation with if-then-else could be like this.
```ocaml
match ... with
| (a,b,c) ->
    if pred1 a then ...
    else if pred2 b then ...
    else if pred3 c then ...
    else ... something_else ...
| ...
```
In this case all three bindings are in scope in the right hand side of matching, you can by mistake use them for something. And you can't use wildcards because all three bindings are required in right hand side.

Let's rewrite it with guards:
```ocaml
match ... with
| (a,_,_) when pred1 a -> ...
| (_,b,_) when pred2 b -> ...
| (_,_,c) when pred3 c -> ...
| ...
```

In this variant you have less potential for copy-paste mistake
  |}
  ;;

  open Parsetree
  open Ast_iterator

  type input = Ast_iterator.iterator

  let msg = "Prefer guard instead of if-then-else in case construction"

  let report_txt ~loc ppf =
    cut_build_dir ();
    let main = Location.mkloc (fun ppf -> Caml.Format.fprintf ppf "%s" msg) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let report_md ~loc ppf =
    fprintf ppf "* %s\n%!" msg;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;;

  let report ~loc =
    let module M = struct
      let md ppf () = report_md ~loc ppf
      let txt ppf () = report_txt ~loc ppf

      let golint ppf () =
        ErrorFormat.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          pp_print_string
          msg
      ;;

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          pp_print_string
          msg
      ;;
    end
    in
    (module M : LINT.REPORTER)
  ;;

  let run _ fallback =
    { fallback with
      case =
        (fun self case ->
          match case.pc_rhs.pexp_desc with
          | Pexp_ifthenelse (_, _, _) ->
            let loc = case.pc_rhs.pexp_loc in
            CollectedLints.add ~loc (report ~loc)
          | _ -> fallback.case self case)
    }
  ;;
end

let ends_with ~suffix s = String.equal (String.suffix s (String.length suffix)) suffix

module ParsetreeHasDocs : LINT.UNTYPED = struct
  let describe_itself () =
    describe_as_clippy_json
      "no_docs_parsetree"
      ~docs:
        {|
### What it does
It checks that file `Parsetree.mli' has documentation comments for all constructors. Usually files like this are used to describe abstract syntax tree (AST) of a language. In this case it's recommended to annotate every constructor with a documentation about meaning of the constructors, for example, which real syntax if supposed to be parsed to this part of AST.

As example of this kind of documentation you can consult [OCaml 4.13 parse tree](https://github.com/ocaml/ocaml/blob/4.13/parsing/parsetree.mli#L282)
  |}
  ;;

  open Parsetree
  open Ast_iterator

  type input = Ast_iterator.iterator

  let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt
  let msg ppf name = fprintf ppf "Constructor '%s' has no documentation attribute" name

  let report_txt name ~loc ppf =
    cut_build_dir ();
    let r =
      let main = Location.mkloc (fun ppf -> msg ppf name) loc in
      Location.{ sub = []; main; kind = Report_alert "zanuda-linter" }
    in
    Location.print_report ppf r
  ;;

  let report_md name ~loc ppf =
    fprintf ppf "* %a\n%!" msg name;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt name ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;;

  let report name ~loc =
    let module M = struct
      let md ppf () = report_md name ~loc ppf
      let txt ppf () = report_txt name ~loc ppf

      let golint ppf () =
        ErrorFormat.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          msg
          name
      ;;

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          name
      ;;
    end
    in
    (module M : LINT.REPORTER)
  ;;

  let is_mli s = ends_with ~suffix:".mli" s

  let run { Compile_common.source_file; _ } fallback =
    if ends_with ~suffix:"arsetree.mli" source_file
    then
      { fallback with
        type_kind =
          (fun self -> function
            | Ptype_variant cds ->
              List.iter cds ~f:(fun cd ->
                  let loc = cd.pcd_loc in
                  if not (List.exists cd.pcd_attributes ~f:is_doc_attribute)
                  then CollectedLints.add ~loc (report cd.pcd_name.txt ~loc))
            | tk -> fallback.type_kind self tk)
      }
    else fallback
  ;;
end
