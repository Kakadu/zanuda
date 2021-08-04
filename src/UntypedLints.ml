open Base
open Format

module ErrorFormat = struct
  let pp ppf ~filename ~line ~col:_ msg x =
    Format.fprintf ppf "%s:%d:%d:%a\n%!" filename line (* col *) 0 msg x
  ;;
end

module RDJsonl = struct
  let pp ppf ~filename ~line ?code msg x =
    let location file ~line ~col =
      `Assoc
        [ "path", `String file
        ; "range", `Assoc [ "start", `Assoc [ "line", `Int line; "column", `Int col ] ]
        ]
    in
    let j =
      `Assoc
        ([ "message", `String (asprintf "%a" msg x)
         ; "location", location filename ~line ~col:1
         ; "severity", `String "INFO"
         ]
        @
        match code with
        | None -> []
        | Some (desc, url) ->
          [ "code", `Assoc [ "value", `String desc; "url", `String url ] ])
    in
    Format.fprintf ppf "%s\n%!" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

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

module Casing : LINT.S = struct
  let is_camel_case s = String.(lowercase s <> s)

  let describe_itself () =
    describe_as_clippy_json
      "camel_cased_types"
      ~docs:
        {|
### What it does
Checks for comparisons where one side of the relation is either the minimum or maximum value for its type and warns if it involves a case that is always true or always false. Only integer and boolean types are checked.

### Why is this bad?
An expression like min <= x may misleadingly imply that it is possible for x to be less than the minimum. Expressions like max < x are probably mistakes.

### Known problems
For usize the size of the current compile target will be assumed (e.g., 64 bits on 64 bit systems). This means code that uses such a comparison to detect target pointer width will trigger this lint. One can use mem::sizeof and compare its value or conditional compilation attributes like #[cfg(target_pointer_width = "64")] .. instead.

### Example
```
let vec: Vec<isize> = Vec::new();
if vec.len() <= 0 {}
if 100 > i32::MAX {}
```
  |}
  ;;

  open Ast_iterator

  let msg ppf name = fprintf ppf "Type name `%s` should be in snake case" name

  let report_txt name ~loc ppf =
    let main = Location.mkloc (fun ppf -> msg ppf name) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let report_md name ~loc ppf =
    fprintf ppf "* %a\n%!" msg name;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt name ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;;

  let report ~loc name =
    let module M = struct
      let md ppf () = report_md name ~loc ppf
      let txt ppf () = report_txt name ~loc ppf
      (* let rdjson ppf () = report_rdjson name ~loc ppf *)

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

  let stru _ fallback =
    { fallback with
      type_declaration =
        (fun self tdecl ->
          let open Parsetree in
          let tname = tdecl.ptype_name.txt in
          let loc = tdecl.ptype_loc in
          if is_camel_case tname then CollectedLints.add ~loc (report ~loc tname);
          fallback.type_declaration self tdecl)
    }
  ;;
end

module GuardInsteadOfIf : LINT.S = struct
  let describe_itself () =
    describe_as_clippy_json
      "use_guard_instead_of_if"
      ~docs:
        {|
### What it does
Checks for comparisons where one side of the relation is either the minimum or maximum value for its type and warns if it involves a case that is always true or always false. Only integer and boolean types are checked.

### Why is this bad?
An expression like min <= x may misleadingly imply that it is possible for x to be less than the minimum. Expressions like max < x are probably mistakes.

### Known problems
For usize the size of the current compile target will be assumed (e.g., 64 bits on 64 bit systems). This means code that uses such a comparison to detect target pointer width will trigger this lint. One can use mem::sizeof and compare its value or conditional compilation attributes like #[cfg(target_pointer_width = "64")] .. instead.

### Example
```
let vec: Vec<isize> = Vec::new();
if vec.len() <= 0 {}
if 100 > i32::MAX {}
```
  |}
  ;;

  open Parsetree
  open Ast_iterator

  let msg = "Prefer guard instead of if-then-else in case construction"

  let report_txt ~loc ppf =
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

  let stru _ fallback =
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

module ParsetreeHasDocs : LINT.S = struct
  let describe_itself () =
    describe_as_clippy_json
      "no_docs_parsetree"
      ~docs:
        {|
### What it does
Checks for comparisons where one side of the relation is either the minimum or maximum value for its type and warns if it involves a case that is always true or always false. Only integer and boolean types are checked.

### Why is this bad?
An expression like min <= x may misleadingly imply that it is possible for x to be less than the minimum. Expressions like max < x are probably mistakes.

### Known problems
For usize the size of the current compile target will be assumed (e.g., 64 bits on 64 bit systems). This means code that uses such a comparison to detect target pointer width will trigger this lint. One can use mem::sizeof and compare its value or conditional compilation attributes like #[cfg(target_pointer_width = "64")] .. instead.

### Example
```
let vec: Vec<isize> = Vec::new();
if vec.len() <= 0 {}
if 100 > i32::MAX {}
```
  |}
  ;;

  open Parsetree
  open Ast_iterator

  let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt
  let msg ppf name = fprintf ppf "Constructor '%s' has no documentation attribute" name

  let report_txt name ~loc ppf =
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

  let stru { Compile_common.source_file; _ } fallback =
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
