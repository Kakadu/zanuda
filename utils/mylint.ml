open Base
open Caml.Format

let () = Printexc.record_backtrace true

module Options = struct
  type t =
    { mutable outfile : string option
    ; mutable outgolint : string option
    ; mutable out_rdjsonl : string option
          (* Spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf#rdjson *)
    ; mutable infile : string
          (* Below options to manage file paths. Not sure are they really required *)
    ; mutable workspace : string option
    ; mutable prefix_to_cut : string option
    ; mutable prefix_to_add : string option
    }

  let opts =
    { outfile = None
    ; outgolint = None
    ; out_rdjsonl = None
    ; infile = ""
    ; workspace = None
    ; prefix_to_cut = None
    ; prefix_to_add = None
    }
  ;;

  let set_out_file s = opts.outfile <- Some s
  let set_out_golint s = opts.outgolint <- Some s
  let set_out_rdjsonl s = opts.out_rdjsonl <- Some s
  let set_workspace s = opts.workspace <- Some s
  let set_prefix_to_cut s = opts.prefix_to_cut <- Some s
  let set_prefix_to_add s = opts.prefix_to_add <- Some s
  let prefix_to_cut () = opts.prefix_to_cut
  let prefix_to_add () = opts.prefix_to_add
  let outfile () = opts.outfile
  let out_golint () = opts.outgolint
  let out_rdjsonl () = opts.out_rdjsonl
  let infile () = opts.infile
  let set_in_file s = opts.infile <- s
end

let recover_filepath s =
  let filepath = s in
  let filepath =
    match Options.prefix_to_cut () with
    | Some s -> String.drop_prefix filepath (String.length s)
    | None -> filepath
  in
  let filepath =
    match Options.prefix_to_add () with
    | Some s -> Format.sprintf "%s%s" s filepath
    | None -> filepath
  in
  filepath
;;

module Level = struct
  type t =
    | Allow
    | Warn
    | Deny
    | Deprecated
end

module Groups = struct
  type t = Style
  (* Correctness Perf Restriction Deprecated Pedantic Complexity Suspicious Cargo Nursery *)
end

module Lints = struct
  type t = string
  type single_printer = loc:Warnings.loc -> Format.formatter -> unit

  let found_Lints : (Location.t * (module LINT.REPORTER)) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc m = Queue.enqueue found_Lints (loc, m)

  let report () =
    let mdfile =
      match Options.outfile () with
      | Some s ->
        (* Format.printf "Opening file '%s'...\n%!" s; *)
        let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
        let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
        [ ( (fun (module M : LINT.REPORTER) ppf -> M.md ppf)
          , Format.formatter_of_out_channel ch
          , ch )
        ]
      | None -> []
    in
    let golint_files =
      match Options.out_golint () with
      | Some s ->
        let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
        (* By some reason on CI Open_creat is not enough to create a file *)
        let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
        [ ( (fun (module M : LINT.REPORTER) ppf -> M.golint ppf)
          , Format.formatter_of_out_channel ch
          , ch )
        ]
      | None -> []
    in
    let rdjsonl_files =
      match Options.out_rdjsonl () with
      | Some s ->
        let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
        (* By some reason on CI Open_creat is not enough to create a file *)
        let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
        [ ( (fun (module M : LINT.REPORTER) ppf -> M.rdjsonl ppf)
          , Format.formatter_of_out_channel ch
          , ch )
        ]
      | None -> []
    in
    let all_files = List.concat [ rdjsonl_files; golint_files; mdfile ] in
    Base.Exn.protect
      ~f:(fun () ->
        (* Format.printf "Total lints found: %d\n%!" (Queue.length found_Lints); *)
        Queue.iter found_Lints ~f:(fun (_loc, ((module M : LINT.REPORTER) as m)) ->
            M.txt Format.std_formatter ();
            List.iter all_files ~f:(fun (f, ppf, _) -> f m ppf ())
            (* let () =
              if not (List.is_empty mdfile)
              then
                List.iter
                  ~f:(fun (ppf, ch) ->
                    Format.fprintf ppf "%a%!" M.md ();
                    Caml.flush ch)
                  mdfile
            in
            if not (List.is_empty golint_files)
            then (
              let () = print_endline "printing in golint format" in
              List.iter golint_files ~f:(fun (ppf, _) ->
                  (* print_endline "Trying to print something as golint "; *)
                  Format.fprintf ppf "%a%!" M.golint ())) *)))
      ~finally:(fun () ->
        let f (_, ppf, ch) =
          Format.pp_print_flush ppf ();
          Caml.close_out ch
        in
        List.iter ~f all_files)
  ;;
end

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
    Format.fprintf ppf "%s\n" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

module Casing : LINT.S = struct
  let is_camel_case s = String.(lowercase s <> s)

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

  (* let report_rdjson _name ~loc:_ _ppf = () *)

  let report ~loc name =
    let module M = struct
      let md ppf () = report_md name ~loc ppf
      let txt ppf () = report_txt name ~loc ppf
      (* let rdjson ppf () = report_rdjson name ~loc ppf *)

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          name
      ;;

      let golint ppf () =
        ErrorFormat.pp
          ppf
          ~filename:(recover_filepath loc.loc_start.pos_fname)
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
          if is_camel_case tname
          then
            (* let () = Format.printf "type name %s is BAD\n%!" tname in *)
            Lints.add ~loc (report ~loc tname);
          fallback.type_declaration self tdecl)
    }
  ;;
end

module GuardInsteadOfIf : LINT.S = struct
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
          ~filename:(recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          pp_print_string
          msg
      ;;

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(recover_filepath loc.loc_start.pos_fname)
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
            Lints.add ~loc (report ~loc)
          | _ -> fallback.case self case)
    }
  ;;
end

module ParsetreeHasDocs : LINT.S = struct
  open Parsetree
  open Ast_iterator

  let ends_with ~suffix s = String.equal (String.suffix s (String.length suffix)) suffix
  let is_mli s = ends_with ~suffix:".mli" s
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
          ~filename:(recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          msg
          name
      ;;

      let rdjsonl ppf () =
        RDJsonl.pp
          ppf
          ~filename:(recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum
          msg
          name
      ;;
    end
    in
    (module M : LINT.REPORTER)
  ;;

  let stru { Compile_common.source_file; _ } fallback =
    if is_mli source_file
    then
      { fallback with
        type_kind =
          (fun self -> function
            | Ptype_variant cds ->
              List.iter cds ~f:(fun cd ->
                  let loc = cd.pcd_loc in
                  if not (List.exists cd.pcd_attributes ~f:is_doc_attribute)
                  then Lints.add ~loc (report cd.pcd_name.txt ~loc))
            | tk -> fallback.type_kind self tk)
      }
    else fallback
  ;;
end

let all_linters = [ GuardInsteadOfIf.stru; Casing.stru; ParsetreeHasDocs.stru ]

let build_iterator info ~f =
  let compose lint fallback = lint info fallback in
  let o =
    List.fold_left
      ~f:(fun acc lint -> compose lint acc)
      ~init:Ast_iterator.default_iterator
      all_linters
  in
  f o
;;

let on_structure = build_iterator ~f:(fun o -> o.Ast_iterator.structure o)
let on_signature = build_iterator ~f:(fun o -> o.Ast_iterator.signature o)

let load_file filename =
  let with_info f =
    Compile_common.with_info
      ~native:false
      ~source_file:filename
      ~tool_name:"asdf"
      ~output_prefix:"asdf"
      ~dump_ext:"asdf"
      f
  in
  let () =
    with_info (fun info ->
        (* Format.printf "%s %d    %s\n%!" __FILE__ __LINE__ info.source_file; *)
        if String.equal (String.suffix info.source_file 3) ".ml"
        then (
          (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
          let parsetree = Compile_common.parse_impl info in
          on_structure info parsetree)
        else if String.equal (String.suffix info.source_file 4) ".mli"
        then (
          let parsetree = Compile_common.parse_intf info in
          on_signature info parsetree)
        else Format.printf "%s %d\n%!" __FILE__ __LINE__)
  in
  (* Caml.print_endline @@ Pprintast.string_of_structure parsetree; *)
  Lints.report ();
  Lints.clear ();
  (* let tstr, _coe = with_info (fun info -> Compile_common.typecheck_impl info parsetree) in *)
  (* Format.printf "%a\n%!" Printtyped.implementation tstr; *)
  ()
;;

let () =
  Arg.parse
    [ "-o", Arg.String Options.set_out_file, "Set Markdown output file"
    ; "-ogolint", Arg.String Options.set_out_golint, "Set output file in golint format"
    ; ( "-ordjsonl"
      , Arg.String Options.set_out_rdjsonl
      , "Set output file in rdjjsonl format" )
    ; "-ws", Arg.String Options.set_workspace, "Set dune workspace root"
    ; ( "-del-prefix"
      , Arg.String Options.set_prefix_to_cut
      , "Set prefix to cut from file names" )
    ; ( "-add-prefix"
      , Arg.String Options.set_prefix_to_add
      , "Set prefix to reprend to file names" )
    ]
    Options.set_in_file
    "usage";
  Clflags.error_style := Some Misc.Error_style.Contextual;
  let filename = Caml.Sys.argv.(1) in
  load_file filename;
  Caml.exit 0
;;
