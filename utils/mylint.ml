open Base
open Caml.Format

module Options = struct
  type t =
    { mutable outfile : string option
    ; mutable outrdjson : string option
          (* Spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf#rdjson *)
    ; mutable infile : string
    }

  let opts = { outfile = None; outrdjson = None; infile = "" }
  let set_out_file s = opts.outfile <- Some s
  let outfile () = opts.outfile
  let infile () = opts.infile
  let set_in_file s = opts.infile <- s
end

module Level = struct
  type t =
    | Allow
    | Warn
    | Deny
    | Deprecated
end

module Groups = struct
  type t = Style
  (* Correctness Perf Restriction  Deprecated Pedantic Complexity Suspicious Cargo Nursery *)
end

module Lints = struct
  type t = string
  type single_printer = loc:Warnings.loc -> Format.formatter -> unit

  let found_Lints : (Location.t * (single_printer * single_printer)) Queue.t =
    Queue.create ()
  ;;

  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc (txt, md) = Queue.enqueue found_Lints (loc, (txt, md))

  let report () =
    let dest =
      match Options.outfile () with
      | Some s -> [ Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s ]
      | None -> []
    in
    Base.Exn.protect
      ~f:(fun () ->
        Queue.iter found_Lints ~f:(fun (loc, (ftxt, fmd)) ->
            ftxt ~loc Format.std_formatter;
            if not (List.is_empty dest)
            then
              List.iter
                ~f:(fun out -> fmd ~loc (Format.formatter_of_out_channel out))
                dest))
      ~finally:(fun () -> List.iter ~f:Caml.close_out dest)
  ;;
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

  let stru _ fallback =
    { fallback with
      type_declaration =
        (fun self tdecl ->
          let open Parsetree in
          let tname = tdecl.ptype_name.txt in
          let loc = tdecl.ptype_loc in
          if is_camel_case tname then Lints.add ~loc (report_txt tname, report_md tname);
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

  let stru _ fallback =
    { fallback with
      case =
        (fun self case ->
          match case.pc_rhs.pexp_desc with
          | Pexp_ifthenelse (_, _, _) ->
            let loc = case.pc_rhs.pexp_loc in
            Lints.add ~loc (report_txt, report_md)
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
  let msg = "Constructor has no documentation attribute"

  let report_txt ~loc ppf =
    let r =
      let main = Location.mkloc (fun ppf -> Caml.Format.fprintf ppf "%s" msg) loc in
      Location.{ sub = []; main; kind = Report_alert "zanuda-linter" }
    in
    Location.print_report ppf r
  ;;

  let report_md ~loc ppf =
    fprintf ppf "* %s\n%!" msg;
    fprintf ppf "  ```\n%!";
    fprintf ppf "  @[%a@]%!" (fun ppf () -> report_txt ~loc ppf) ();
    fprintf ppf "  ```\n%!"
  ;;

  let stru { Compile_common.source_file; _ } fallback =
    if is_mli source_file
    then
      { fallback with
        type_kind =
          (fun self -> function
            | Ptype_variant cds ->
              List.iter cds ~f:(fun cd ->
                  if not (List.exists cd.pcd_attributes ~f:is_doc_attribute)
                  then Lints.add ~loc:cd.pcd_loc (report_txt, report_md))
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
        if String.equal (String.suffix info.source_file 3) ".ml"
        then (
          let parsetree = Compile_common.parse_impl info in
          on_structure info parsetree)
        else if String.equal (String.suffix info.source_file 4) ".mli"
        then (
          let parsetree = Compile_common.parse_intf info in
          on_signature info parsetree)
        else ())
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
    [ "-o", Arg.String Options.set_out_file, "Set Markdown output file" ]
    Options.set_in_file
    "usage";
  Clflags.error_style := Some Misc.Error_style.Contextual;
  let filename = Caml.Sys.argv.(1) in
  load_file filename;
  Caml.exit 0
;;
