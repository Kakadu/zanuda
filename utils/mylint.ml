open Base

module Options = struct
  type t =
    { mutable outfile : string option
    ; mutable infile : string
    }

  let opts = { outfile = None; infile = "" }
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

  let found_Lints : (Location.t * t) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc s = Queue.enqueue found_Lints (loc, s)
  let addf ?(loc = Location.none) fmt = Caml.Format.kasprintf (add ~loc) fmt
  let snake_case ~loc name = addf ~loc "Type name `%s` should be in snake case" name

  let guard_insteadof_if ~loc =
    addf ~loc "Prefer guard instead of if-then-else in case construction"
  ;;

  let report () =
    let dest =
      match Options.outfile () with
      | Some s ->
        print_endline "opening out file";
        [ Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s ]
      | None -> []
    in
    Base.Exn.protect
      ~f:(fun () ->
        Queue.iter found_Lints ~f:(fun (loc, s) ->
            let r =
              let main = Location.mkloc (fun ppf -> Caml.Format.fprintf ppf "%s" s) loc in
              Location.{ sub = []; main; kind = Report_alert "asdf" }
            in
            Location.print_report Caml.Format.std_formatter r;
            List.iter
              ~f:(fun out ->
                Location.print_report (Format.formatter_of_out_channel out) r)
              dest))
      ~finally:(fun () -> List.iter ~f:Caml.close_out dest)
  ;;
end

module Casing : LINT.S = struct
  let is_camel_case s = String.(lowercase s <> s)

  open Ast_iterator

  let stru _ fallback =
    { fallback with
      type_declaration =
        (fun self tdecl ->
          let open Parsetree in
          let tname = tdecl.ptype_name.txt in
          if is_camel_case tname then Lints.snake_case ~loc:tdecl.ptype_loc tname;
          fallback.type_declaration self tdecl)
    }
  ;;
end

module GuardInsteadOfIf : LINT.S = struct
  open Parsetree
  open Ast_iterator

  let stru _ fallback =
    { fallback with
      case =
        (fun self case ->
          match case.pc_rhs.pexp_desc with
          | Pexp_ifthenelse (_, _, _) ->
            Lints.guard_insteadof_if ~loc:case.pc_rhs.pexp_loc
          | _ -> fallback.case self case)
    }
  ;;
end

module ParsetreeHasDocs : LINT.S = struct
  open Parsetree
  open Ast_iterator

  let ends_with ~suffix s = String.equal (String.suffix s (String.length suffix)) suffix
  let is_mli s = ends_with ~suffix:".mli" s

  let stru { Compile_common.source_file; _ } fallback =
    if is_mli source_file
    then
      { fallback with
        type_kind =
          (fun self -> function
            | Ptype_variant cds -> List.iter cds ~f:(fun _ -> ())
            (* match case.pc_rhs.pexp_desc with
            | Pexp_ifthenelse (_, _, _) ->
              Lints.guard_insteadof_if ~loc:case.pc_rhs.pexp_loc *)
            | tk -> fallback.type_kind self tk)
      }
    else fallback
  ;;
  (* TODO: ocamlc lib/ast.mli -stop-after typing -dsource -dparsetree   *)
end

let all_linters = [ GuardInsteadOfIf.stru; Casing.stru ]

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
