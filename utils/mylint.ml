open Base

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

open UntypedLints

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
        else Format.printf "%s %d\n%!" __FILE__ __LINE__)
  in
  (* Caml.print_endline @@ Pprintast.string_of_structure parsetree; *)
  CollectedLints.report ();
  CollectedLints.clear ();
  (* let tstr, _coe = with_info (fun info -> Compile_common.typecheck_impl info parsetree) in *)
  (* Format.printf "%a\n%!" Printtyped.implementation tstr; *)
  ()
;;

let () =
  let open Config in
  Arg.parse
    [ "-o", Arg.String Options.set_out_file, "Set Markdown output file"
    ; "-ogolint", Arg.String Options.set_out_golint, "Set output file in golint format"
    ; "-ordjsonl", Arg.String Options.set_out_rdjsonl, "Set output file in rdjsonl format"
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
