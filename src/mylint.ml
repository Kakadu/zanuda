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

let untyped_linters =
  let open UntypedLints in
  [ (module GuardInsteadOfIf : LINT.UNTYPED)
  ; (module Casing : LINT.UNTYPED)
  ; (module ParsetreeHasDocs : LINT.UNTYPED)
  ]
;;

let typed_linters =
  let open TypedLints in
  [ (module L1 : LINT.TYPED) ]
;;

(* TODO: Functions below are a little bit copy-pasty. Rework them *)
let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let untyped_on_structure info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.structure o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let untyped_on_signature info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.signature o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let typed_on_structure info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let typed_on_signature info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let load_file filename =
  Clflags.error_style := Some Misc.Error_style.Contextual;
  let with_info f =
    Compile_common.with_info
      ~native:false
      ~source_file:filename
      ~tool_name:"asdf" (* TODO: pass right tool name *)
      ~output_prefix:"asdf"
      ~dump_ext:"asdf"
      f
  in
  let () =
    let process_structure info =
      let parsetree = Compile_common.parse_impl info in
      untyped_on_structure info parsetree;
      let typedtree, _ = Compile_common.typecheck_impl info parsetree in
      typed_on_structure info typedtree;
      ()
    in
    let process_signature info =
      let parsetree = Compile_common.parse_intf info in
      untyped_on_signature info parsetree;
      let typedtree = Compile_common.typecheck_intf info parsetree in
      typed_on_signature info typedtree
    in
    with_info (fun info ->
        if UntypedLints.ends_with info.source_file ~suffix:".ml"
        then process_structure info
        else if UntypedLints.ends_with info.source_file ~suffix:".mli"
        then process_signature info
        else (
          let () =
            Format.eprintf
              "Don't know to do with file '%s'\n%s %d\n%!"
              info.source_file
              __FILE__
              __LINE__
          in
          Caml.exit 1))
  in
  CollectedLints.report ()
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
    ; ( "-dump-lints"
      , Arg.String Options.set_dump_file
      , "Dump information about available linters to JSON" )
    ]
    Options.set_in_file
    "usage";
  let () =
    match Options.dump_file () with
    | None -> ()
    | Some filename ->
      let info =
        List.concat
          [ List.map untyped_linters ~f:(fun (module L : LINT.UNTYPED) ->
                L.describe_itself ())
          ; List.map typed_linters ~f:(fun (module L : LINT.TYPED) ->
                L.describe_itself ())
          ]
      in
      let ch = Caml.open_out filename in
      Exn.protect
        ~f:(fun () -> Yojson.Safe.pretty_to_channel ~std:true ch (`List info))
        ~finally:(fun () -> Caml.close_out ch);
      Caml.exit 0
  in
  load_file (Options.infile ())
;;