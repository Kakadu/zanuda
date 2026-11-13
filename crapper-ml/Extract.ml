(** Extraction of functions from the parsetree of a source file.

    Scored units are top-level function bindings and class methods; local
    bindings stay inside their enclosing function. The namespace is the dune
    module path ([Some_lib.Module] for wrapped libraries), so nested modules
    extend it.

    [.cmt] files are used only as an index: they tell which modules were
    compiled, where their sources live and how dune wrapped them. The source
    is parsed into the compiler parsetree and immediately translated into the
    parsetree of the current version of ppxlib; the analysis always runs on
    the ppxlib parsetree to simplify future OCaml migrations. *)

(* [foo__Bar.cmt] compiles module [Foo.Bar]; plain names are the module of
   their own source file. *)
let namespace_root_of_cmt cmt_path =
  let name = Filename.remove_extension (Filename.basename cmt_path) in
  let capitalize name =
    String.mapi
      (fun i c ->
        if i = 0 && c >= 'a' && c <= 'z' then Char.chr (Char.code c - 32) else c)
      name
  in
  let rec index_of s sub =
    let ls = String.length s
    and lt = String.length sub in
    let rec go i =
      if i + lt > ls then None else if String.sub s i lt = sub then Some i else go (i + 1)
    in
    go 0
  in
  match index_of name "__" with
  | Some i ->
    capitalize (String.sub name 0 i)
    ^ "."
    ^ String.sub name (i + 2) (String.length name - i - 2)
  | None -> capitalize name
;;

let source_path_of_cmt cmt_path (infos : Cmt_format.cmt_infos) =
  match infos.Cmt_format.cmt_sourcefile with
  | Some source -> source
  | None -> Filename.remove_extension cmt_path
;;

(* dune compiles with the build tree as working directory, so the source path
   recorded in the [.cmt] file may be relative to it *)
let resolve_source cmt_path (infos : Cmt_format.cmt_infos) =
  let source = source_path_of_cmt cmt_path infos in
  if Sys.file_exists source
  then source
  else (
    let candidates =
      [ Filename.concat infos.Cmt_format.cmt_builddir source
      ; Filename.concat (Filename.dirname cmt_path) (Filename.basename source)
      ]
    in
    match List.find_opt Sys.file_exists candidates with
    | Some resolved -> resolved
    | None -> source)
;;

(* The pattern of a [let] binding names the function: [f] in [let f x = ...],
   [(f, _) = ...] and so on. *)
let rec pat_name (pat : Ppxlib.Parsetree.pattern) =
  match pat.Ppxlib.Parsetree.ppat_desc with
  | Ppat_var { txt = name; _ } -> Some name
  | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> pat_name inner
  | Ppat_tuple pats -> List.find_map pat_name pats
  | _ -> None
;;

let span_of_location (loc : Ppxlib.Location.t) =
  ( loc.Ppxlib.Location.loc_start.Lexing.pos_lnum
  , loc.Ppxlib.Location.loc_end.Lexing.pos_lnum
  , loc.Ppxlib.Location.loc_start.Lexing.pos_cnum
  , loc.Ppxlib.Location.loc_end.Lexing.pos_cnum )
;;

let make_function ~name ~namespace ~loc ~complexity =
  let start_line, end_line, start_byte, end_byte = span_of_location loc in
  { Model.name
  ; namespace
  ; complexity
  ; start_line
  ; end_line
  ; start_byte
  ; end_byte
  ; path = ""
  ; language = "ocaml"
  }
;;

let rec add_structure namespace (str : Ppxlib.Parsetree.structure) acc =
  List.fold_left (fun acc item -> add_structure_item namespace item acc) acc str

and add_structure_item namespace (item : Ppxlib.Parsetree.structure_item) acc =
  match item.Ppxlib.Parsetree.pstr_desc with
  | Pstr_value (_, bindings) ->
    List.fold_left (fun acc vb -> add_value_binding namespace vb acc) acc bindings
  | Pstr_module mb ->
    (match mb.Ppxlib.Parsetree.pmb_name.Location.txt with
     | Some name when name <> "_" ->
       add_module_expr (namespace ^ "." ^ name) mb.Ppxlib.Parsetree.pmb_expr acc
     | _ -> acc)
  | Pstr_recmodule mbs ->
    List.fold_left
      (fun acc mb ->
        match mb.Ppxlib.Parsetree.pmb_name.Location.txt with
        | Some name when name <> "_" ->
          add_module_expr (namespace ^ "." ^ name) mb.Ppxlib.Parsetree.pmb_expr acc
        | _ -> acc)
      acc
      mbs
  | Pstr_class classes ->
    List.fold_left (fun acc cd -> add_class namespace cd acc) acc classes
  | _ -> acc

and add_module_expr namespace (me : Ppxlib.Parsetree.module_expr) acc =
  match me.Ppxlib.Parsetree.pmod_desc with
  | Pmod_structure str -> add_structure namespace str acc
  | Pmod_functor (_, body) -> add_module_expr namespace body acc
  | _ -> acc

and add_value_binding namespace vb acc =
  let expr = vb.Ppxlib.Parsetree.pvb_expr in
  match expr.Ppxlib.Parsetree.pexp_desc with
  | Pexp_function _ | Pexp_fun _ ->
    (match pat_name vb.Ppxlib.Parsetree.pvb_pat with
     | Some name ->
       make_function
         ~name
         ~namespace
         ~loc:vb.Ppxlib.Parsetree.pvb_loc
         ~complexity:(Complexity.of_expression expr)
       :: acc
     | None -> acc)
  | _ -> acc

and add_class namespace cd acc =
  let namespace = namespace ^ "." ^ cd.Ppxlib.Parsetree.pci_name.Location.txt in
  add_class_expr namespace cd.Ppxlib.Parsetree.pci_expr acc

and add_class_expr namespace (ce : Ppxlib.Parsetree.class_expr) acc =
  match ce.Ppxlib.Parsetree.pcl_desc with
  | Pcl_structure cs ->
    List.fold_left
      (fun acc cf -> add_class_field namespace cf acc)
      acc
      cs.Ppxlib.Parsetree.pcstr_fields
  | Pcl_fun (_, _, _, body)
  | Pcl_apply (body, _)
  | Pcl_let (_, _, body)
  | Pcl_open (_, body)
  | Pcl_constraint (body, _) -> add_class_expr namespace body acc
  | _ -> acc

and add_class_field namespace (cf : Ppxlib.Parsetree.class_field) acc =
  match cf.Ppxlib.Parsetree.pcf_desc with
  | Pcf_method (name_loc, _, Cfk_concrete (_, expr)) ->
    make_function
      ~name:name_loc.Location.txt
      ~namespace
      ~loc:cf.Ppxlib.Parsetree.pcf_loc
      ~complexity:(Complexity.of_expression expr)
    :: acc
  | _ -> acc
;;

let functions_of_structure namespace (str : Ppxlib.Parsetree.structure) source =
  let functions = add_structure namespace str [] in
  List.rev_map (fun (f : Model.function_) -> { f with Model.path = source }) functions
;;

let warn_broken_cmt cmt_path exn =
  Printf.eprintf
    "crapper: cannot analyze '%s' (%s). Skipped.\n%!"
    cmt_path
    (Printexc.to_string exn)
;;

let functions_of_cmt cmt_path =
  let infos = Cmt_format.read_cmt cmt_path in
  let namespace = namespace_root_of_cmt cmt_path in
  let source = resolve_source cmt_path infos in
  let str = Pparse.parse_implementation ~tool_name:"crapper" source in
  (* Translate the parsetree of the current compiler into the parsetree of
     the current version of ppxlib *)
  let str = Ppxlib.Selected_ast.of_ocaml Ppxlib.Selected_ast.Type.Structure str in
  functions_of_structure namespace str source
;;
