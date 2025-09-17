[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Utils
open Dune_project

type w =
  | Wrapped of string
  | Non_wrapped

let pp_w ppf = function
  | Non_wrapped -> Format.fprintf ppf "Non_wrapped"
  | Wrapped s -> Format.fprintf ppf "Wrapped %S" s
;;

let fine_module { impl } =
  match impl with
  | Some s when String.ends_with s ~suffix:".ml-gen" -> false
  | _ -> true
;;

let to_module_name name =
  if Base.Char.is_uppercase name.[0]
  then name
  else String.mapi (fun i c -> if i = 0 then Base.Char.uppercase c else c) name
;;

let discover_wrappness modules =
  let module W = struct
    type w =
      | W of string * string
      | NW of string

    let pp_w ppf = function
      | NW s -> Format.fprintf ppf "NW %S" s
      | W (pref, suf) -> Format.fprintf ppf "W (%s __ %s)" pref suf
    ;;

    let is_NW = function
      | NW _ -> true
      | _ -> false
    ;;

    let is_W_with name = function
      | W (s, _) when String.equal s name -> true
      | _ -> false
    ;;
  end
  in
  let extract str =
    let pos_slash = String.rindex str '/' in
    let pos_dot = String.rindex str '.' in
    let len = pos_dot - pos_slash - 1 in
    assert (len > 0);
    let name = StringLabels.sub str ~pos:(1 + pos_slash) ~len in
    match Base.String.substr_index name ~pattern:"__" with
    | None -> [ W.NW name ]
    | Some i ->
      [ W.W
          (Base.String.prefix name i, Base.String.suffix name (String.length name - i - 2))
      ]
  in
  let mm = List.concat_map (fun m -> Option.fold m.cmt ~none:[] ~some:extract) modules in
  let nonw, wrapp = Base.List.partition_tf ~f:W.is_NW mm in
  if List.for_all W.is_NW mm
  then Some Non_wrapped
  else (
    match nonw with
    | [ W.NW libname ] when List.for_all (W.is_W_with libname) wrapp ->
      Some (Wrapped (to_module_name libname))
    | _ -> None)
;;

let pp_maybe_wrapped ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some x -> Format.fprintf ppf "Some %a" pp_w x
;;

(* TODO: move these tests to a separate library *)

let%expect_test _ =
  let ans =
    discover_wrappness
      [ Dune_project.module_ "a" ~cmt:"/a.cmt" ~cmti:"/a.cmti"
      ; Dune_project.module_ "b" ~cmt:"/b.cmt" ~cmti:"/b.cmti"
      ]
  in
  Format.printf "%a\n%!" pp_maybe_wrapped ans;
  [%expect {| Some Non_wrapped |}]
;;

let%expect_test _ =
  let ans =
    discover_wrappness
      [ Dune_project.module_ "a" ~cmt:"/libname__a.cmt" ~cmti:"/libname__a.cmti"
      ; Dune_project.module_ "b" ~cmt:"/libname__b.cmt" ~cmti:"/libname__b.cmti"
      ]
  in
  Format.printf "%a\n%!" pp_maybe_wrapped ans;
  [%expect {| None |}]
;;

let%expect_test _ =
  let ans =
    discover_wrappness
      [ Dune_project.module_ "libname" ~cmt:"/libname.cmt"
      ; Dune_project.module_ "a" ~cmt:"/libname__a.cmt" ~cmti:"/libname__a.cmti"
      ]
  in
  Format.printf "%a\n%!" pp_maybe_wrapped ans;
  [%expect {| Some Wrapped "Libname" |}]
;;

let analyze_dir ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path =
  Unix.chdir path;
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    close_in ch;
    s
  in
  let db = [%of_sexp: t Base.list] s in
  (* List.iter db ~f:(fun x -> Format.printf "%a\n%!" Sexplib.Sexp.pp_hum (sexp_of_t x)); *)
  Lint_filesystem.check db;
  let on_module (is_wrapped : w) m =
    (* printf "\t Working on module %S (wrapped = %b)\n%!" m.name is_wrapped; *)
    (* we analyze syntax tree without expanding syntax extensions *)
    let try_untyped filename =
      try analyze_untyped filename with
      | Syntaxerr.Error _e ->
        Format.eprintf "Syntaxerr.Error in analysis of '%s'. Skipped.\n%!" filename
    in
    Option.iter try_untyped m.impl;
    Option.iter try_untyped m.intf;
    (* Now analyze Typedtree extracted from cmt[i] *)
    let on_cmti source_file (_cmi_info, cmt_info) =
      cmt_info
      |> Option.iter (fun cmt ->
        Collected_lints.clear_tdecls ();
        match cmt.Cmt_format.cmt_annots with
        | Cmt_format.Implementation stru -> analyze_cmt is_wrapped source_file stru
        | Interface sign -> analyze_cmti is_wrapped source_file sign
        | Packed _ | Partial_implementation _ | Partial_interface _ ->
          printfn "%s %d" __FILE__ __LINE__;
          exit 1)
    in
    ListLabels.iter
      [ m.impl, m.cmt; m.intf, m.cmti ]
      ~f:(function
        | None, None ->
          (* TODO: I'm not 100% sure when it happens *)
          (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
          ()
        | Some filename, None ->
          Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | None, Some filename ->
          Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | Some source_filename, Some cmt_filename ->
          let build_dir = "_build/default/" in
          let wrap =
            (* Format.printf "checking for prefix %S in %s\n%!" build_dir cmt_filename; *)
            if String.starts_with ~prefix:build_dir cmt_filename
            then
              if Stdlib.Sys.file_exists cmt_filename
              then (fun f ->
                Unix.chdir build_dir;
                let infos =
                  if Config.verbose ()
                  then printfn "Reading cmt[i] file '%s'" cmt_filename;
                  Cmt_format.read
                    (Base.String.drop_prefix cmt_filename (String.length build_dir))
                in
                f infos;
                Unix.chdir "../..")
              else
                fun _ ->
                Format.eprintf
                  "File '%s' doesn't exist. Maybe some of source files are not compiled?\n\
                   %!"
                  cmt_filename
            else
              fun f ->
              printfn "Loading CMT %S" cmt_filename;
              let cmt = Cmt_format.read cmt_filename in
              f cmt
          in
          (* Format.printf "%s %d src=%S\n%!" __FILE__ __LINE__ source_filename; *)
          wrap (on_cmti source_filename))
  in
  let loop_database () =
    ListLabels.iter db ~f:(function
      | Build_context _ | Root _ -> ()
      | Executables { modules; requires = _ } ->
        ListLabels.iter modules ~f:(fun m ->
          (* Dune doesn't allow to specify 'wrapped' for executables *)
          if fine_module m then on_module Non_wrapped m)
      | Library { Library.modules; name; _ } ->
        let wrappedness = discover_wrappness modules in
        (match wrappedness with
         | None -> Stdlib.Printf.eprintf "Can't detect wrappedness for a library %S" name
         | Some wrappedness ->
           (* printfn "Discovered wrappedness: %a" pp_w wrappedness; *)
           ListLabels.iter modules ~f:(fun m ->
             (* Format.printf "Trying module %a...\n%!" Sexp.pp (Dune_project.sexp_of_module_ m); *)
             if fine_module m
             then on_module wrappedness m
             else if (* Usually this happend with 'fake' wrapped modules from dune *)
                     not (String.equal name (String.lowercase_ascii m.name))
             then if Config.verbose () then printfn "module %S is omitted" m.name)))
  in
  loop_database ()
;;
