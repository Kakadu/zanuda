(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: It would be great not to depend on Base, but currently ppx_expect requires it *)
open Base
open Utils
open Dune_project

type w =
  | Wrapped of string
  | Non_wrapped

let fine_module { impl } =
  match impl with
  | Some s when String.is_suffix s ~suffix:".ml-gen" -> false
  | _ -> true
;;

let analyze_dir ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path =
  Unix.chdir path;
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    Caml.close_in ch;
    s
  in
  let db = [%of_sexp: t list] s in
  (* List.iter db ~f:(fun x -> Format.printf "%a\n%!" Sexplib.Sexp.pp_hum (sexp_of_t x)); *)
  Lint_filesystem.check db;
  (* let get_library name =
     List.find_map db ~f:(function
     | Library l when String.equal name l.uid -> Some l
     | _ -> None)
     in *)
  let on_module (is_wrapped : w) m =
    (* printf "\t Working on module %S (wrapped = %b)\n%!" m.name is_wrapped; *)
    (* we analyze syntax tree without expanding syntax extensions *)
    let try_untyped filename =
      try analyze_untyped filename with
      | Syntaxerr.Error _e ->
        Format.eprintf "Syntaxerr.Error in analysis of '%s'. Skipped.\n%!" filename
    in
    Option.iter m.impl ~f:try_untyped;
    Option.iter m.intf ~f:try_untyped;
    (* Now analyze Typedtree extracted from cmt[i] *)
    let on_cmti source_file (_cmi_info, cmt_info) =
      Option.iter cmt_info ~f:(fun cmt ->
        match cmt.Cmt_format.cmt_annots with
        | Cmt_format.Implementation stru -> analyze_cmt is_wrapped source_file stru
        | Cmt_format.Interface sign -> analyze_cmti is_wrapped source_file sign
        | Cmt_format.Packed _
        | Cmt_format.Partial_implementation _
        | Cmt_format.Partial_interface _ ->
          printfn "%s %d" Caml.__FILE__ Caml.__LINE__;
          Caml.exit 1)
    in
    List.iter
      [ m.impl, m.cmt; m.intf, m.cmti ]
      ~f:(function
        | None, None ->
          (* TODO: I'm not 100% sure when it happens *)
          (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
          ()
        | Some filename, None ->
          Caml.Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | None, Some filename ->
          Caml.Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | Some source_filename, Some cmt_filename ->
          let build_dir = "_build/default/" in
          let wrap =
            (* Format.printf "checking for prefix %S in %s\n%!" build_dir cmt_filename; *)
            if String.is_prefix ~prefix:build_dir cmt_filename
            then
              if Caml.Sys.file_exists cmt_filename
              then (fun f ->
                Unix.chdir build_dir;
                let infos =
                  if Config.verbose ()
                  then printfn "Reading cmt[i] file '%s'" cmt_filename;
                  Cmt_format.read
                    (String.drop_prefix cmt_filename (String.length build_dir))
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
    List.iter db ~f:(function
      | Build_context _ | Root _ -> ()
      | Executables { modules; requires = _ } ->
        (* let extra_paths =
          requires
          |> List.filter_map ~f:(fun uid -> get_library uid)
          |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
        in *)
        List.iter modules ~f:(fun m ->
          (* TODO: Why non wrapped? *)
          if fine_module m then on_module Non_wrapped m)
      | Library { Library.modules; name; _ } ->
        List.iter modules ~f:(fun m ->
          let is_wrapped =
            match m.cmti with
            | Some cmti ->
              Base.String.is_suffix
                cmti
                ~suffix:(Printf.sprintf "/%s__%s.cmti" name m.name)
            | None ->
              (match m.cmt with
               | Some cmt ->
                 Base.String.is_suffix
                   cmt
                   ~suffix:(Printf.sprintf "/%s__%s.cmt" name m.name)
               | None -> false)
          in
          let is_wrapped =
            if is_wrapped
            then
              Wrapped
                (if Char.is_uppercase name.[0]
                 then name
                 else
                   String.mapi name ~f:(fun i c -> if i = 0 then Char.uppercase c else c))
            else Non_wrapped
          in
          (* Format.printf "Trying module %a...\n%!" Sexp.pp (Dune_project.sexp_of_module_ m); *)
          if fine_module m
          then on_module is_wrapped m
          else if (* Usually this happend with 'fake' warpped modules from dune *)
                  not (String.equal name (String.lowercase m.name))
          then printfn "module %S is omitted" m.name))
  in
  loop_database ()
;;
