open Base
open Format
open Utils

type module_ =
  { name : string
  ; impl : string option
  ; intf : string option
  ; cmt : string option
  ; cmti : string option
  }
[@@deriving sexp]

type executables =
  { names : string list
  ; modules : module_ list
  ; requires : string list
  ; include_dirs : string list
  }
[@@deriving sexp]

module Library = struct
  type t =
    { name : string
    ; uid : string
    ; local : bool
    ; requires : string list
    ; source_dir : string
    ; modules : module_ list
    ; include_dirs : string list
    }
  [@@deriving sexp]
end

type t =
  | Executables of executables
  | Library of Library.t
[@@deriving sexp]

let fine_module { impl } =
  match impl with
  | Some s when String.is_suffix s ~suffix:".ml-gen" -> false
  | _ -> true
;;

let analyze_dir analyze_untyped analyze_cmt analyze_cmti path =
  Unix.chdir path;
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    Caml.close_in ch;
    s
  in
  let db = [%of_sexp: t list] s in
  (* List.iter db ~f:(fun x -> Format.printf "%a\n%!" Sexplib.Sexp.pp_hum (sexp_of_t x)); *)
  let get_library name =
    List.find_map db ~f:(function
        | Library l when String.equal name l.uid -> Some l
        | _ -> None)
  in
  let on_module _ m =
    (* we analyze syntax tree without expanding syntax extensions *)
    Option.iter m.impl ~f:analyze_untyped;
    Option.iter m.intf ~f:analyze_untyped;
    (* Now analyze Typedtree extracted from cmt[i] *)
    let on_cmti source_file (_cmi_info, cmt_info) =
      Option.iter cmt_info ~f:(fun cmt ->
          match cmt.Cmt_format.cmt_annots with
          | Cmt_format.Implementation stru -> analyze_cmt source_file stru
          | Cmt_format.Interface sign -> analyze_cmti source_file sign
          | Cmt_format.Packed _
          | Cmt_format.Partial_implementation _
          | Cmt_format.Partial_interface _ ->
            printf "%s %d\n%!" __FILE__ __LINE__;
            Caml.exit 1)
    in
    List.iter [ m.impl, m.cmt; m.intf, m.cmti ] ~f:(function
        | None, None -> ()
        | Some filename, None ->
          Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | None, Some filename ->
          Format.printf "Found ml[i] file '%s' without cmt[i] file\n" filename
        | Some source_filename, Some cmt_filename ->
          let build_dir = "_build/default/" in
          let wrap =
            if String.is_prefix ~prefix:build_dir cmt_filename
            then (fun f ->
              Unix.chdir build_dir;
              let infos =
                if Config.Options.verbose ()
                then printfn "Reading cmt[i] file '%s'" cmt_filename;
                Cmt_format.read
                  (String.drop_prefix cmt_filename (String.length build_dir))
              in
              f infos;
              Unix.chdir "../..")
            else
              fun f ->
              let cmt = Cmt_format.read cmt_filename in
              f cmt
          in
          wrap (on_cmti source_filename))
  in
  let loop_database () =
    List.iter db ~f:(function
        | Executables { modules; requires } ->
          let extra_paths =
            requires
            |> List.filter_map ~f:(fun uid -> get_library uid)
            |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
          in
          List.iter modules ~f:(fun m -> if fine_module m then on_module extra_paths m)
        | Library { Library.modules; requires } ->
          let extra_paths =
            requires
            |> List.filter_map ~f:(fun uid -> get_library uid)
            |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
          in
          List.iter modules ~f:(fun m -> if fine_module m then on_module extra_paths m))
  in
  loop_database ()
;;
