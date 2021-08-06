open Base
open Format

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
  | Some s when UntypedLints.ends_with s ~suffix:".ml-gen" -> false
  | _ -> true
;;

let analyze_dir analyze_untyped path =
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
    (* Format.printf "looking for library %s\n%!" name; *)
    List.find_map db ~f:(function
        | Library l when String.equal name l.uid -> Some l
        | _ -> None)
  in
  let on_module _ { impl } =
    Option.iter impl ~f:(fun impl ->
        printf "Parsetree of %s\n%!" impl;
        analyze_untyped impl)
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
