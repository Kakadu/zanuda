[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Dune_project

let on_module { impl; intf } =
  match impl, intf with
  | None, None -> failwith "Not implemented"
  | Some m, _ when String.(ends_with (lowercase_ascii m) ~suffix:"ast.ml") -> ()
  | None, Some _ | Some _, Some _ -> ()
  | Some ml, None ->
    let filename = Config.recover_filepath ml in
    if not (String.ends_with ~suffix:".ml-gen" filename)
    then
      CollectedLints.add
        ~loc:Location.none
        (module struct
          let msg ppf file =
            Format.fprintf ppf "File '%s' doesn't have corresponding .mli interface" file
          ;;

          let txt ppf () = Format.fprintf ppf "%a\n" msg ml

          let rdjsonl ppf () =
            let filename = Config.recover_filepath ml in
            Utils.RDJsonl.pp ppf ~filename ~line:1 msg filename
          ;;
        end)
;;

let on_library { Library.modules } = List.iter on_module modules

let on_executables { modules = _ } =
  (* List.iter on_module modules *)
  ()
;;

let check db =
  if Config.is_check_filesystem ()
  then
    List.iter
      (function
        | Build_context _ | Root _ -> ()
        | Executables es -> on_executables es
        | Library l -> on_library l)
      db
;;
