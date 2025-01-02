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
      Collected_lints.add
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

let docs =
  {|
### What it does
Checks that dune project tree is well formed

### Why is is important?

All modules should have .mli interfaces.
The .mli files allow to

* hide some structure items
* write documentation for modules.
* hide dependencies between modules (i.e. speedup compilation)

Without .mli files all your functions will be 'public' in the sence of C++ style OOP.
Usually people autogenerate .mli files and all defined values in that time become public.
Zanuda is able to detect this via `zanuda -unused-decls .`.
But in some cases, .mli files may be too heavy, for example, when we write many type
declarations (sort of AST) with deriving attributes.
In that case .mli file is almost identical to .ml.
To workaround this, this check allows file with a suffix `ast.ml` not to have an .mli interface file.

TODO: Add custom configuration for this.

|}
  |> Stdlib.String.trim
;;

let lint_id = "lint_filesystem"
let describe_as_json () = Utils.describe_as_clippy_json lint_id ~docs
