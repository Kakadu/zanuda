(** Collect used values from structure files.

    An initial implementation was contributed by GitHub user [jegorpopow] *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Utils

let run _info filename fallback =
  let _ : string = filename in
  let rec get_ident_string = function
    | Path.Pident id -> Some (Ident.name id)
    | Path.Pdot (lhs, rhs) ->
      get_ident_string lhs |> Option.map ~f:(fun str -> str ^ "." ^ rhs)
    | _ -> None
  in
  let pat =
    let open Tast_pattern in
    texp_ident __
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path () ->
            (*Format.printf "path: %s\n" (String.concat ~sep:", " (List.map ~f:Ident.unique_toplevel_name (Path.heads path)));*)
            match path, get_ident_string path with
            | Pdot (_, _), Some str ->
              (* if String.is_substring filename ~substring:"demo"
                 then printfn "Adding used %S" str; *)
              (* printfn " ... %a\n" (LoadDune.pp_w )  _info; *)
              Collected_decls.add_used_decl str
            | _, _ -> ()
            (*Format.printf "%s\n" (Ident.unique_toplevel_name (Path.head path))*))
          ();
        fallback.expr self expr)
  }
;;
