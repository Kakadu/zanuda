[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Utils
module StringSet = Set.Make (String)

let all_decls = Hashtbl.create 100
let used_decls = Hashtbl.create 100

let add_decl dict decl =
  if String.starts_with ~prefix:"Stdlib." decl
  then ()
  else if not (Hashtbl.mem dict decl)
  then Hashtbl.add dict decl ()
;;

let add_used_decl decl = add_decl used_decls decl

let add_just_decl decl =
  (* printfn "%s: %s" __FUNCTION__ decl; *)
  add_decl all_decls decl
;;

let print_decls info dict =
  let names = Hashtbl.to_seq_keys dict |> List.of_seq |> List.sort String.compare in
  if not (Base.List.is_empty names)
  then (
    Utils.printfn "%s:" info;
    List.iteri (fun i -> Format.printf "%2d: %s\n" i) names)
;;

let print_used_decls () = print_decls "used" used_decls
let print_all_decls () = print_decls "all" all_decls

let collect_unused () =
  Hashtbl.iter (fun k _ -> Hashtbl.remove all_decls k) used_decls;
  print_decls "Unused declarations" all_decls
;;

let not_skippable_ident id =
  let name = Ident.name id in
  (* Highly likely some function created by ppx_deriving *)
  (not (Base.String.is_prefix name ~prefix:"pp_"))
  && (not (Base.String.is_prefix name ~prefix:"show_"))
  (* TODO: detect usage of fancy operators. Currently we don't count them *)
  && (not (Base.String.equal name "let*"))
  && not (Base.String.equal name "let+")
;;

let collect_from_mli_tree (is_wrapped : Load_dune.w) filename tree =
  let module_name =
    filename
    |> String.split_on_char '/'
    |> List.rev
    |> List.hd
    |> String.split_on_char '.'
    |> List.hd
    |> String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c)
  in
  let __ _ =
    printfn
      "%s, modname = %s,    wrapped = %a"
      __FUNCTION__
      module_name
      Load_dune.pp_w
      is_wrapped
  in
  let rec collect_from_module seed = function
    | { Typedtree.sig_items } ->
      let open Typedtree in
      List.iter
        (function
          | { sig_desc = Tsig_value { val_id = id } } ->
            (*Format.printf "found value %s\n" (seed ^ Ident.name id);*)
            if not_skippable_ident id then add_just_decl (seed ^ "." ^ Ident.name id)
          | { sig_desc =
                Tsig_module
                  { md_id = Some id; md_type = { mty_desc = Tmty_signature sign } }
            } ->
            (*Format.printf "found module %s\n" (Ident.name id);*)
            collect_from_module (seed ^ "." ^ Ident.name id) sign
          | _ -> ())
        sig_items
  in
  collect_from_module
    (match is_wrapped with
     | Wrapped name -> name ^ "." ^ module_name
     | _ -> module_name)
    tree
;;
