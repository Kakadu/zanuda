(** Copyright 2021-2024, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
module StringSet = Set.Make (String)

(** Sonmething like Hastbl is needed **)

(* let rec print_path = function
  | Path.Pident ident -> "<ident>(" ^ Ident.unique_toplevel_name ident ^ ")"
  | Path.Pdot (lhs, rhs) -> "<arrow>(" ^ print_path lhs ^ " -> " ^ rhs ^ ")"
  | Path.Papply (lhs, rhs) -> "<apply>(" ^ print_path lhs ^ " @ " ^ print_path rhs ^ ")"
;; *)

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
  Utils.printfn "%s:" info;
  let names = Hashtbl.to_seq_keys dict |> List.of_seq |> List.sort String.compare in
  List.iter (Format.printf "  %s\n") names
;;

let print_used_decls () = print_decls "used" used_decls
let print_all_decls () = print_decls "all" all_decls

let collect_unused () =
  Hashtbl.iter (fun k _ -> Hashtbl.remove all_decls k) used_decls;
  Hashtbl.iter (fun elem _ -> Format.printf "unused decl: %s\n" elem) all_decls
;;

let collect_from_mli_tree (is_wrapped : LoadDune.w) filename tree =
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
      LoadDune.pp_w
      is_wrapped
  in
  let rec collect_from_module seed = function
    | { Typedtree.sig_items } ->
      let open Typedtree in
      List.iter
        (function
          | { sig_desc = Tsig_value { val_id = id } } ->
            (*Format.printf "found value %s\n" (seed ^ Ident.name id);*)
            add_just_decl (seed ^ "." ^ Ident.name id)
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
