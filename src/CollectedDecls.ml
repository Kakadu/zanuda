(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Hashtbl
open Queue
module StringSet = Set.Make (String)

(** Sonmething like Hastbl is needed **)

let rec print_path = function
  | Path.Pident ident -> "<ident>(" ^ Ident.unique_toplevel_name ident ^ ")"
  | Path.Pdot (lhs, rhs) -> "<arrow>(" ^ print_path lhs ^ " -> " ^ rhs ^ ")"
  | Path.Papply (lhs, rhs) -> "<apply>(" ^ print_path lhs ^ " @ " ^ print_path rhs ^ ")"
;;

let all_decls = Hashtbl.create 100
let used_decls = Hashtbl.create 100

let add_decl dict module_name decl =
  if not (Hashtbl.mem dict module_name)
  then Hashtbl.add dict module_name (Queue.create ());
  ();
  Queue.add decl (Hashtbl.find dict module_name)
;;

let add_used_decl module_name decl = add_decl used_decls module_name decl
let add_just_decl module_name decl = add_decl all_decls module_name decl

let print_decls seed dict _ =
  Hashtbl.iter
    (fun module_name decls ->
      Format.printf "module `%s` (%s):\n" module_name seed;
      Queue.iter (fun path -> Format.printf "    logged decl : %s\n" path) decls)
    dict
;;

let print_used_decls = print_decls "used" used_decls
let print_all_decls = print_decls "all" all_decls

let collect_unused _ =
  let queue2set q =
    Queue.fold (fun acc value -> StringSet.add value acc) StringSet.empty q
  in
  let unused =
    Hashtbl.fold
      (fun module_name decls acc ->
        let all_decls_set = queue2set decls in
        if Hashtbl.mem used_decls module_name
        then (
          let used_decls_set = queue2set (Hashtbl.find used_decls module_name) in
          (StringSet.diff all_decls_set used_decls_set |> StringSet.elements) :: acc)
        else  
          StringSet.elements all_decls_set :: acc
      )
      all_decls
      []
  in
  List.iter
    (fun elem -> List.iter (fun decl -> Format.printf "unused decl: %s\n" decl) elem)
    unused
;;

let collect_from_mli_tree filename tree =
  let module_name =
    filename
    |> String.split_on_char '/'
    |> List.rev
    |> List.hd
    |> String.split_on_char '.'
    |> List.hd
    |> String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c)
  in
  let rec collect_from_module seed = function
    | { Typedtree.sig_items } ->
      let open Typedtree in
      List.iter
        (function
          | { sig_desc = Tsig_value { val_id = id } } ->
            (*Format.printf "found value %s\n" (seed ^ Ident.name id);*)
            add_just_decl module_name (seed ^ Ident.name id)
          | { sig_desc =
                Tsig_module
                  { md_id = Some id; md_type = { mty_desc = Tmty_signature sign } }
            } ->
            (*Format.printf "found module %s\n" (Ident.name id);*)
            collect_from_module (seed ^ Ident.name id ^ ".") sign
          | _ -> ())
        sig_items
  in
  collect_from_module (module_name ^ ".") tree
;;