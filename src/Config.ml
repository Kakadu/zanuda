(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Caml.Format

type mode =
  | Unspecified
  | Dump_json of string
  | Dump_text
  | File of string
  | Dir of string

type t =
  { mutable outfile : string option
  ; mutable outgolint : string option
  ; mutable out_rdjsonl : string option
      (* Spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf#rdjson *)
  ; mutable mode : mode
      (* Below options to manage file paths. Not sure are they really required *)
  ; mutable workspace : string option
  ; mutable prefix_to_cut : string option
  ; mutable prefix_to_add : string option
  ; mutable extra_includes : string list
  ; mutable verbose : bool
  ; enabled_lints : string Hash_set.t
  ; mutable skip_level_allow : bool
  ; mutable check_filesystem : bool
  }

let opts =
  { outfile = None
  ; outgolint = None
  ; out_rdjsonl = None
  ; mode = Unspecified
  ; workspace = None
  ; prefix_to_cut = Some "_build/default/"
  ; prefix_to_add = None
  ; extra_includes = []
  ; verbose = false
  ; enabled_lints = Hash_set.create (module String)
  ; skip_level_allow = true
  ; check_filesystem = true
  }
;;

(** Modes *)

let mode () = opts.mode
let set_mode m = opts.mode <- m
let set_dump_file s = set_mode (Dump_json s)
let set_dump_text () = set_mode Dump_text
let set_in_file s = set_mode (File s)
let set_in_dir s = set_mode (Dir s)

(** Other switches *)

let add_include s = opts.extra_includes <- s :: opts.extra_includes
let set_out_file s = opts.outfile <- Some s
let set_out_golint s = opts.outgolint <- Some s
let set_out_rdjsonl s = opts.out_rdjsonl <- Some s
let set_workspace s = opts.workspace <- Some s
let set_prefix_to_cut s = opts.prefix_to_cut <- Some s
let set_prefix_to_add s = opts.prefix_to_add <- Some s
let includes () = opts.extra_includes
let prefix_to_cut () = opts.prefix_to_cut
let prefix_to_add () = opts.prefix_to_add
let is_check_filesystem () = opts.check_filesystem
let enabled_lints () = opts.enabled_lints
let outfile () = opts.outfile
let out_golint () = opts.outgolint
let out_rdjsonl () = opts.out_rdjsonl
let unset_check_filesystem () = opts.check_filesystem <- false
let verbose () = opts.verbose
let set_verbose () = opts.verbose <- true
let set_skip_level_allow b = opts.skip_level_allow <- b

let recover_filepath filepath =
  let filepath =
    match prefix_to_cut () with
    | Some prefix when String.is_prefix filepath ~prefix ->
      String.drop_prefix filepath (String.length prefix)
    | Some prefix when verbose () ->
      Caml.Format.eprintf "Can't cut prefix '%s' from '%s'\n%!" prefix filepath;
      filepath
    | Some _ | None -> filepath
  in
  let filepath =
    match prefix_to_add () with
    | Some s -> sprintf "%s%s" s filepath
    | None -> filepath
  in
  filepath
;;

let is_enabled () =
  let hash = enabled_lints () in
  fun (module M : LINT.GENERAL) ->
    (* Format.printf "is_enabled of %s\n%!" M.lint_id; *)
    match M.level with
    | LINT.Allow when opts.skip_level_allow -> false
    | _ -> Hash_set.mem hash M.lint_id
;;

let parse_args () =
  let open Caml in
  let standard_args =
    [ "-o", Arg.String set_out_file, "[FILE] Set Markdown output file"
    ; "-dump", Arg.Unit set_dump_text, "Dump info about available lints to terminal"
    ; ( "-dump-lints"
      , Arg.String set_dump_file
      , "[FILE] Dump information about available lints to JSON" )
    ; "-dir", Arg.String set_in_dir, "[FILE] Set root directory of dune project"
    ; "-ogolint", Arg.String set_out_golint, "Set output file in golint format"
    ; "-ordjsonl", Arg.String set_out_rdjsonl, "Set output file in rdjsonl format"
    ; ( "-del-prefix"
      , Arg.String set_prefix_to_cut
      , "Set prefix to cut from pathes in OUTPUT file" )
    ; ( "-add-prefix"
      , Arg.String set_prefix_to_add
      , "Set prefix to prepend to pathes in OUTPUT file" )
    ; "-I", Arg.String add_include, "Add extra include path for type checking"
      (* ; "-ws", Arg.String set_workspace, "[FILE] Set dune workspace root" *)
    ; ( "-skip-level-allow"
      , Arg.Bool set_skip_level_allow
      , "[bool] Skip lints with level = Allow" )
    ; "-v", Arg.Unit set_verbose, "More verbose output"
    ; ( "-version"
      , Arg.Unit
          (fun () ->
            let open Build_info.V1 in
            Printf.printf
              "version: %s\n"
              (Option.fold ~none:"n/a" ~some:Version.to_string (version ())))
      , " print version" )
    ]
  in
  let extra_args =
    Hash_set.fold
      ~init:
        [ ( "-no-check-filesystem"
          , Arg.Unit unset_check_filesystem
          , " Disable checking structure of a project" )
        ]
      ~f:(fun acc x ->
        assert (x <> "");
        ( sprintf "-no-%s" x
        , Arg.Unit (fun () -> Hash_set.remove opts.enabled_lints x)
        , " Disable checking for this lint" )
        :: acc)
      opts.enabled_lints
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  Arg.parse
    (standard_args @ extra_args)
    set_in_file
    "Use -dir [PATH] to check dune-based project"
;;
