open Base
open Caml.Format

type mode =
  | Unspecified
  | Dump of string
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
  ; check_filesystem = true
  }
;;

let mode () = opts.mode
let set_mode m = opts.mode <- m
let set_dump_file s = set_mode (Dump s)
let set_in_file s = set_mode (File s)
let set_in_dir s = set_mode (Dir s)
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

let parse_args () =
  let open Caml in
  let standard_args =
    [ "-o", Arg.String set_out_file, "Set Markdown output file"
    ; "-dir", Arg.String set_in_dir, "Set root directory of dune project"
    ; "-ogolint", Arg.String set_out_golint, "Set output file in golint format"
    ; "-ordjsonl", Arg.String set_out_rdjsonl, "Set output file in rdjsonl format"
    ; "-ws", Arg.String set_workspace, "Set dune workspace root"
    ; "-del-prefix", Arg.String set_prefix_to_cut, "Set prefix to cut from file names"
    ; "-add-prefix", Arg.String set_prefix_to_add, "Set prefix to reprend to file names"
    ; ( "-dump-lints"
      , Arg.String set_dump_file
      , "Dump information about available linters to JSON" )
    ; "-I", Arg.String add_include, "Add extra include path for type checking"
    ; "-v", Arg.Unit set_verbose, "More verbose output"
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
  in
  Arg.parse
    (standard_args @ List.rev extra_args)
    set_in_file
    "Use [-dir PATH] switch to check dune-based project"
;;
