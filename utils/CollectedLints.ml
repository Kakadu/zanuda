open Base
open Format

type t = string
type single_printer = loc:Warnings.loc -> Format.formatter -> unit

let found_Lints : (Location.t * (module LINT.REPORTER)) Queue.t = Queue.create ()
let clear () = Queue.clear found_Lints
let is_empty () = Queue.is_empty found_Lints
let add ~loc m = Queue.enqueue found_Lints (loc, m)

let report () =
  let mdfile =
    match Config.Options.outfile () with
    | Some s ->
      (* Format.printf "Opening file '%s'...\n%!" s; *)
      let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
      let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
      [ ( (fun (module M : LINT.REPORTER) ppf -> M.md ppf)
        , Format.formatter_of_out_channel ch
        , ch )
      ]
    | None -> []
  in
  let golint_files =
    match Config.Options.out_golint () with
    | Some s ->
      let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
      (* By some reason on CI Open_creat is not enough to create a file *)
      let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
      [ ( (fun (module M : LINT.REPORTER) ppf -> M.golint ppf)
        , Format.formatter_of_out_channel ch
        , ch )
      ]
    | None -> []
  in
  let rdjsonl_files =
    match Config.Options.out_rdjsonl () with
    | Some s ->
      let (_ : int) = Caml.Sys.command (asprintf "touch %s" s) in
      (* By some reason on CI Open_creat is not enough to create a file *)
      let ch = Caml.open_out_gen [ Caml.Open_append; Open_creat ] 0o666 s in
      [ ( (fun (module M : LINT.REPORTER) ppf -> M.rdjsonl ppf)
        , Format.formatter_of_out_channel ch
        , ch )
      ]
    | None -> []
  in
  let all_files = List.concat [ rdjsonl_files; golint_files; mdfile ] in
  Base.Exn.protect
    ~f:(fun () ->
      (* Format.printf "Total lints found: %d\n%!" (Queue.length found_Lints); *)
      Queue.iter found_Lints ~f:(fun (_loc, ((module M : LINT.REPORTER) as m)) ->
          M.txt Format.std_formatter ();
          List.iter all_files ~f:(fun (f, ppf, _) -> f m ppf ())))
    ~finally:(fun () ->
      let f (_, ppf, ch) =
        Format.pp_print_flush ppf ();
        Caml.close_out ch
      in
      List.iter ~f all_files)
;;
