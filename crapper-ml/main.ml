(* Entry point of the crapper: CRAP metrics for OCaml functions. *)

open Crapper_ml

let no_coverage = ref false
let use_existing_coverage = ref false
let threshold = ref None
let roots = ref []
let usage = "usage: crapper [options] [files-or-dirs]"

let () =
  let anon arg = roots := arg :: !roots in
  Arg.parse
    (Arg.align
       [ ( "--no-coverage"
         , Arg.Set no_coverage
         , " complexity only; coverage and CRAP are N/A" )
       ; ( "--use-existing-coverage"
         , Arg.Set use_existing_coverage
         , " read coverage reports already on disk" )
       ; ( "--threshold"
         , Arg.Int (fun i -> threshold := Some i)
         , "N exit 2 when the worst score is higher" )
       ])
    anon
    usage
;;

let () =
  let roots =
    match List.rev !roots with
    | [] -> Discover.default_roots
    | roots -> roots
  in
  let cmts = Discover.discover_cmt roots in
  if cmts = []
  then print_endline "No .cmt files found. Is the project compiled?"
  else (
    let functions =
      List.concat_map
        (fun cmt_path ->
          try Crapper_ml.Extract.functions_of_cmt cmt_path with
          | exn ->
            Crapper_ml.Extract.warn_broken_cmt cmt_path exn;
            [])
        cmts
    in
    if functions = []
    then print_endline "No functions found"
    else
      List.iter
        (fun f ->
          Printf.printf
            "%s:%d-%d  %s.%s\n"
            f.Model.path
            f.Model.start_line
            f.Model.end_line
            f.Model.namespace
            f.Model.name)
        functions;
    (* The worst CRAP score is compared with the threshold once functions and
     their scores are computed. Nothing is scored yet. *)
    ())
;;
