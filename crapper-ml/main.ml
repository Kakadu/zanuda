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
    let entries =
      functions
      |> List.map (Crapper_ml.Crap.make_entry ~coverage:None)
      |> Crapper_ml.Crap.sort_entries
    in
    print_endline (Crapper_ml.Report.format_report entries);
    (* exit 2 when the worst score is higher than the threshold *)
    match !threshold with
    | Some limit ->
      (match List.find_opt (fun e -> Option.is_some e.Model.crap) entries with
       | Some { Model.crap = Some worst; _ } when worst > float_of_int limit -> exit 2
       | _ -> ())
    | None -> ())
;;
