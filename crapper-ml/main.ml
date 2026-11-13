(* Entry point of the crapper: CRAP metrics for OCaml functions. *)

open Crapper_ml

let no_coverage = ref false
let use_existing_coverage = ref false
let threshold = ref None
let coverage_command = ref None
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
       ; ( "--coverage-command"
         , Arg.String (fun s -> coverage_command := Some s)
         , "CMD replace the default coverage command with one of your own" )
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
      let coverage =
        if !no_coverage
        then None
        else
          Some
            (Coverage.load
               ~use_existing:!use_existing_coverage
               ~command:
                 (match !coverage_command with
                  | Some cmd -> cmd
                  | None -> Coverage.default_coverage_command))
      in
      let coverage_of f =
        match coverage with
        | None -> None
        | Some data -> Coverage.coverage_of f data
      in
      functions
      |> List.map (fun f -> Crap.make_entry ~coverage:(coverage_of f) f)
      |> Crap.sort_entries
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
