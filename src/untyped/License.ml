open Caml.Format
open Zanuda_core
open Utils

let lint_id = "top_file_license"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:{|
### What it does

### Why is this bad?
  |}
;;

type input = Tast_iterator.iterator

let msg ppf _name = fprintf ppf "No license comment"

let report ~loc ~filename typ_name =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg typ_name

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        typ_name
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pm () =
    let open Tast_pattern in
    tstr_docattr __
  in
  let open Typedtree in
  let parse_str h1 =
    Tast_pattern.parse (pm ()) h1.str_loc h1 ~on_error:(fun _ -> None) (fun s -> Some s)
  in
  (* print_endline "check license"; *)
  (* TODO: use angstrom *)
  let open Tast_iterator in
  { fallback with
    structure =
      (fun _ stru ->
        match stru.str_items with
        | h1 :: h2 :: _ ->
          let rez1 = parse_str h1 in
          let rez2 = parse_str h2 in
          (match rez1, rez2 with
           | None, None ->
             let loc = h1.str_loc in
             let filename = loc.Location.loc_start.Lexing.pos_fname in
             CollectedLints.add ~loc @@ report ~loc ~filename "WTF"
           | _ -> ())
        | _ -> ())
  }
;;
