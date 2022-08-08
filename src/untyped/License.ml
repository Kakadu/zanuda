open Caml.Format
open Zanuda_core
open Utils

include (
  struct
    open Angstrom

    let is_space = function
      | ' ' | '\t' -> true
      | _ -> false
    ;;

    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    ;;

    let ws = skip_while is_space
    let year = take_while1 is_digit *> return ()

    let parse1 =
      let year_range = return (fun _ _ -> ()) <*> (year <* char '-') <*> year in
      ws *> string "Copyright" *> ws *> (year_range <|> year)
      <* ws
      <* char ','
      <* take_while (fun _ -> true)
    ;;

    let decent_char = function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '.' -> true
      | _ -> false
    ;;

    let parse2 =
      ws *> string "SPDX-License-Identifier:" *> ws *> take_while1 decent_char *> ws
    ;;

    let parses p str =
      match parse_string ~consume:Consume.All p str with
      | Result.Ok _ -> true
      | Error _ -> false
    ;;

    let verify_line1 = parses parse1
    let verify_line2 = parses parse2
  end :
    sig
      val verify_line1 : string -> bool
      val verify_line2 : string -> bool
    end)

let%test _ = verify_line1 " Copyright 2021-2022, Kakadu and contributors  "
let%test _ = verify_line2 " SPDX-License-Identifier: LGPL-3.0-or-later  "

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

let msg ppf s = fprintf ppf "%s" s

let report ~loc ~filename reason =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg reason

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~code:lint_id
        ppf
        msg
        reason
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

type panic_mode =
  | No_license_at_all
  | No_second_line
  | Bad_copyright_info
  | Bad_license_info

let panic ~loc ~filename mode =
  CollectedLints.add
    ~loc
    (match mode with
     | No_license_at_all ->
       report
         ~loc
         ~filename
         "OCaml files should start from copyright and license information"
     | No_second_line ->
       report
         ~loc
         ~filename
         "OCaml files should provide license information in second line (structure item)"
     | Bad_copyright_info ->
       report
         ~loc
         ~filename
         {| First item in file should be a documentation comment with copyright information. For example:
            (** Copyright 2021-2022, Winnie Pooh et al. *)|}
     | Bad_license_info ->
       report
         ~loc
         ~filename
         {|Second item in file should be a documentation comment with corrent license information. For example:
           (** SPDX-License-Identifier: LGPL-3.0-or-later *)|})
;;

let run info fallback =
  let pm () =
    let open Tast_pattern in
    tstr_docattr __
  in
  let open Typedtree in
  let extract_string h1 =
    Tast_pattern.parse (pm ()) h1.str_loc h1 ~on_error:(fun _ -> None) (fun s -> Some s)
  in
  (* print_endline "check license"; *)
  (* TODO: use angstrom *)
  let filename = info.Compile_common.source_file in
  let loc = Location.in_file filename in
  let open Tast_iterator in
  { fallback with
    structure =
      (fun _ { str_items = items } ->
        let wrap h1 mode =
          let verifier, bad_spec =
            match mode with
            | `First -> verify_line1, Bad_copyright_info
            | `Second -> verify_line2, Bad_license_info
          in
          match extract_string h1 with
          | None ->
            let loc = h1.str_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some s when not (verifier s) ->
            let loc = h1.str_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some _ -> ()
        in
        let () =
          match items with
          | h1 :: _ -> wrap h1 `First
          | [] -> panic ~loc ~filename No_license_at_all
        in
        let () =
          match items with
          | _ :: h2 :: _ -> wrap h2 `Second
          | [ _ ] | [] -> panic ~loc ~filename No_license_at_all
        in
        ())
  }
;;
