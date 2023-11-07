(** Copyright 2021-2023, Dmitrii Kosarev a.k.a. Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-only *)

(*
   Checking that every file starts from SPDX copyright and license information.
   N.B. ppx could add extra attributes that complicate this check a little bit

   TODO: support double licensing (AND keyword)
*)

(* TODO: We can omit appearance of the license in the generated documentation.
   We should write like this:
   (** /* *)
   (** Copyright ... *)
   (** SPDX-License-Identifier: ... *)
   (** */ *)
*)

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
let level = LINT.Warn

let documentation =
  {|
### What it does
Ensures that every files start from license and copyright information. The description is expected in SPDX format.

### Why is this bad?
These annotation allow automatization tools to check code for license compliance.
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
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
         {|Second item in file should be a documentation comment with correct license information. For example:
           (** SPDX-License-Identifier: LGPL-3.0-or-later *)|})
;;

(*
   Typical ppx_bisect support code:
   + [@@@ocaml.text "/*"]
   + module Bisect_visit___good2___ml =
   + struct
   + ...
   + end
   + open Bisect_visit___good2___ml
   + [@@@ocaml.text "/*"]

   It could be changed in the new version of PPX-bisect
*)
let run info fallback =
  let open Typedtree in
  let extract_string h1 =
    let open Tast_pattern in
    let pm = tstr_docattr __ in
    parse pm h1.str_loc h1 ~on_error:(fun _ -> None) (fun s -> Some s)
  in
  let extract_string_sig h1 =
    let open Tast_pattern in
    let pm = tsig_docattr __ in
    parse pm h1.sig_loc h1 ~on_error:(fun _ -> None) (fun s -> Some s)
  in
  let is_ocaml_ppx_context item =
    let open Tast_pattern in
    let pm = tstr_attribute (attribute (string "ocaml.ppx.context") drop) in
    parse pm item.str_loc item ~on_error:(fun _ -> false) true
  in
  let is_ocaml_ppx_context_sig item =
    let open Tast_pattern in
    let pm = tsig_attribute (attribute (string "ocaml.ppx.context") drop) in
    parse pm item.sig_loc item ~on_error:(fun _ -> false) true
  in
  let is_ppx_bisect_artifact item =
    let open Tast_pattern in
    let pm =
      let pat_bisect_mod () =
        of_func (fun _ loc x k ->
          match x.Typedtree.str_desc with
          | Tstr_module { mb_name = { txt = Some name }; _ }
            when String.starts_with ~prefix:"Bisect_visit" name -> k
          | _ -> fail loc "")
      in
      let pat_bisect_open () =
        of_func (fun _ loc x k ->
          match x.Typedtree.str_desc with
          | Tstr_open { open_expr = { mod_desc = Tmod_ident (_, { txt = Lident lid }) } }
            when String.starts_with ~prefix:"Bisect_visit" lid -> k
          | _ -> fail loc "")
      in
      let pat_bisect_c_comment () =
        tstr_attribute
          (attribute
             (string "ocaml.text")
             (of_func (fun _ loc x k ->
                match x with
                | Parsetree.PStr
                    [ { pstr_desc =
                          Pstr_eval
                            ( { pexp_desc = Pexp_constant (Pconst_string ("/*", _, None)) }
                            , _ )
                      }
                    ] -> k
                | _ -> fail loc "Can't parse right payload")))
      in
      pat_bisect_mod () ||| pat_bisect_open () ||| pat_bisect_c_comment ()
    in
    parse pm item.str_loc item ~on_error:(fun _ -> false) true
  in
  let filename = info.Compile_common.source_file in
  let loc = Location.in_file filename in
  let open Tast_iterator in
  { fallback with
    signature =
      (fun _ { sig_items = items } ->
        let wrap item mode =
          let verifier, bad_spec =
            match mode with
            | `First -> verify_line1, Bad_copyright_info
            | `Second -> verify_line2, Bad_license_info
          in
          match extract_string_sig item with
          | None ->
            let loc = item.sig_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some s when not (verifier s) ->
            let loc = item.sig_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some _ -> ()
        in
        let () =
          let rec loop = function
            | h :: tl when is_ocaml_ppx_context_sig h -> loop tl
            | h :: _ -> wrap h `First
            | [] -> panic ~loc ~filename No_license_at_all
          in
          loop items
        in
        let () =
          let rec loop = function
            | h :: tl when is_ocaml_ppx_context_sig h -> loop tl
            | _ :: h :: _ -> wrap h `Second
            | [ _ ] | [] -> panic ~loc ~filename No_license_at_all
          in
          loop items
        in
        ())
  ; structure =
      (fun _ { str_items = items } ->
        let wrap item mode =
          let verifier, bad_spec =
            match mode with
            | `First -> verify_line1, Bad_copyright_info
            | `Second -> verify_line2, Bad_license_info
          in
          match extract_string item with
          | None ->
            let loc = item.str_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some s when not (verifier s) ->
            let loc = item.str_loc in
            let filename = loc.Location.loc_start.Lexing.pos_fname in
            panic ~loc ~filename bad_spec
          | Some _ -> ()
        in
        (* The analysis is complicated because various preprocessors can
           install code in the beginning of the file
           TODO(Kakadu). Maybe we should look for two documentation comments
           colocated in the file, and not in the beginning of the file?
        *)
        let () =
          let rec loop = function
            | h :: tl when is_ocaml_ppx_context h -> loop tl
            | h :: tl when is_ppx_bisect_artifact h -> loop tl
            | h :: _ -> wrap h `First
            | [] -> panic ~loc ~filename No_license_at_all
          in
          loop items
        in
        let () =
          let rec loop = function
            | h :: tl when is_ocaml_ppx_context h -> loop tl
            | h :: tl when is_ppx_bisect_artifact h -> loop tl
            | _ :: h :: _ -> wrap h `Second
            | [ _ ] | [] -> panic ~loc ~filename No_license_at_all
          in
          loop items
        in
        ())
  }
;;
