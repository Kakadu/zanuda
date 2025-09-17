(** A check that every file starts from SPDX copyright and license information.
    N.B. ppx could add extra attributes that complicate this check a little bit *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Dmitrii Kosarev a.k.a. Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-only *)

[@@@ocaml.text "/*"]

(* TODO: support double licensing (AND keyword) *)

(* TODO: Write down why this lint is implemented as typed and not as untyped *)

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
Ensures that every files start from license and copyright information.
The description is expected in [SPDX](https://spdx.org/licenses) format.

### Why is this bad?
These annotation allow automation tools to check code for license compliance.

````
[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Vasya Pupkin *)

(** SPDX-License-Identifier: LGPL-3.0-only *)

[@@@ocaml.text "/*"]
````

The `"/*"` decorations prevent ocamldoc/odoc from including comments with license into documentation.
|}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

type input = Tast_iterator.iterator

let msg ppf s = Stdlib.Format.fprintf ppf "%s" s

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
  Collected_lints.add
    ~loc
    (match mode with
     | No_license_at_all ->
       report
         ~loc
         ~filename
         "OCaml files should start with copyright and license information"
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
           (** SPDX-License-Identifier: AGPL-3.0-only *)|})
;;

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
  let filename = Utils.source_of_info info in
  let iter_items extract get_loc items =
    let on_last_line () =
      let loc, loc2 =
        match items with
        | [] ->
          let l = Location.in_file filename in
          l, l
        | h :: [] ->
          let l = get_loc h in
          l, l
        | l1 :: l2 :: _ -> get_loc l1, get_loc l2
      in
      panic ~loc ~filename Bad_copyright_info;
      panic ~loc:loc2 ~filename No_second_line
    in
    let rec loop = function
      | l1 :: l2 :: tl ->
        (match extract l1 with
         | None -> loop (l2 :: tl)
         | Some s when verify_line1 s ->
           (* Line 1 verified *)
           (match extract l2 with
            | None -> panic ~loc:(get_loc l2) ~filename No_second_line
            | Some s when not (verify_line2 s) ->
              panic ~loc:(get_loc l2) ~filename Bad_copyright_info
            | _ -> () (* success *))
         | Some _ ->
           (* We skip unsuited lines. We hope that these lines were added by PPX rewriters,
              and actual license information is below *)
           loop (l2 :: tl))
      | l1 :: [] ->
        (match extract l1 with
         | Some s when verify_line1 s ->
           (* Copyright is written, license doesn't *)
           panic ~loc:(get_loc l1) ~filename No_license_at_all
         | _ -> on_last_line ())
      | [] -> on_last_line ()
    in
    loop items
  in
  let open Tast_iterator in
  { fallback with
    signature =
      (fun _ { sig_items } ->
        iter_items extract_string_sig (fun l -> l.sig_loc) sig_items)
  ; structure =
      (fun _ { str_items } -> iter_items extract_string (fun l -> l.str_loc) str_items)
  }
;;
