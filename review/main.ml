(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* https://docs.github.com/en/rest/pulls/reviews#create-a-review-for-a-pull-request *)

let _example () =
  match Curly.(run (Request.make ~url:"https://opam.ocaml.org" ~meth:`GET ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers;
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

type info =
  { mutable commit_id : string option
  ; mutable pull_number : int option
  ; mutable repo : string option
  ; mutable owner : string option
  ; mutable review_id : int option
  ; mutable in_rdjsonl : string option
  ; mutable token : string option
  }

let info =
  { owner = None
  ; repo = None
  ; pull_number = None
  ; commit_id = None
  ; review_id = None
  ; in_rdjsonl = None
  ; token = None
  }
;;

open Printf

let github_patch_url ~owner ~repo ~pull_number =
  Printf.sprintf "https://api.github.com/repos/%s/%s/pulls/%d" owner repo pull_number
;;

let get_diff ~owner ~repo ~pull_number =
  let url = github_patch_url ~owner ~repo ~pull_number in
  let headers = [ "Accept", "application/vnd.github.diff" ] in
  match Curly.(run (Request.make ~headers ~url ~meth:`GET ())) with
  | Ok x -> Some x.Curly.Response.body
  | Error e ->
    Format.eprintf "Fail to get diff: %a\n" Curly.Error.pp e;
    None
;;

let create_review info =
  match
    info.owner, info.repo, info.pull_number, info.commit_id, info.in_rdjsonl, info.token
  with
  | Some owner, Some repo, Some pull_number, Some commit_id, Some in_rdjsonl, Some token
    ->
    (match get_diff ~owner ~repo ~pull_number with
     | Some diff ->
       (match Diff_parser.parse_string diff with
        | Error e_parse ->
          Format.eprintf "Parsing failed: %s\n" e_parse;
          Format.eprintf
            "of a diff from '%s'\n"
            (github_patch_url ~owner ~repo ~pull_number);
          exit 1
        | Result.Ok parsed ->
          let parse_json json =
            let open Yojson.Basic.Util in
            let msg = json |> member "message" |> to_string in
            let loc = json |> member "location" in
            let file = loc |> member "path" |> to_string in
            let range = loc |> member "range" in
            let start = range |> member "start" in
            let line = start |> member "line" |> to_int in
            msg, file, line
          in
          let make_comment path pos body =
            `Assoc [ "path", `String path; "body", `String body; "position", `Int pos ]
          in
          let comments =
            in_rdjsonl
            |> Yojson.Basic.seq_from_file
            |> Seq.filter_map (fun json ->
              let msg, file, line = parse_json json in
              match Diff_parser.lookup parsed ~file ~line with
              | Some diff_pos -> Some (make_comment file diff_pos msg)
              | None -> None)
            |> List.of_seq
          in
          let url =
            Printf.sprintf
              "https://api.github.com/repos/%s/%s/pulls/%d/reviews"
              owner
              repo
              pull_number
          in
          let headers =
            [ "Authorization", sprintf "Bearer %s" token
            ; "Accept", "application/vnd.github+json"
            ]
          in
          let body =
            `Assoc
              [ "commit_id", `String commit_id
              ; "body", `String "Zanuda-linter report"
              ; "event", `String "COMMENT"
              ; "comments", `List comments
              ]
            |> Yojson.Safe.pretty_to_string
          in
          (match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
           | Ok x ->
             Format.printf "status: %d\n" x.Curly.Response.code;
             Format.printf "body: %s\n" x.Curly.Response.body
           | Error e_api -> Format.eprintf "Failed: %a\n" Curly.Error.pp e_api))
     | None -> ())
  | _ -> Format.eprintf "Some argument was not initialized\n"
;;

let dismiss_review info =
  match info.owner, info.repo, info.pull_number, info.review_id, info.token with
  | Some owner, Some repo, Some pull_number, Some review_id, Some token ->
    let url =
      Printf.sprintf
        "https://api.github.com/repos/%s/%s/pulls/%d/reviews/%d"
        owner
        repo
        pull_number
        review_id
    in
    let headers =
      [ "Authorization", sprintf "Bearer %s" token
      ; "Accept", "application/vnd.github+json"
      ]
    in
    (match Curly.(run (Request.make ~headers ~url ~meth:`DELETE ())) with
     | Ok x ->
       Format.printf "status: %d\n" x.Curly.Response.code;
       Format.printf "body: %s\n" x.Curly.Response.body
     | Error e -> Format.eprintf "Failed: %a" Curly.Error.pp e)
  | _ -> Format.eprintf "Some argument was not initialized\n"
;;

let list_reviews info =
  match info.owner, info.repo, info.pull_number, info.token with
  | Some owner, Some repo, Some pull_number, Some token ->
    let url =
      Printf.sprintf
        "https://api.github.com/repos/%s/%s/pulls/%d/reviews"
        owner
        repo
        pull_number
    in
    let headers =
      [ "Authorization", sprintf "Bearer %s" token
      ; "Accept", "application/vnd.github+json"
      ]
    in
    (match Curly.(run (Request.make ~headers ~url ~meth:`GET ())) with
     | Ok x ->
       Format.printf "status: %d\n" x.Curly.Response.code;
       Format.printf "body: %s\n" x.Curly.Response.body
     | Error e -> Format.eprintf "Failed: %a" Curly.Error.pp e)
  | _ -> Format.eprintf "Some argument was not initialized\n"
;;

let submit_review info =
  match info.owner, info.repo, info.pull_number, info.review_id, info.token with
  | Some owner, Some repo, Some pull_number, Some review_id, Some token ->
    let url =
      Printf.sprintf
        "https://api.github.com/repos/%s/%s/pulls/%d/reviews/%d/events"
        owner
        repo
        pull_number
        review_id
    in
    let headers =
      [ "Authorization", sprintf "Bearer %s" token
      ; "Accept", "application/vnd.github+json"
      ]
    in
    let body =
      `Assoc [ "body", `String "body"; "event", `String "REQUEST_CHANGES" ]
      |> Yojson.Safe.pretty_to_string
    in
    (match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
     | Ok x ->
       Format.printf "status: %d\n" x.Curly.Response.code;
       Format.printf "body: %s\n" x.Curly.Response.body
     | Error e -> Format.eprintf "Failed: %a" Curly.Error.pp e)
  | _ -> Format.eprintf "Some argument was not initialized\n"
;;

let () =
  Arg.parse
    [ "-commit", Arg.String (fun s -> info.commit_id <- Some s), " A commit hash"
    ; ( "-pr_number"
      , Arg.Int (fun s -> info.pull_number <- Some s)
      , " An index of pull request (or issue)" )
    ; "-repo", Arg.String (fun s -> info.repo <- Some s), " GitHub repository"
    ; "-owner", Arg.String (fun s -> info.owner <- Some s), " Owner of that repository"
    ; ( "-irdjsonl"
      , Arg.String (fun s -> info.in_rdjsonl <- Some s)
      , " Set input file in rdjsonl format" )
    ; "-token", Arg.String (fun s -> info.token <- Some s), " An access token"
      (* *** *** *** *** *** *** *** *** *)
    ; "-review", Arg.Unit (fun () -> create_review info), " Create a review"
    ; "-submit_review", Arg.Unit (fun () -> submit_review info), " "
      (* TODO(Kakadu): is submit_review required?? *)
    ; "-review_id", Arg.Int (fun s -> info.review_id <- Some s), " A review ID"
    ; ( "-disreview"
      , Arg.Unit (fun () -> dismiss_review info)
      , " Dismiss a review specified by -review_id" )
    ; "-list_reviews", Arg.Unit (fun () -> list_reviews info), " "
    ]
    (fun s ->
      Printf.eprintf "Anonymous arguments %S is not supported\n" s;
      exit 1)
    "Use -owner [OWNER] -repo [REPO NAME] -pr_number [PR NUMBER] -commit [HASH] -token \
     [READ-WRITE TOKEN] -irdjsonl [PATH] -review to create review"
;;
