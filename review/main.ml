(* https://docs.github.com/en/rest/pulls/reviews#create-a-review-for-a-pull-request
   *)
[@@@ocaml.warnerror "-32"]

let _example () =
  match Curly.(run (Request.make ~url:"https://opam.ocaml.org" ~meth:`GET ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers;
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

(* let owner = "Kakadu"
let repo = "test-ocaml-ci-docker"
let pull_number = 2
let commit_id = "18f545c56d3e95f2b1cad0d127dc988969a397eb" *)

let your_token =
  match Sys.getenv "TOKEN" with
  | s -> s
  | exception Not_found -> failwith "Setup token using env TOKEN"
;;

type info =
  { mutable commit_id : string
  ; mutable pull_number : int
  ; mutable repo : string
  ; mutable owner : string
  ; mutable review_id : int
  }

let info =
  { owner = "Kakadu"
  ; repo = "test-ocaml-ci-docker"
  ; pull_number = 2
  ; commit_id = "18f545c56d3e95f2b1cad0d127dc988969a397eb"
  ; review_id = 0
  }
;;

open Printf

let create_review () =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/pulls/%d/reviews"
      info.owner
      info.repo
      info.pull_number
  in
  let headers =
    [ "Authorization", sprintf "Bearer %s" your_token
    ; "Accept", "application/vnd.github+json"
    ]
  in
  let body =
    let make_comment ~path ~body position =
      `Assoc [ "path", `String path; "position", `Int position; "body", `String body ]
    in
    `Assoc
      [ "commit_id", `String info.commit_id
      ; "body", `String "body"
      ; "event", `String "REQUEST_CHANGES"
      ; "comments", `List [ make_comment ~path:"Lambda/lib/ast.mli" ~body:"THE_BODY" 6 ]
      ]
    |> Yojson.Safe.pretty_to_string
  in
  match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    (* Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers; *)
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

let dismiss_review review_id =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/pulls/%d/reviews/%d"
      info.owner
      info.repo
      info.pull_number
      review_id
  in
  let headers =
    [ "Authorization", sprintf "Bearer %s" your_token
    ; "Accept", "application/vnd.github+json"
    ]
  in
  match Curly.(run (Request.make ~headers ~url ~meth:`DELETE ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    (* Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers; *)
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

let list_reviews info =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/pulls/%d/reviews"
      info.owner
      info.repo
      info.pull_number
  in
  let headers =
    [ "Authorization", sprintf "Bearer %s" your_token
    ; "Accept", "application/vnd.github+json"
    ]
  in
  match Curly.(run (Request.make ~headers ~url ~meth:`GET ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    (* Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers; *)
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

let submit_review info =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/pulls/%d/reviews/%d/events"
      info.owner
      info.repo
      info.pull_number
      info.review_id
  in
  let headers =
    [ "Authorization", sprintf "Bearer %s" your_token
    ; "Accept", "application/vnd.github+json"
    ]
  in
  let body =
    `Assoc [ "body", `String "body"; "event", `String "REQUEST_CHANGES" ]
    |> Yojson.Safe.pretty_to_string
  in
  match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
  | Ok x ->
    Format.printf "status: %d\n" x.Curly.Response.code;
    (* Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers; *)
    Format.printf "body: %s\n" x.Curly.Response.body
  | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
;;

let () =
  Arg.parse
    [ "-commit", Arg.String (fun s -> info.commit_id <- s), " "
    ; "-review_id", Arg.Int (fun s -> info.review_id <- s), " "
    ; "-pr_number", Arg.Int (fun s -> info.pull_number <- s), " "
    ; "-repo", Arg.String (fun s -> info.repo <- s), " "
    ; "-owner", Arg.String (fun s -> info.owner <- s), " "
      (* *** *** *** *** *** *** *** *** *)
    ; "-disreview", Arg.Unit (fun () -> dismiss_review info.review_id), " "
    ; "-review", Arg.Unit (fun () -> create_review ()), " "
    ; "-submit_review", Arg.Unit (fun () -> submit_review info), " "
    ; "-list_reviews", Arg.Unit (fun () -> list_reviews info), " "
    ]
    (fun _ -> assert false)
    " xxx "
;;
