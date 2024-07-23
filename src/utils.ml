(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Stdlib.Format

let printfn fmt = kfprintf (fun ppf -> fprintf ppf "\n%!") std_formatter fmt

module ErrorFormat = struct
  let pp ppf ~filename ~line ~col:_ msg x =
    fprintf ppf "%s:%d:%d:%a\n%!" filename line (* col *) 0 msg x
  ;;
end

type rdjsonl_code = string * string option

module RDJsonl : sig
  val pp
    :  formatter
    -> filename:string
    -> line:int
    -> ?code:rdjsonl_code
    -> (formatter -> 'a -> unit)
    -> 'a
    -> unit
end = struct
  let pp ppf ~filename ~line ?code msg x =
    let location file ~line ~col =
      `Assoc
        [ "path", `String file
        ; "range", `Assoc [ "start", `Assoc [ "line", `Int line; "column", `Int col ] ]
        ]
    in
    let j =
      `Assoc
        ([ "message", `String (asprintf "%a" msg x)
         ; "location", location filename ~line ~col:1
         ; "severity", `String "INFO"
         ]
         @
         match code with
         | None -> []
         | Some (desc, None) -> [ "code", `Assoc [ "value", `String desc ] ]
         | Some (desc, Some url) ->
           [ "code", `Assoc [ "value", `String desc; "url", `String url ] ])
    in
    fprintf ppf "%s\n%!" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

let cut_build_dir s =
  let prefix = "_build/default/" in
  if String.is_prefix ~prefix s then String.drop_prefix s (String.length prefix) else s
;;

module Report = struct
  let txt ~loc ~filename ppf msg msg_arg =
    Option.iter !Location.input_lexbuf ~f:Lexing.flush_input;
    Location.input_name := cut_build_dir filename;
    let loc =
      let open Location in
      { loc with
        loc_start = { loc.loc_start with pos_fname = !input_name }
      ; loc_end = { loc.loc_end with pos_fname = !input_name }
      }
    in
    let main = Location.mkloc (fun ppf -> msg ppf msg_arg) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let rdjsonl ~loc ~filename ~code ppf msg msg_arg =
    let code = code, Some "https://kakadu.github.io/zanuda/" in
    RDJsonl.pp ppf ~filename ~line:loc.Location.loc_start.pos_lnum ~code msg msg_arg
  ;;
end

let string_of_group : LINT.group -> string = function
  | LINT.Correctness -> "correctness"
  | Style -> "style"
  | Perf -> "perf"
  | Restriction -> "restriction"
  | Deprecated -> "deprecated"
  | Pedantic -> "pedantic"
  | Complexity -> "complexity"
  | Suspicious -> "suspicious"
  | Nursery -> "nursery"
;;

let string_of_level : LINT.level -> string = function
  | LINT.Allow -> "allow"
  | Warn -> "warn"
  | Deny -> "deny"
  | Deprecated -> "deprecated"
;;

let string_of_impl = function
  | LINT.Typed -> "typed"
  | _ -> "untyped"
;;

let describe_as_clippy_json
  ?(group = LINT.Correctness)
  ?(level = LINT.Deny)
  ?(impl = LINT.Typed)
  id
  ~docs
  : Yojson.Safe.t
  =
  (* List if clippy lints https://github.com/rust-lang/rust-clippy/blob/gh-pages/master/lints.json *)
  `Assoc
    [ "id", `String id
    ; "group", `String (string_of_group group)
    ; "level", `String (string_of_level level)
    ; "impl", `String (string_of_impl impl)
    ; "docs", `String docs
    ; ( "applicability"
      , `Assoc
          [ "is_multi_part_suggestion", `Bool false
          ; "applicability", `String "Unresolved"
          ] )
    ]
;;

let no_ident ident =
  let exception Found in
  let open Tast_iterator in
  let open Typedtree in
  let it =
    { default_iterator with
      expr =
        (fun self e ->
          (* TODO: rewrite with FCPM *)
          match e.exp_desc with
          | Texp_ident (Path.Pident id, _, _) when Ident.equal id ident -> raise Found
          | Texp_function { param } when Ident.equal ident param -> ()
          | _ -> default_iterator.expr self e)
    ; case =
        (fun (type a) self (c : a case) ->
          match c.c_lhs.pat_desc with
          | Tpat_value v ->
            (match (v :> pattern) with
             | { pat_desc = Tpat_var (id, _) } ->
               if Ident.equal ident id then () else default_iterator.case self c
             | _ -> default_iterator.case self c)
          | _ -> default_iterator.case self c)
    }
  in
  fun f ->
    try
      f it;
      true
    with
    | Found -> false
;;
