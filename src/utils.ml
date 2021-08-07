open Base
open Format

let printfn fmt = Format.kfprintf (fun ppf -> Format.fprintf ppf "\n%!") std_formatter fmt

module ErrorFormat = struct
  let pp ppf ~filename ~line ~col:_ msg x =
    Format.fprintf ppf "%s:%d:%d:%a\n%!" filename line (* col *) 0 msg x
  ;;
end

module RDJsonl = struct
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
        | Some (desc, url) ->
          [ "code", `Assoc [ "value", `String desc; "url", `String url ] ])
    in
    Format.fprintf ppf "%s\n%!" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

let cut_build_dir () =
  let prefix = "_build/default/" in
  if String.is_prefix ~prefix !Location.input_name
  then
    Location.input_name := String.drop_prefix !Location.input_name (String.length prefix)
;;
