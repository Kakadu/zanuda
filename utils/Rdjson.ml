open Location

type severity =
  | Unknown
  | Error
  | Warning
  | Info

let header ppf =
  Printexc.print_backtrace stdout;
  Format.fprintf
    ppf
    "%s\n%!"
    {|
  {
    "source": {
      "name": "super lint",
      "url": "https://example.com/url/to/super-lint"
    },
    "severity": "WARNING",
    "diagnostics": [
  |}
;;

let footer ppf = Format.fprintf ppf "\n]}"

let diagnostic ~loc msg =
  `Assoc
    [ "message", `String msg
    ; ( "location"
      , `Assoc
          [ "path", `String loc.loc_start.Lexing.pos_fname
          ; ( "range"
            , `Assoc
                [ ( "start"
                  , `Assoc
                      [ "line", `Int loc.loc_start.Lexing.pos_lnum
                      ; "column", `Int loc.loc_start.Lexing.pos_cnum
                      ] )
                ] )
          ] )
    ; "severity", `String "ERROR"
    ; "code", `Assoc [ "value", `String "RULE1" ]
    ]
;;
