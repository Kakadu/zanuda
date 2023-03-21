open Base
module Format = Caml.Format
open Format
open Zanuda_core
open Utils

let is_camel_case s = String.(lowercase s <> s)
let lint_id = "camel_extra_dollar"
let lint_source = LINT.FPCourse

let documentation =
  {|
### What it does
The `@@` operator is used for writing less parentheses in expression. Code like `f (g (h x))` could be rewritten as
`f @@ g (h x)`. But is some cases it is not required, like `print_int @@ 1`.
Some of these cases are reported by this lint.

  |}
  |> Stdlib.String.trim
;;

let describe_as_json () =
  describe_as_clippy_json lint_id ~impl:LINT.Untyped ~docs:documentation
;;

type input = Ast_iterator.iterator

let msg ppf () = fprintf ppf "Extranous `@@@@`."

let report ~loc ~filename info =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg info

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        info
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let open Ast_iterator in
  { fallback with
    expr =
      (fun self e ->
        let open Ppxlib.Ast_pattern in
        let pm =
          pexp_apply
            (pexp_ident (lident (string "@@")))
            ((nolabel ** pexp_ident __) ^:: (nolabel ** pexp_record __ __) ^:: nil)
        in
        let loc = e.pexp_loc in
        let filename = loc.Location.loc_start.Lexing.pos_fname in
        let () =
          try
            parse
              pm
              Location.none
              e
              ~on_error:(fun _ -> ())
              (fun _ _ _ -> CollectedLints.add ~loc (report ~loc ~filename ()))
          with
          | Location.Error e -> Format.printf "%a\n%!" Location.print_report e
        in
        fallback.expr self e)
  }
;;
