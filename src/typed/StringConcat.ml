open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "string_concat"
let group = LINT.Perf
let level = LINT.Warn
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~group
    ~level
    ~docs:
      {|
### What it does
Concatenating multiple strings at once (`a^b^c`) has a perfomance issue. OCaml needs to allocate memory for the result of
`a^b` and after that it needs to allocate memory for a result of concatenation `a^b` and `c`, i.e. it allocates unneeded memory for intermediate results. (The same issue arises in left-associative concatenation of lists).


### How to fix
Use [function](https://github.com/ocaml/ocaml/blob/4.14/stdlib/string.ml#L72) `val concat: string -> string list -> string` from standard library.

Or rewrite using printf: `Format.printf "%s%s%s" a b c`.
|}
;;

let msg ppf () =
  Caml.Format.fprintf
    ppf
    "Concatenating multiple strings at once (`a^b^c`) has a perfomance issue.\n%!"
;;

let report filename ~loc e =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg e

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        e
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    let op =
      let typ_str = typ_constr (path [ "Stdlib"; "string" ] ||| path [ "string" ]) nil in
      texp_ident_typ
        (path [ "Stdlib"; "^" ] ||| path [ "Stdlib!"; "^" ])
        (typ_arrow typ_str (typ_arrow typ_str typ_str))
    in
    texp_apply2 op drop (texp_apply2 op drop drop)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        let __ _ =
          if String.is_substring loc.loc_start.pos_fname ~substring:"StringConcat"
          then (
            let u = Untypeast.(default_mapper.expr default_mapper expr) in
            Format.printf
              "%a\n%a\n%a\n%!"
              Pprintast.expression
              u
              (Printast.expression 0)
              u
              MyPrinttyped.expr
              expr)
        in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun () ->
            CollectedLints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc ()))
          ();
        fallback.expr self expr)
  }
;;
