open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "exc_failwith"
let group = LINT.Suspicious
let level = LINT.Allow
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~group
    ~level
    ~docs:
      {|
### What it does
The usage of 'Stdlib.failwith' in production code could be error-prone. The constructions `failwith "not implemented"` should be implemented sooner or later.

Constructions `failwith "should not happen"` smells. Maybe techniques from https://doi.org/10.1145/3299711.3242755 could help.
|}
;;

let msg ppf () =
  Caml.Format.fprintf
    ppf
    "Using `failwith` (or `assert false`) usually is a clue that a corner case is not \
     being handled properly. To report errors we recommend using error monad instead. In \
     princliple, these construction are OK for temporary work-in-progress code, but in \
     release they should be eliminated%!"
;;

let report filename ~loc =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg ()

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        ()
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_ident (path [ "Stdlib"; "failwith" ])
    ||| texp_ident (path [ "Base"; "failwith" ])
    ||| texp_assert_false ()
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (* if String.is_substring loc.loc_start.pos_fname ~substring:"Failwith"
        then (
          let u = Untypeast.(default_mapper.expr default_mapper expr) in
          Format.printf "%a\n%a\n%!" Pprintast.expression u (Printast.expression 0) u); *)
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun () ->
            CollectedLints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc))
          ();
        fallback.expr self expr)
  }
;;
