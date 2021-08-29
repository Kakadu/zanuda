open Base
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "exc_error_swallowing"
let group = LINT.Suspicious
let level = LINT.Deny
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~group
    ~level
    ~docs:
      {|
### What it does
Catching all possible exceptions with wildcard considered as antipattern

See also https://en.wikipedia.org/wiki/Error_hiding
|}
;;

let msg ppf () = Caml.Format.fprintf ppf "Antipattern: error swallowing%!"

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
    texp_try drop (case tpat_any none __' ^:: nil)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        (* TODO: support exceptions during matching *)
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun { loc } () ->
            (* Reported location is a location of whole match and not of pattern
              TODO: understand how to fix it  *)
            (* Format.printf "%a\n%!" Location.print_loc loc; *)
            CollectedLints.add ~loc (report loc.Location.loc_start.Lexing.pos_fname ~loc))
          ();
        fallback.expr self expr)
  }
;;
