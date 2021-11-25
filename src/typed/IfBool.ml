open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let lint_id = "if_bool"
let group = LINT.Style
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

Checks funny uses of if-then-else expresssion, like 'if true ...', 'if ... then false', etc.

|}
;;

let msg ppf s = Caml.Format.fprintf ppf "%s\n%!" s

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
    texp_ite ebool drop drop
    |> map1 ~f:(Format.asprintf "Executing 'if %b' smells bad")
    ||| (texp_ite drop ebool drop
        |> map1 ~f:(Format.asprintf "Executing 'if ... then %b' smells bad"))
    ||| (texp_ite drop drop (some ebool)
        |> map1 ~f:(Format.asprintf "Executing 'if ... then .. else %b' smells bad"))
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun s () ->
            CollectedLints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc s))
          ();
        fallback.expr self expr)
  }
;;
