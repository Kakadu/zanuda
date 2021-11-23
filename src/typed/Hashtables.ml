open Base
open Zanuda_core
open Zanuda_core.Utils
open Format

type input = Tast_iterator.iterator

let lint_source = LINT.FPCourse
let lint_id = "mutable_hashtables"

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~docs:
      {|
### What it does
Using mutable data structures for teaching purposes is usually discouraged. You
     should try to use standart tree-like maps. If you really want to use hashing
     consider Hash-Array Mapped Tries (HAMT)


##### TODO
Write down cases when mutation is really required.
|}
;;

let msg ppf () =
  Caml.Format.fprintf
    ppf
    "Using mutable data structures for teaching purposes is usually discouraged. You \
     should try to use standart tree-like maps. If you really want to use hashing \
     consider Hash-Array Mapped Tries (HAMT)%!"
;;

let report filename ~loc kind =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg kind

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        kind
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  let pat =
    let open Tast_pattern in
    let typ_hashtbl =
      typ_constr (path [ "Stdlib"; "Hashtbl"; "t" ]) (drop ^:: drop ^:: nil)
    in
    let typ_hashtbl_base =
      typ_constr (path [ "Base"; "Hashtbl"; "t" ]) (drop ^:: drop ^:: nil)
    in
    texp_ident_typ drop (typ_hashtbl ||| typ_hashtbl_base)
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        let __ _ =
          if String.is_substring loc.loc_start.pos_fname ~substring:"Hashtables"
          then
            Format.printf
              "%a\n%!"
              Pprintast.expression
              Untypeast.(default_mapper.expr default_mapper expr)
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
