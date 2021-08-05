open Base

(* open Parsetree *)
open Location
module ErrorFormat = UntypedLints.ErrorFormat
module RDJsonl = UntypedLints.RDJsonl

module L1 : LINT.TYPED = struct
  type input = Tast_iterator.iterator

  let describe_itself _ = `String "none"

  let msg ppf () =
    Format.fprintf ppf "Bad measurement of a list (with non-negative size)%!"
  ;;

  let report_md ~loc:_ ppf = msg ppf ()

  let report_txt ~loc ppf =
    (* Format.fprintf ppf "%a\n" msg () *)
    let main = Location.mkloc (fun ppf -> msg ppf ()) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let report ~loc =
    let module M = struct
      let md ppf () = report_md ~loc ppf
      let txt ppf () = report_txt ~loc ppf

      let golint ppf () =
        ErrorFormat.pp
          ppf
          ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
          ~line:loc.loc_start.pos_lnum (* loc.loc_start.pos_cnum *)
          ~col:0
          msg
          ()
      ;;

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
      let ops = [ ">", 0; "<", 0; ">=", 0; "<=", 0 ] in
      (* TODO: flippable operators *)
      let single (op, n) =
        Tast_pattern.(
          texp_apply2
            (texp_ident (path [ "Stdlib"; op ]))
            (texp_apply1 (texp_ident (path [ "Stdlib"; "List"; "length" ])) __)
            (eint @@ int n))
      in
      List.fold
        ~init:(single @@ List.hd_exn ops)
        (List.tl_exn ops)
        ~f:(fun acc x -> acc ||| single x)
    in
    let open Tast_iterator in
    { fallback with
      expr =
        (fun self expr ->
          (* Format.printf
            "%a\n%!"
            Pprintast.expression
            Untypeast.(default_mapper.expr default_mapper expr); *)
          let open Typedtree in
          let loc = expr.exp_loc in
          Tast_pattern.parse
            pat
            loc
            ~on_error:(fun () -> ())
            expr
            (fun _list_to_be_measured -> CollectedLints.add ~loc (report ~loc));
          fallback.expr self expr)
    }
  ;;
end
