(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type input = Tast_iterator.iterator

let lint_id = "list_length_comparisons"
let lint_source = LINT.FPCourse
let level = LINT.Warn

let documentation =
  {|
### What it does
The function `Stdlib.List.length` evaluated length of standart OCaml linked lists (`'a list`). There return values supposed to be non-negative, so all code like `List.length .. <= 0` smells bad. If they need to check that list is empty it is more recommended to use pattern matching instead of calculating length, because for large list we will do full iteration, and it will not be too efficient.
  |}
  |> Stdlib.String.trim
;;

let describe_as_json () = describe_as_clippy_json lint_id ~docs:documentation

let msg ppf () =
  Caml.Format.fprintf ppf "Bad measurement of a list (with non-negative size)\n%!"
;;

let msg_long ppf (l, r) =
  msg ppf ();
  Caml.Format.fprintf
    ppf
    "Between '%a' and '%a'.%!"
    Pprintast.expression
    (MyUntype.expr l)
    Pprintast.expression
    (MyUntype.expr r)
;;

let report filename ~loc l r =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg_long (l, r)

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        (fun _ () -> ())
        ()
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

open Typedtree
open Tast_pattern

let pat_list_length () : (expression, 'a, 'a) Tast_pattern.t =
  texp_ident (path [ "Stdlib"; "List"; "length" ])
  ||| texp_ident (path [ "Stdlib!"; "List"; "length" ])
  ||| texp_ident (path [ "Base"; "List"; "length" ])
  ||| texp_ident (path [ "Base!"; "List"; "length" ])
;;

let make_pat_op : 'a. string -> (expression, 'a, 'a) t =
 fun op ->
  texp_ident (path [ "Stdlib"; op ])
  ||| texp_ident (path [ "Stdlib!"; op ])
  ||| texp_ident (path [ "Base!"; op ])
  ||| texp_ident (path [ "Base"; op ])
;;

let pat
  : ( Typedtree.expression, Typedtree.expression -> Typedtree.expression -> 'a, 'a )
  Tast_pattern.t
  =
  let open Tast_pattern in
  let ops = [ ">=", "<=", 0; "<=", ">=", 0; ">", "<", 0; "=", "=", 0 ] in
  let single (op, dualop, n) =
    let open Tast_pattern in
    (* TODO: understand difference between Stdlib and Stdlib! *)
    let u () =
      let f () = make_pat_op op in
      let len () = as__ (texp_apply1 (pat_list_length ()) drop) in
      texp_apply2 (f ()) (len ()) (as__ (eint (int n)))
    in
    let v () =
      let f () = make_pat_op dualop in
      let len () = as__ (texp_apply1 (pat_list_length ()) drop) in
      texp_apply2 (f ()) (as__ (eint (int n))) (len ())
    in
    u () ||| v ()
  in
  List.fold_left
    (List.tl_exn ops)
    ~init:(single @@ List.hd_exn ops)
    ~f:(fun acc x -> acc ||| single x)
;;

(* let%test _ =
  Tast_pattern.parse
    pat
    Location.none
    ~on_error:(fun _ -> true)
    [%expr List.length xs = List.length ys]
    (fun _ () -> true)
;; *)

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          expr
          (fun e1 e2 () ->
            let __ _ =
              Format.printf
                "FUCK '%a' and '%a'\n%!"
                MyPrinttyped.expr
                e1
                MyPrinttyped.expr
                e2
            in
            CollectedLints.add
              ~loc
              (report loc.Location.loc_start.Lexing.pos_fname ~loc e1 e2))
          ~on_error:(fun _desc () -> fallback.expr self expr)
          ())
  }
;;
