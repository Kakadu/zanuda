module Ast_pattern0 = struct
  exception Expected of Location.t * string

  let fail loc expected = raise (Expected (loc, expected))

  type context =
    { (* [matched] counts how many constructors have been matched. This is used to find what
       pattern matches the most some piece of ast in [Ast_pattern.alt]. In the case where
       all branches fail to match, we report the error from the one that matches the
       most.
       This is only incremented by combinators that can fail. *)
      mutable matched : int
    }

  type ('matched_value, 'k, 'k_result) t =
    | T of (context -> Location.t -> 'matched_value -> 'k -> 'k_result)

  (* end of copy-paste from https://github.com/ocaml-ppx/ppxlib/blob/0.22.2/src/ast_pattern0.ml *)
  (* TODO: deal with licencing issues *)
end

open Format
open Location
open Base
open Ast_pattern0

type ('a, 'b, 'c) t = ('a, 'b, 'c) Ast_pattern0.t

let save_context ctx = ctx.matched
let restore_context ctx backup = ctx.matched <- backup
let incr_matched c = c.matched <- c.matched + 1

let parse (T f) loc ?on_error x k =
  try f { matched = 0 } loc x k with
  | Expected (loc, expected) ->
    (match on_error with
    | None -> Location.raise_errorf ~loc "%s expected" expected
    | Some f -> f expected)
;;

module Packed = struct
  type ('a, 'b) t = T : ('a, 'b, 'c) Ast_pattern0.t * 'b -> ('a, 'c) t

  let create t f = T (t, f)
  let parse (T (t, f)) loc x = parse t loc x f
end

let __ =
  T
    (fun ctx _loc x k ->
      incr_matched ctx;
      k x)
;;

let __' =
  T
    (fun ctx loc x k ->
      incr_matched ctx;
      k { loc; txt = x })
;;

let drop =
  T
    (fun ctx _loc _ k ->
      incr_matched ctx;
      k)
;;

let cst ~to_string ?(equal = Poly.equal) v =
  T
    (fun ctx loc x k ->
      if equal x v
      then (
        incr_matched ctx;
        (* printf "cst succeeded for %s\n%!" (to_string v); *)
        k)
      else fail loc (to_string v))
;;

let int v = cst ~to_string:Int.to_string v
let char v = cst ~to_string:(Printf.sprintf "%C") v
let string v = cst ~to_string:(Printf.sprintf "%S") v
let float v = cst ~to_string:Float.to_string v
let int32 v = cst ~to_string:Int32.to_string v
let int64 v = cst ~to_string:Int64.to_string v
let nativeint v = cst ~to_string:Nativeint.to_string v
let bool v = cst ~to_string:Bool.to_string v

let false_ =
  T
    (fun ctx loc x k ->
      match x with
      | false ->
        ctx.matched <- ctx.matched + 1;
        k
      | _ -> fail loc "false")
;;

let true_ =
  T
    (fun ctx loc x k ->
      match x with
      | true ->
        ctx.matched <- ctx.matched + 1;
        k
      | _ -> fail loc "true")
;;

let nil =
  T
    (fun ctx loc x k ->
      match x with
      | [] ->
        ctx.matched <- ctx.matched + 1;
        k
      | _ -> fail loc "[]")
;;

let ( ^:: ) (T f0) (T f1) =
  T
    (fun ctx loc x k ->
      match x with
      | x0 :: x1 ->
        ctx.matched <- ctx.matched + 1;
        let k = f0 ctx loc x0 k in
        let k = f1 ctx loc x1 k in
        k
      | _ -> fail loc "::")
;;

let none =
  T
    (fun ctx loc x k ->
      match x with
      | None ->
        ctx.matched <- ctx.matched + 1;
        k
      | _ -> fail loc "None")
;;

let some (T f0) =
  T
    (fun ctx loc x k ->
      match x with
      | Some x0 ->
        ctx.matched <- ctx.matched + 1;
        let k = f0 ctx loc x0 k in
        k
      | _ -> fail loc "Some")
;;

let pair (T f1) (T f2) =
  T
    (fun ctx loc (x1, x2) k ->
      let k = f1 ctx loc x1 k in
      let k = f2 ctx loc x2 k in
      k)
;;

let ( ** ) = pair

let triple (T f1) (T f2) (T f3) =
  T
    (fun ctx loc (x1, x2, x3) k ->
      let k = f1 ctx loc x1 k in
      let k = f2 ctx loc x2 k in
      let k = f3 ctx loc x3 k in
      k)
;;

let alt (T f1) (T f2) =
  T
    (fun ctx loc x k ->
      let backup = save_context ctx in
      try f1 ctx loc x k with
      | e1 ->
        let m1 = save_context ctx in
        restore_context ctx backup;
        (try f2 ctx loc x k with
        | e2 ->
          let m2 = save_context ctx in
          if m1 >= m2
          then (
            restore_context ctx m1;
            raise e1)
          else raise e2))
;;

let ( ||| ) = alt
let map (T func) ~f = T (fun ctx loc x k -> func ctx loc x (f k))
let map' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (f loc k))
let map_result (T func) ~f = T (fun ctx loc x k -> f (func ctx loc x k))
let ( >>| ) t f = map t ~f
let map0 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (k f))
let map1 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a -> k (f a)))
let map2 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a b -> k (f a b)))
let map0' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (k (f loc)))
let map1' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a -> k (f loc a)))
let map2' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a b -> k (f loc a b)))
let alt_option some none = alt (map1 some ~f:(fun x -> Some x)) (map0 none ~f:None)

let many (T f) =
  T (fun ctx loc l k -> k (List.map l ~f:(fun x -> f ctx loc x (fun x -> x))))
;;

let loc (T f) = T (fun ctx _loc (x : _ Ppxlib.Loc.t) k -> f ctx x.loc x.txt k)
let pack0 t = map t ~f:(fun f -> f ())
let pack2 t = map t ~f:(fun f x y -> f (x, y))
let pack3 t = map t ~f:(fun f x y z -> f (x, y, z))

(* end of copy-paste from https://github.com/ocaml-ppx/ppxlib/blob/0.22.2/src/ast_pattern.ml *)
(* TODO: deal with licencing issues *)

let path xs =
  let rec helper ps ctx loc x k =
    let cmp_names l r =
      let ans = String.equal l r in
      (* printf "\t\tCompare names %s and %s:  %b\n%!" l r ans; *)
      ans
    in
    match x, ps with
    | Path.Pident id, [ id0 ] ->
      if cmp_names (Ident.name id) id0
      then (
        let () = ctx.matched <- ctx.matched + 1 in
        k)
      else fail loc "path"
    | Path.Pdot (next, id), id0 :: ids when cmp_names id id0 -> helper ids ctx loc next k
    | Path.Papply _, _ -> fail loc "path got Papply"
    | _ -> fail loc (sprintf "path %s" (String.concat ~sep:"." xs))
  in
  T (helper (List.rev xs))
;;

let rec path_of_list xs =
  match xs with
  | [] -> failwith "Bad argument: path_of_list"
  | s :: tl ->
    List.fold_left
      tl
      ~init:(Path.Pident (Ident.create_local s))
      ~f:(fun acc x -> Path.Pdot (acc, x))
;;

include struct
  let names = [ "Stdlib!"; "List"; "length" ]

  (* let () = Format.printf "%a" Path.print (path_of_list names) *)
  let%test _ =
    String.equal "Stdlib!/3.List.length" (asprintf "%a" Path.print (path_of_list names))
  ;;

  let%test _ =
    let noloc =
      Warnings.
        { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true }
    in
    parse
      (path names)
      noloc
      ~on_error:(fun _ ->
        print_endline "some error";
        false)
      (path_of_list names)
      true
  ;;
end

include struct
  open Typedtree

  let eint (T f0) =
    T
      (fun ctx loc x k ->
        match x.exp_desc with
        | Texp_constant (Asttypes.Const_int n) ->
          ctx.matched <- ctx.matched + 1;
          f0 ctx loc n k
        | _ -> fail loc (sprintf "eint"))
  ;;

  let texp_ident (T path0) =
    T
      (fun ctx loc x k ->
        match x.exp_desc with
        | Texp_ident (path, _, _) ->
          ctx.matched <- ctx.matched + 1;
          path0 ctx loc path k
        | _ -> fail loc "texp_ident")
  ;;

  let texp_apply (T f0) (T args0) =
    T
      (fun ctx loc x k ->
        match x.exp_desc with
        | Texp_apply (f, args) ->
          ctx.matched <- ctx.matched + 1;
          k |> f0 ctx loc f |> args0 ctx loc args
        | _ -> fail loc "texp_apply")
  ;;

  let nolabel =
    T
      (fun ctx loc x k ->
        match x with
        | Asttypes.Nolabel ->
          ctx.matched <- ctx.matched + 1;
          k
        | _ -> fail loc "nolabel")
  ;;

  let texp_apply1 f x = texp_apply f ((nolabel ** some x) ^:: nil)

  let texp_apply2 f x y =
    texp_apply f ((nolabel ** some x) ^:: (nolabel ** some y) ^:: nil)
  ;;
end
