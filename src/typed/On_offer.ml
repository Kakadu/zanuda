[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Zanuda_core
open Zanuda_core.Utils
open Tast_pattern

type on_off =
  | On
  | Off

type stack_item = (string, on_off) Hashtbl.t

let stack : stack_item Stack.t = Stack.create ()

[@@@coverage off]

let log ppf =
  Format.kasprintf
    (fun s ->
      print_endline s;
      flush stdout)
    ppf
;;

let trace_stack_top () =
  match Stdlib.Stack.top_opt stack with
  | None -> Format.printf "EMPTY STACK\n%!"
  | Some h ->
    let xs = Hashtbl.to_seq h |> List.of_seq in
    let open Format in
    Format.printf
      "[ %a ]\n%!"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf "; ")
         (fun ppf -> function
           | s, On -> fprintf ppf "+%s" s
           | s, Off -> fprintf ppf "-%s" s))
      xs
;;

[@@@coverage on]

let push_stack () =
  (* log "push stack"; *)
  Stack.push (Hashtbl.create 52) stack
;;

let pop_stack () =
  (* log "pop stack"; *)
  let _ = Stack.pop stack in
  ()
;;

(** Not reentrant *)
let on_spell payload =
  assert (not (Stack.is_empty stack));
  let xs = String.split_on_char ',' payload |> List.map String.trim in
  let hash = Stack.top stack in
  let disable_via_hash hash key =
    match Hashtbl.find hash key with
    | exception Not_found -> ()
    | On -> Hashtbl.replace hash key Off
    | Off -> ()
  in
  let enable_via_hash hash key =
    match Hashtbl.find hash key with
    | exception Not_found -> Hashtbl.add hash key On
    | On -> ()
    | Off -> Hashtbl.replace hash key On
  in
  let cut1 s = StringLabels.sub s ~pos:1 ~len:(String.length s - 1) in
  let enable s =
    enable_via_hash hash s;
    Config.enable s
  in
  let disable s =
    disable_via_hash hash s;
    Config.disable s
  in
  List.iter
    (fun s ->
      if String.starts_with ~prefix:"+" s
      then enable (cut1 s)
      else if String.starts_with ~prefix:"-" s
      then disable (cut1 s)
      else Format.eprintf "Bad lint specification: '%s'\n%!" payload)
    xs;
  ()
;;

module First = struct
  type input = Tast_iterator.iterator

  let lint_id = "on_offer_prologue"
  let level = LINT.Warn
  let lint_source = LINT.Other

  let documentation =
    String.trim
      {|
  This Technical lint helper manages state that allows to disable and enable lints in the module scope.
  |}
  ;;

  (* TODO: level and source should not be defined for technical lints, but it should be fixes later. *)

  let describe_as_json () =
    describe_as_clippy_json lint_id ~group:LINT.Style ~level ~docs:documentation
  ;;

  let run _ fallback =
    let _ : Tast_iterator.iterator = fallback in
    { fallback with
      structure_item =
        (fun self si ->
          let open Typedtree in
          let () =
            Tast_pattern.(parse (tstr_zanuda_attr __))
              si.str_loc
              si
              (fun s () -> on_spell s)
              ~on_error:(fun _ () -> ())
              ()
          in
          fallback.structure_item self si)
    ; structure =
        (fun self stru ->
          push_stack ();
          fallback.structure self stru)
    }
  ;;
end

module Last = struct
  type input = Tast_iterator.iterator

  let lint_id = "on_offer_epilogue"
  let level = LINT.Warn
  let lint_source = LINT.FPCourse
  let documentation = First.documentation

  let describe_as_json () =
    describe_as_clippy_json lint_id ~group:LINT.Style ~level ~docs:documentation
  ;;

  let run _ fallback =
    let _ : Tast_iterator.iterator = fallback in
    { fallback with
      structure =
        (fun self si ->
          (* Because of on_offer.Last should be last iterator,
            it is important to run fallback before removing from the stack.
          Otherwise the useful information will be removed too early *)
          fallback.structure self si;
          pop_stack ())
    }
  ;;
end
