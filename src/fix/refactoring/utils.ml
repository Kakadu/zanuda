[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Location
open Typedtree
open Replacement.Refill

type end_point =
  | Start
  | End

type point =
  { loc : Location.t
  ; pos : end_point
  }

let position { loc; pos } =
  match pos with
  | Start -> loc.loc_start
  | End -> loc.loc_end
;;

let make_point loc pos = { loc; pos }
let pat_point p point = { loc = p.pat_loc; pos = point }
let exp_point e point = { loc = e.exp_loc; pos = point }
let exp_start e = exp_point e Start
let exp_end e = exp_point e End

let gen_loc spoint epoint =
  { loc_start = position spoint; loc_end = position epoint; loc_ghost = false }
;;

let fname loc = loc.loc_start.pos_fname
let set_payload ({ location; _ } as r) = add (fname location) r

let set_padding p p' payload =
  let location = gen_loc p p' in
  set_payload { location; payload }
;;

let set_empty_padding p p' = set_padding p p' Void

open Lexing

let shift_point_cnum { loc; pos } offset =
  match pos with
  | Start ->
    { loc =
        { loc with
          loc_start = { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + offset }
        }
    ; pos
    }
  | End ->
    { loc =
        { loc with
          loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + offset }
        }
    ; pos
    }
;;
