type ('a, 'b, 'c) t

(** Matches a value against a pattern. *)
val parse : ('a, 'b, 'c) t -> Location.t -> ?on_error:(string -> 'c) -> 'a -> 'b -> 'c

module Packed : sig
  type ('a, 'b, 'c) pattern = ('a, 'b, 'c) t
  type ('a, 'b) t

  val create : ('a, 'b, 'c) pattern -> 'b -> ('a, 'c) t
  val parse : ('a, 'b) t -> Location.t -> 'a -> 'b
end
with type ('a, 'b, 'c) pattern := ('a, 'b, 'c) t

(** Pattern that captures its input. *)
val __ : ('a, 'a -> 'b, 'b) t

(** Pattern that captures its input with location. *)
val __' : ('a, 'a Location.loc -> 'b, 'b) t

val drop : ('a, 'b, 'b) t
val nil : ('a list, 'b, 'b) t
val ( ^:: ) : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t
val none : ('a option, 'b, 'b) t
val some : ('a, 'b, 'c) t -> ('a option, 'b, 'c) t
val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
val loc : ('a, 'b, 'c) t -> ('a Location.loc, 'b, 'c) t

open Typedtree

val int : int -> (int, 'a, 'a) t
val path : string list -> (Path.t, 'a, 'a) t
val eint : (int, 'a, 'b) t -> (expression, 'a, 'b) t

[%%if ocaml_version < (4, 11, 0)]

type case_val = Typedtree.case
type case_comp = Typedtree.case
type value_pat = pattern
type comp_pat = pattern

[%%else]

type case_val = value case
type case_comp = computation case
type value_pat = value pattern_desc pattern_data
type comp_pat = computation pattern_desc pattern_data

[%%endif]

val tpat_var : (string, 'a, 'b) t -> (pattern, 'a, 'b) t
val tpat_exception : (value_pat, 'a, 'b) t -> (comp_pat, 'a, 'b) t
val tpat_any : (value_pat, 'a, 'a) t
val texp_ident : (Path.t, 'a, 'b) t -> (expression, 'a, 'b) t

val texp_apply
  :  (expression, 'a, 'b) t
  -> ((Asttypes.arg_label * expression option) list, 'b, 'c) t
  -> (expression, 'a, 'c) t

val texp_apply1
  :  (expression, 'a, 'b) t
  -> (expression, 'b, 'c) t
  -> (expression, 'a, 'c) t

val texp_apply2
  :  (expression, 'a, 'b) t
  -> (expression, 'b, 'c) t
  -> (expression, 'c, 'd) t
  -> (expression, 'a, 'd) t

val texp_function : (case_val list, 'a, 'b) t -> (expression, 'a, 'b) t

val case
  :  (pattern, 'a, 'b) t
  -> (expression option, 'b, 'c) t
  -> (expression, 'c, 'd) t
  -> (case_val, 'a, 'd) t

val texp_match
  :  (expression, 'a, 'b) t
  -> (case_comp list, 'b, 'c) t
  -> (expression, 'a, 'c) t

val texp_try
  :  (expression, 'a, 'b) t
  -> (value case list, 'b, 'c) t
  -> (expression, 'a, 'c) t
