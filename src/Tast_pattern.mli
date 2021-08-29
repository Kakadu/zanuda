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

val as__ : ('a, 'b, 'c) t -> ('a, 'a -> 'b, 'c) t

(** Pattern that captures its input. *)
val __ : ('a, 'a -> 'b, 'b) t

(** Pattern that captures its input with location. *)
val __' : ('a, 'a Location.loc -> 'b, 'b) t

val drop : ('a, 'b, 'b) t
val nil : ('a list, 'b, 'b) t
val ( ^:: ) : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t
val none : ('a option, 'b, 'b) t
val some : ('a, 'b, 'c) t -> ('a option, 'b, 'c) t
val pair : ('a, 'b, 'c) t -> ('d, 'c, 'e) t -> ('a * 'd, 'b, 'e) t
val ( ** ) : ('a, 'b, 'c) t -> ('d, 'c, 'e) t -> ('a * 'd, 'b, 'e) t
val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
val loc : ('a, 'b, 'c) t -> ('a Location.loc, 'b, 'c) t
val ( >>| ) : ('a, 'b, 'c) t -> ('d -> 'b) -> ('a, 'd, 'c) t

(** Mapping results of applying pattern-combinator *)

val map0 : ('a, 'b, 'c) t -> f:'d -> ('a, 'd -> 'b, 'c) t
val map1 : ('a, 'b -> 'c, 'd) t -> f:('b -> 'e) -> ('a, 'e -> 'c, 'd) t
val map2 : ('a, 'b -> 'c -> 'd, 'e) t -> f:('b -> 'c -> 'f) -> ('a, 'f -> 'd, 'e) t

val map3
  :  ('a, 'b -> 'c -> 'd -> 'e, 'f) t
  -> f:('b -> 'c -> 'd -> 'g)
  -> ('a, 'g -> 'e, 'f) t

val map4
  :  ('a, 'b -> 'c -> 'd -> 'e -> 'f, 'g) t
  -> f:('b -> 'c -> 'd -> 'e -> 'h)
  -> ('a, 'h -> 'f, 'g) t

val map5
  :  ('a, 'b -> 'c -> 'd -> 'e -> 'f -> 'g, 'h) t
  -> f:('b -> 'c -> 'd -> 'e -> 'f -> 'i)
  -> ('a, 'i -> 'g, 'h) t

open Typedtree

val int : int -> (int, 'a, 'a) t
val lident : (string, 'a, 'b) t -> (Longident.t, 'a, 'b) t
val path : string list -> (Path.t, 'a, 'a) t
val path_pident : (Ident.t, 'a, 'b) t -> (Path.t, 'a, 'b) t
val eint : (int, 'a, 'b) t -> (expression, 'a, 'b) t
val ebool : (expression, bool -> 'a, 'a) t

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

val nolabel : (Asttypes.arg_label, 'a, 'a) t
val tpat_var : (string, 'a, 'b) t -> (pattern, 'a, 'b) t
val tpat_exception : (value_pat, 'a, 'b) t -> (comp_pat, 'a, 'b) t
val tpat_any : (value_pat, 'a, 'a) t
val texp_ident : (Path.t, 'a, 'b) t -> (expression, 'a, 'b) t

val texp_ident_typ
  :  (Path.t, 'a, 'b) t
  -> (Types.type_expr, 'b, 'c) t
  -> (expression, 'a, 'c) t

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

val texp_apply_nolabelled
  :  (expression, 'a, 'b) t
  -> (expression list, 'b, 'c) t
  -> (expression, 'a, 'c) t

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

val texp_ite
  :  (expression, 'a, 'b) t
  -> (expression, 'b, 'c) t
  -> (expression option, 'c, 'd) t
  -> (expression, 'a, 'd) t

val texp_try
  :  (expression, 'a, 'b) t
  -> (case_val list, 'b, 'c) t
  -> (expression, 'a, 'c) t

val texp_record
  :  (expression option, 'a, 'b) t
  -> ((Types.label_description * record_label_definition) array, 'b, 'c) t
  -> (expression, 'a, 'c) t

val texp_field
  :  (expression, 'a, 'b) t
  -> (Types.label_description, 'b, 'c) t
  -> (expression, 'a, 'c) t

val label_desc : (string, 'a, 'b) t -> (Types.label_description, 'a, 'b) t
val rld_kept : (record_label_definition, 'a, 'a) t

val rld_overriden
  :  (Longident.t, 'a, 'b) t
  -> (expression, 'b, 'c) t
  -> (record_label_definition, 'a, 'c) t

val typ_constr
  :  (Path.t, 'a, 'b) t
  -> (Types.type_expr list, 'b, 'c) t
  -> (Types.type_expr, 'a, 'c) t
