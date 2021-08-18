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

val drop : ('a, 'b, 'b) t
val nil : ('a list, 'b, 'b) t
val ( ^:: ) : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t
val none : ('a option, 'b, 'b) t
val some : ('a, 'b, 'c) t -> ('a option, 'b, 'c) t
val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

open Typedtree

val int : int -> (int, 'a, 'a) t
val path : string list -> (Path.t, 'a, 'a) t
val eint : (int, 'a, 'b) t -> (expression, 'a, 'b) t
val tpat_var : (string, 'a, 'b) t -> (pattern, 'a, 'b) t
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

[%%if ocaml_version < (4, 12, 0)]

type case1 = Typedtree.case

[%%else]

type case1 = value case

[%%endif]

val texp_function : (case1 list, 'a, 'b) t -> (expression, 'a, 'b) t

val case
  :  (pattern, 'a, 'b) t
  -> (expression option, 'b, 'c) t
  -> (expression, 'c, 'd) t
  -> (case1, 'a, 'd) t

val texp_match
  :  (expression, 'a, 'b) t
  -> (case1 list, 'b, 'c) t
  -> (expression, 'a, 'c) t
