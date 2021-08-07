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

val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

open Typedtree

val int : int -> (int, 'a, 'a) t
val path : string list -> (Path.t, 'a, 'a) t
val eint : (int, 'a, 'b) t -> (expression, 'a, 'b) t
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
