type group =
  | Style
  | Correctness
  | Perf
  | Restriction
  | Deprecated
  | Pedantic
  | Complexity
  | Suspicious
  | Nursery

type level =
  | Allow
  | Warn
  | Deny
  | Deprecated

type impl =
  | Typed
  | Untyped

type lint_source =
  | Camelot
  | Clippy
  | FPCourse
  | Other

module type GENERAL = sig
  type input

  val lint_id : string
  val lint_source : lint_source
  val run : Compile_common.info -> input -> input
end

(* In this design we can't define a linter that processes both parsetree and typedtree. Is it important? *)

module type UNTYPED = sig
  type input = Tast_iterator.iterator

  include GENERAL with type input := input

  val describe_itself : unit -> Yojson.Safe.t
end

module type TYPED = sig
  type input = Tast_iterator.iterator

  include GENERAL with type input := input

  val describe_itself : unit -> Yojson.Safe.t
end

module type REPORTER = sig
  val txt : Format.formatter -> unit -> unit
  val rdjsonl : Format.formatter -> unit -> unit
end
