module type GENERAL = sig
  type input

  val run : Compile_common.info -> input -> input
end

(* In this design we can't define a linter that processes both parsetree and typedtree. Is it important? *)

module type UNTYPED = sig
  type input = Ast_iterator.iterator

  include GENERAL with type input := input
  (* val xxx : (GENERAL with type input = Ast_iterator.iterator).run *)

  (* val run : Compile_common.info -> Ast_iterator.iterator -> Ast_iterator.iterator *)
  val describe_itself : unit -> Yojson.Safe.t
end

module type TYPED = sig
  type input = Tast_iterator.iterator

  include GENERAL with type input := input

  (* val run : Compile_common.info -> Tast_iterator.iterator -> Tast_iterator.iterator *)
  val describe_itself : unit -> Yojson.Safe.t
end

module type REPORTER = sig
  val txt : Format.formatter -> unit -> unit
  val md : Format.formatter -> unit -> unit
  val rdjsonl : Format.formatter -> unit -> unit
  val golint : Format.formatter -> unit -> unit
end
