module type S = sig
  val stru : Compile_common.info -> Ast_iterator.iterator -> Ast_iterator.iterator
  val describe_itself : unit -> Yojson.Safe.t
end

module type REPORTER = sig
  val txt : Format.formatter -> unit -> unit
  val md : Format.formatter -> unit -> unit
  val rdjsonl : Format.formatter -> unit -> unit
  val golint : Format.formatter -> unit -> unit
end
