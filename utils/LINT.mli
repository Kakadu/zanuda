module type S = sig
  val stru : Compile_common.info -> Ast_iterator.iterator -> Ast_iterator.iterator
end

module type REPORTER = sig
  val txt : Format.formatter -> unit
  val md : Format.formatter -> unit
  val rdjson : Format.formatter -> unit
  val golint : Format.formatter -> unit
end
