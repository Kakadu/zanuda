module type S = sig
  val stru : Compile_common.info -> Ast_iterator.iterator -> Ast_iterator.iterator
end

module type REPORTER = sig
  val txt : Format.formatter -> unit -> unit
  val md : Format.formatter -> unit -> unit

  (* val rdjson : Format.formatter -> unit -> unit *)
  val golint : Format.formatter -> unit -> unit
end
