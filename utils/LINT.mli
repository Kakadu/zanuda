module type S = sig
  val stru : Compile_common.info -> Ast_iterator.iterator -> Ast_iterator.iterator
end
