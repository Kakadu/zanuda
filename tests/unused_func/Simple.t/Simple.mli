module Meow : sig
  module InnerMeow : sig
    val inner : int
  end
  val woof : int
  val meow : int
end

val meow : int
val is_space : char -> bool