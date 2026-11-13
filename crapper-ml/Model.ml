(** One scored unit: a function, a method or a value bound to a function. *)

type function_ =
  { name : string
  ; namespace : string
  ; complexity : int
  ; start_line : int
  ; end_line : int
  ; (* UTF-8 byte span of the function in the source file *)
    start_byte : int
  ; end_byte : int
  ; path : string
  ; language : string
  }
