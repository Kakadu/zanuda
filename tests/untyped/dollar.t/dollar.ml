type t = { field : int }
let pp : t -> unit = fun _ -> ()
let f =
  pp @@ { field = 1 };;
let _ = string_of_int @@ 4
let _ = string_of_int @@ max_int
let _ = (function _ -> ()) @@ (1,2)
let _ = (function _ -> ()) @@ None
