type t = { field : int }
let pp : t -> unit = fun _ -> ()
let f =
  pp @@ { field = 1 }
