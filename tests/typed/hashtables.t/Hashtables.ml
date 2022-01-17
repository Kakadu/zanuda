let _f () =
  let h = Hashtbl.create 18 in
  if None = None then h else h
(*
let f () =
  let open Base in
  let h = Hashtbl.create (module Int) in
  h *)


type _t1 = { xxx : int ref }
type _t2 = { xxx : int Base.ref }
