type t0 = { e: int; g:int }

type t = { e: int; g:int; h:int}


let t_of_t0 f = { e=f.e; g=f.g; h=1}
