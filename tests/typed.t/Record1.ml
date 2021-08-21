type t = { x: int; y:int; z:int}

let f1 _ = { x=1; y=1; z=1 }
let f2 r = { x=r.x; y=r.y; z=r.z }
let f3 r = { x=r.x; y=r.y; z=18 }
let f4 r r2 = { x=r.x; y=r.y; z=r2.z }
let f5 r r2 = { x=r.x; y=1; z=r2.z }
