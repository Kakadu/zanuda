let rec _foo x = _foo x
let _boo x = 1+x
let _ = _boo 5

let cbool =
  let _true = "true"  in
  _true
;;
