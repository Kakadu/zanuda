let rec _foo x = _foo x
let _boo x = 1+x
let _ = _boo 5

let cbool =
  let _true = "true"  in
  _true
;;
let cbool2 =
  let _true = "true"  in
  let _true = _true  in
  1
;;
let __ _menhir_action_05 = _menhir_action_05
let __ _menhir_cell1_RULECOMPONENT = _menhir_cell1_RULECOMPONENT
let f1 () = 
  let _menhir_action_05 = 1 in 
  _menhir_action_05

(** Should not give a warning *)
let __ () = ()

(* let f2 () = __ () *)
let __ () = ()

let _menhir_action_1 = ()
include struct
  let _menhir_run_0  = function
      | None -> _menhir_action_1 ()
      | _ -> ()
end
