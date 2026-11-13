(* Exercises every branch of [complex_copy]; [complex] stays uncovered *)
let () =
  ignore (Cm.complex_copy [] 0);
  ignore (Cm.complex_copy [ 2 ] 2);
  ignore (Cm.complex_copy [ 2 ] (-2))
