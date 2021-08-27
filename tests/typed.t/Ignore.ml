let __ xs =
  ignore (List.map ((+)1) xs);
  xs

let __ xs =
  Base.ignore (List.map string_of_int xs)
