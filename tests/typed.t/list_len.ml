[@@@ocaml.warning "-33"]
open Parsetree

let __ xs = List.length xs < 0

let __ xs = List.length xs <= 0

let __ xs = List.length xs = 0

let __ xs = List.length xs >= 0

let __ xs = List.length xs > 0

let __ xs =  0 < List.length xs
