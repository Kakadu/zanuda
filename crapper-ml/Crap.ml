(** CRAP formula shared by crap4clj, crap4java and crap4go. *)

(* round to four decimal places, like the reference implementation *)
let round_metric = function
  | None -> None
  | Some v -> Some (Float.round (v *. 10000.0) /. 10000.0)
;;

(* [coverage_pct] is a percentage from 0 to 100 *)
let score complexity coverage_pct =
  let cc = float_of_int complexity in
  let uncovered = 1.0 -. (coverage_pct /. 100.0) in
  (cc *. cc *. Float.pow uncovered 3.0) +. cc
;;

let make_entry ~coverage (f : Model.function_) =
  let coverage = round_metric coverage in
  let crap =
    match coverage with
    | None -> None
    | Some c -> Some (score f.Model.complexity c)
  in
  { Model.name = f.Model.name
  ; namespace = f.Model.namespace
  ; complexity = f.Model.complexity
  ; coverage
  ; crap = round_metric crap
  ; path = f.Model.path
  ; language = f.Model.language
  }
;;

(* Worst CRAP first. Unscored rows follow scored rows. *)
let sort_entries entries =
  let key (e : Model.entry) =
    match e.Model.crap with
    | Some crap -> 0, -.crap, e.Model.namespace, e.Model.name, e.Model.path
    | None ->
      1, float_of_int (-e.Model.complexity), e.Model.namespace, e.Model.name, e.Model.path
  in
  List.sort (fun a b -> compare (key a) (key b)) entries
;;
