(** The CRAP report printed on stdout, worst score first. *)

let coverage_str = function
  | None -> "    N/A"
  | Some c -> Printf.sprintf "%6.1f%%" c
;;

let crap_str = function
  | None -> "     N/A"
  | Some c -> Printf.sprintf "%8.1f" c
;;

let column_width default cap = function
  | [] -> default
  | strings ->
    Stdlib.max
      default
      (Stdlib.min
         cap
         (List.fold_left (fun acc s -> Stdlib.max acc (String.length s)) 0 strings))
;;

let format_report entries =
  let name_width = column_width 30 48 (List.map (fun e -> e.Model.name) entries) in
  let ns_width = column_width 35 72 (List.map (fun e -> e.Model.namespace) entries) in
  let b = Buffer.create 512 in
  Buffer.add_string b "CRAP Report\n===========\n";
  (match entries with
   | [] -> ()
   | _ -> Buffer.add_string b "Languages: ocaml\n\n");
  Printf.bprintf
    b
    "%-*s %-*s %4s %7s %8s\n"
    name_width
    "Function"
    ns_width
    "Namespace"
    "CC"
    "Cov%"
    "CRAP";
  let header_len = name_width + 1 + ns_width + 1 + 4 + 1 + 7 + 1 + 8 in
  Buffer.add_string b (String.make header_len '-');
  Buffer.add_char b '\n';
  List.iter
    (fun e ->
      Printf.bprintf
        b
        "%-*s %-*s %4d %s %s\n"
        name_width
        e.Model.name
        ns_width
        e.Model.namespace
        e.Model.complexity
        (coverage_str e.Model.coverage)
        (crap_str e.Model.crap))
    entries;
  Buffer.contents b
;;
