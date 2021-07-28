open Base

module Level = struct
  type t =
    | Allow
    | Warn
    | Deny
    | Deprecated
end

module Groups = struct
  type t = Style
  (* Correctness Perf Restriction  Deprecated Pedantic Complexity Suspicious Cargo Nursery *)
end

module Lints = struct
  type t = string

  let found_Lints : (Location.t * t) Queue.t = Queue.create ()
  let clear () = Queue.clear found_Lints
  let is_empty () = Queue.is_empty found_Lints
  let add ~loc s = Queue.enqueue found_Lints (loc, s)
  let addf ?(loc = Location.none) fmt = Caml.Format.kasprintf (add ~loc) fmt
  let snake_case ~loc name = addf ~loc "Type name `%s` should be in snake case" name

  let guard_insteadof_if ~loc =
    addf ~loc "Prefer guard instead of if-then-else in case construction"
  ;;

  let report () =
    (* let printer = Location.batch_mode_printer in *)
    Queue.iter found_Lints ~f:(fun (loc, s) ->
        (* Format.printf "%s\n%!" s; *)
        let r =
          let main = Location.mkloc (fun ppf -> Caml.Format.fprintf ppf "%s" s) loc in
          Location.{ sub = []; main; kind = Report_alert "asdf" }
        in
        Location.print_report Caml.Format.std_formatter r)
  ;;
end

module Casing = struct
  let is_camel_case s = String.(lowercase s <> s)

  open Ast_iterator

  let l fallback =
    { fallback with
      type_declaration =
        (fun self tdecl ->
          let open Parsetree in
          let tname = tdecl.ptype_name.txt in
          if is_camel_case tname then Lints.snake_case ~loc:tdecl.ptype_loc tname;
          fallback.type_declaration self tdecl)
    }
  ;;
end

module GuardInsteadOfIf = struct
  open Parsetree
  open Ast_iterator

  let l fallback =
    { fallback with
      case =
        (fun self case ->
          match case.pc_rhs.pexp_desc with
          | Pexp_ifthenelse (_, _, _) ->
            Lints.guard_insteadof_if ~loc:case.pc_rhs.pexp_loc
          | _ -> fallback.case self case)
    }
  ;;
end

let on_structure stru =
  let open Ast_iterator in
  let o = GuardInsteadOfIf.l (Casing.l Ast_iterator.default_iterator) in
  o.structure o stru
;;

let load_file filename =
  let with_info f =
    Compile_common.with_info
      ~native:false
      ~source_file:filename
      ~tool_name:"asdf"
      ~output_prefix:"asdf"
      ~dump_ext:"asdf"
      f
  in
  let parsetree = with_info Compile_common.parse_impl in
  (* Caml.print_endline @@ Pprintast.string_of_structure parsetree; *)
  on_structure parsetree;
  Lints.report ();
  Lints.clear ();
  (* let tstr, _coe = with_info (fun info -> Compile_common.typecheck_impl info parsetree) in *)
  (* Format.printf "%a\n%!" Printtyped.implementation tstr; *)
  ()
;;

let () =
  Clflags.error_style := Some Misc.Error_style.Contextual;
  let filename = Caml.Sys.argv.(1) in
  load_file filename;
  Caml.exit 0
;;
