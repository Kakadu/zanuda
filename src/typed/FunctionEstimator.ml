open Base
open Zanuda_core
open Zanuda_core.Utils


let lint_id = "functions_count_estimator"
let lint_source = LINT.Other

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~docs:
      {|
      It's not a linter. It's an estimator of the number of functions defined in file.
      Designed as linter for easier integration to existing architecture.
      The actual number of functions is less than or equal to the estimation.
      More precisely estimation is equal to the sum of the arities of all functions in the file.
      |}

type input = Tast_iterator.iterator


let log (expr: Typedtree.expression) =
  if Config.print_functions_count () & Config.verbose ()
  then
    let loc = expr.exp_loc.loc_start in
    let line = loc.pos_lnum in
    let fname = loc.pos_fname in
    printfn "Function found in %s:%d" fname line


let counters: (string * int ref) Queue.t = Queue.create ()

let printReport () =
  Queue.iter counters ~f:(fun (fname, count) ->
      printfn "There are no more than %d functions in %s" !count fname)


let run info fallback =
  let open Tast_iterator in
  let open Typedtree in

  let counter = ref 0 in
  Queue.enqueue counters (info.Compile_common.source_file, counter);
  { fallback with
    expr = 
      (fun self expr -> 
         (
           match expr.exp_desc with
           | Texp_function _ ->
             log expr;
             incr counter
           | _ -> ()
         ); 
         fallback.expr self expr)
  }
