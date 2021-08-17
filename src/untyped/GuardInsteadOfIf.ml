open Base
open Caml.Format
open Zanuda_core
open Utils

let describe_itself () =
  describe_as_clippy_json
    "use_guard_instead_of_if"
    ~docs:
      {|
### What it does
Pattern matching guards are not very common in mainstream languages so it easy to forget about them for OCaml wannabies.
This lint looks for if-then-else expressions in right hand sides of pattern matching, and recommends to use pattern guards.

### Why is this bad?
Sometimes guards allow you to write less error-prone code. For example, you are matching three values and want to
. if 1st fits predicate then do something and return, check other components otherwise.
. if 2nd fits predicate then do something and return, check other components otherwise.
. if 3rd ..., do something else otherwise.

The implementation with if-then-else could be like this.
```ocaml
match ... with
| (a,b,c) ->
    if pred1 a then ...
    else if pred2 b then ...
    else if pred3 c then ...
    else ... something_else ...
| ...
```
In this case all three bindings are in scope in the right hand side of matching, you can by mistake use them for something. And you can't use wildcards because all three bindings are required in right hand side.

Let's rewrite it with guards:
```ocaml
match ... with
| (a,_,_) when pred1 a -> ...
| (_,b,_) when pred2 b -> ...
| (_,_,c) when pred3 c -> ...
| ...
```

In this variant you have less potential for copy-paste mistake
  |}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let msg = "Prefer guard instead of if-then-else in case construction"

let report ~filename ~loc =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf pp_print_string msg

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        pp_print_string
        msg
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  { fallback with
    case =
      (fun self case ->
        match case.pc_rhs.pexp_desc with
        | Pexp_ifthenelse (_, _, _) ->
          let loc = case.pc_rhs.pexp_loc in
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~filename ~loc)
        | _ -> fallback.case self case)
  }
;;
