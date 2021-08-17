#### An OCaml linter experiment. TODOs

- Follow clippy
  - [ ] naming convention
  - [ ] revisit and support the same list of lints
  - [x] add command line switch to output list of currently supported linters in a [fancy](https://rust-lang.github.io/rust-clippy/master/index.html) manner
  - [x] Generate linter info as [fancy html](https://github.com/rust-lang/rust-clippy/tree/gh-pages/master) and push to github pages
- Add more linters:
  - [ ] An action that panics on `if true then ...`
  - [ ] `if ... then true else false`
  - [ ] Expression `... ^ ... ^ ...` may not be efficient
  - [ ] Parser-combinator stuff:
	- [ ] Simplify 'token "#" >> token "#"'
  - [ ] AlgDT with single constructor
  - [ ] Global mutable variables
  - [ ] Detect functions that not used
  - [ ] Ignore without specifying type of ignored variable
    - [ ] ignore as last expression of function (which doesn't really ignore anything)
    - [ ] maybe restrict ignoring of monadic values
  - [ ] Adding stuff to Monad signature that doesn't belong there
  - [ ] Detect functions already present in Stdlib and Base. Maybe only declared as structure items.
    - [ ] The same for exceptions
  - [ ] Copypaste detection
  - [ ] Rewrite common idiom
    - [ ] 'concat' + 'map' => `concat_map`
    - [ ] 'filter' + 'map' => `filter_map`
    - [ ] etc...
  - [ ] Exceptions
    - [ ] `failwith`
    - [ ] Using of `List.hd_exn` may be error-prone
    - [ ] Matching with `_` exception
    - [ ] Catching `Failure _` exception
    - [ ] Suggest function name with `_exn` suffix when function can throw an exception.
  - [ ] Rewrite 'fun e -> match e with ..' to 'function ...' if possible
  - [ ] Records
    - [ ] Rewrite '{ e=f.e; g=g.g; h=189 }' to `{ f with h = 189 }`
    - [ ] Rewrite `{ key = fkey; foo = _; bar = _ }` to `{ key = fkey; _ }`
    - [ ] Rewrite '{ field=field; ... }' but maybe it is not possible and ocamlformat should do it
  - [ ] Rewrite `fun cd -> let f ... = (*  no use of cd *) in ...` to `let f ... = ... in fun cd -> ...`
  - [ ] Expressions like ` ... :: [ ... ]` smell bad
  - [ ] Matching
    - [ ] Use match instead of `if x = WasReturn then ... else if x = WasContinue then .. else ...`
    - [ ] `  | Return rexpr_o -> ( match rexpr_o with ... )`
    - [ ] Recommend or-patterns when possible
  - [x] Toplevel `print_string ...` should be `let () = ...`


CI-related TODOs:

- [x] Integrate [reviewdog](https://github.com/reviewdog/reviewdog#installation) output
  - Finished in the repo https://github.com/Kakadu/fp2021 . The complicated part was to discover that only RDJSONL is suitable input format for reviewdog. The default one 'errorformat' a la VIM I couldn't manage to make workable.
- [ ] Integrate copy-paste detector
