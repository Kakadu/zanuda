## Working version

### New

### Changed

- #78: Fix false positives while detecting used variables that start from `_`
- #80: Disable fixes collection by default

## 2.0.0 (18-09-2025)

### New

- Add OCaml 5.3 support. Now it works both on 4.14.2 and 5.3.0

## 1.1.0 (18-02-2025)

### New

- #13: Add lint that discourages matching a tuple using 'match' expression with single branch
  (contributed by @s-khechnev)
- #18: Add lint about unneeded mutually recursive types
  (contributed by @s-khechnev)
- Add command line switch '-skip-level-allow <bool>' to enable/disable lints
  with level=Allow. False has higher priority than per-lint command line switch
  (for example, `-no-string_concat`)
- Add check for configuration file '.zanuda' in CWD.
- #22: Add 'reviewer' tool to report lint's a Github review.
  (contributed by @s-khechnev)
- #23: Implement a trial version of the Fix module for auto-correction of lints
  (contributed by @Artem-Rzhankoff)
- #28: Add lint about nested if expressions.
  (contributed by @Artem-Rzhankoff)
- #32: Add lint about constructor names that hide default constructor names
  (contributed by @nnemakin)
- #35: Add lints that detects manual implementations of List.map/fold functions
  (contributed by @nnemakin)
- #50: Propose eta reduction when available (contributed by @jegorpopow)
- #51: Warn about pattern matching on boolean values (contributed by @jegorpopow)
- #53: Warn about `"%s"` in formatted strings
- #54: Detection of unused public declarations. (@Kakadu, initial implementation from @jegorpopow)
  At the moment support is kind of flaky: it could be broken by several things.

    * Custom `public_name` for library may broke detection of `.cmt[i]` files.
    * We [need to](https://github.com/ocaml/dune/issues/9724) do 'dune build @check' to generate all required `cmt` files.
- #56: Simplify lint about license. We look for required doc-comments anywhere in the file,
  not only in the beginning.
- #60: Skip some checks for some source files (configured via '.zanuda'). Currently this is mutability check and phys_equal check

### Changed

- #15: Split 'string_concat' lint to check separately patterns 'a^b^c' (level=Allow) and 'List.fold_left (^)' (level=Warn).
  (reported by @edwintorok)
- #16: Calculate test coverage.
- The lint 'mutability_check' now checks for mutability in general: references, mutable record fields, etc.


## 1.0.0 (24-03-2023)

### Changed

- First release
