#### An OCaml linter experiment. TODOs

- Follow clippy
  - [ ] naming convention
  - [ ] revisit and support the same list of lints
  - [ ] add switch to output list of currently supported linters in a [fancy](https://rust-lang.github.io/rust-clippy/master/index.html) manner
  - [ ] Generate linter info as [fancy html](https://github.com/rust-lang/rust-clippy/tree/gh-pages/master) and push to github pages
- Add more linters:
  - [ ] An action that panics on `if true then ...`
- [x] Integrate [reviewdog](https://github.com/reviewdog/reviewdog#installation) output
  - Finished in the repo https://github.com/Kakadu/fp2021 . The complicated part was to discover that only RDJSONL is suitable input format for reviewdog. The default one 'errorformat' a la VIM I couldn't manage to make workable.
