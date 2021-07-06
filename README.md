## Reproduction: dune does not make .lint-corrected files promotable automatically

The executable `utils/add_copyright.ml` will add a small copyright at the beginning of the file if it is not already there

The file `lib/dune` contains the following linting rule for the library:

```dune
(lint
  (action
    (run copyright %{input-file} %{input-file}.lint-corrected)))
```

As expected, after running `dune build @lint`, we can see that `lib` doesn't contain the copyright so a `.lint-corrected` version is created, but not for `lib_correct.ml` has it so nothing is done.
```sh
$ ls _build/default/lib/
lib.ml
lib.ml.lint-corrected
lib_correct.ml
```

However, dune does not make `lib.ml` promotable nor does it show the diffs.
