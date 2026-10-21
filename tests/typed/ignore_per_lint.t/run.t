Without a config every lint reports in every file.
  $ dune build @check
  $ ../zanuda.exe -no-top_file_license -no-eta_reduction -dir . -osarif all.sarif -ordjsonl /dev/null | sed '/^[[:space:]]*$/d' | grep -v '^ '
  File "A.ml", line 2, characters 2-77:
  2 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
  File "A.ml", line 4, characters 18-40:
  4 | let if_bool x y = if x then y else false
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  File "A.ml", line 5, characters 15-21:
  5 | let phys x y = x == y
  Alert zanuda-linter: Do you really need physical equality? Physical means the equality of pointers.
  File "B.ml", line 2, characters 2-77:
  2 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
  File "B.ml", line 4, characters 18-40:
  4 | let if_bool x y = if x then y else false
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  File "B.ml", line 5, characters 15-21:
  5 | let phys x y = x == y
  Alert zanuda-linter: Do you really need physical equality? Physical means the equality of pointers.
  File '_build/default/C.ml' doesn't have corresponding .mli interface
  $ grep -E '"(ruleId|uri)"' all.sarif
            "ruleId": "forbid_many_nested_if_expressions",
                  "artifactLocation": { "uri": "A.ml" },
            "ruleId": "if_bool",
                  "artifactLocation": { "uri": "A.ml" },
            "ruleId": "physical_equality",
                  "artifactLocation": { "uri": "A.ml" },
            "ruleId": "forbid_many_nested_if_expressions",
                  "artifactLocation": { "uri": "B.ml" },
            "ruleId": "if_bool",
                  "artifactLocation": { "uri": "B.ml" },
            "ruleId": "physical_equality",
                  "artifactLocation": { "uri": "B.ml" },
            "ruleId": "lint_filesystem",
                  "artifactLocation": { "uri": "C.ml" },

A per-lint ignore works for any lint, including one without its own switch
(forbid_many_nested_if_expressions) and lint_filesystem. `forward ... ignore` is
the older spelling of the same thing.
  $ cat > .zanuda <<EOF
  > ignore forbid_many_nested_if_expressions A.ml
  > forward physical_equality ignore B.ml
  > ignore lint_filesystem C.ml
  > EOF
  $ ../zanuda.exe -no-top_file_license -no-eta_reduction -dir . -osarif some.sarif -ordjsonl /dev/null | sed '/^[[:space:]]*$/d' | grep -v '^ '
  File "A.ml", line 4, characters 18-40:
  4 | let if_bool x y = if x then y else false
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  File "A.ml", line 5, characters 15-21:
  5 | let phys x y = x == y
  Alert zanuda-linter: Do you really need physical equality? Physical means the equality of pointers.
  File "B.ml", line 2, characters 2-77:
  2 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
  File "B.ml", line 4, characters 18-40:
  4 | let if_bool x y = if x then y else false
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  $ grep -E '"(ruleId|uri)"' some.sarif
            "ruleId": "if_bool",
                  "artifactLocation": { "uri": "A.ml" },
            "ruleId": "physical_equality",
                  "artifactLocation": { "uri": "A.ml" },
            "ruleId": "forbid_many_nested_if_expressions",
                  "artifactLocation": { "uri": "B.ml" },
            "ruleId": "if_bool",
                  "artifactLocation": { "uri": "B.ml" },

Several paths in one line, and repeated lines accumulate.
  $ cat > .zanuda <<EOF
  > ignore if_bool A.ml,B.ml
  > ignore physical_equality A.ml
  > ignore physical_equality B.ml
  > EOF
  $ ../zanuda.exe -no-top_file_license -no-eta_reduction -no-check-filesystem -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d' | grep -v '^ '
  File "A.ml", line 2, characters 2-77:
  2 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
  File "B.ml", line 2, characters 2-77:
  2 |   if x then (if x then (if x then (if x then x else y) else y) else y) else y
  Alert zanuda-linter: Using nested if expressions more than three layers deep is a bad practice. Use let statements or helper methods or rethinking logic.
