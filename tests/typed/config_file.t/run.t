Without a .zanuda file, the physical equality is reported.
  $ dune build @check
  $ zanuda() { ../zanuda.exe -no-top_file_license -no-eta_reduction -no-check-filesystem -dir . -ordjsonl /dev/null 2>&1; }
  $ zanuda | grep -c 'physical equality'
  1

A `#` line is a comment, and blank lines are skipped.
  $ cat > .zanuda <<EOF
  > # A reason for each entry.
  > 
  > ignore physical_equality A.ml
  >   # indented comments too
  > EOF
  $ zanuda | grep -c '^\.zanuda:'
  0
  [1]
  $ zanuda | grep -c 'physical equality'
  0
  [1]

`ignore <files>` without a lint id skips every finding in those files.
  $ echo 'ignore A.ml' > .zanuda
  $ zanuda | grep -c 'smells bad\|nested if\|physical equality'
  0
  [1]

A line zanuda cannot interpret is an error, so a typo cannot silently leave a lint on.
  $ cat > .zanuda <<EOF
  > ignore physical_equality A.ml
  > ignroe physical_equality A.ml
  > EOF
  $ zanuda
  .zanuda: Don't know what to do with "ignroe physical_equality A.ml"
  [1]

An `ignore <lint_id> <path>` entry that suppressed nothing is reported.
  $ cat > .zanuda <<EOF
  > ignore physical_equality A.ml
  > ignore physical_equality B.ml
  > ignore eta_reduction A.ml
  > EOF
  $ zanuda | grep '^\.zanuda:'
  .zanuda: ignore physical_equality B.ml matched no finding
