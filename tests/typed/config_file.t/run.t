Without a .zanuda file, the physical equality is reported.
  $ dune build @check
  $ zanuda() { ../zanuda.exe -no-top_file_license -no-eta_reduction -no-check-filesystem -dir . -ordjsonl /dev/null "$@" 2>&1; }
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

With `-report-stale-ignores`, an `ignore <lint_id> <path>` entry that suppressed nothing is reported.
  $ cat > .zanuda <<EOF
  > ignore physical_equality A.ml
  > ignore physical_equality B.ml
  > ignore eta_reduction A.ml
  > EOF
  $ zanuda -report-stale-ignores | grep '^\.zanuda:'
  .zanuda: ignore physical_equality B.ml matched no finding

Without `-report-stale-ignores` nothing is reported.
  $ zanuda | grep -c '^\.zanuda:'
  0
  [1]

By default findings do not change the exit status.
  $ rm -f .zanuda
  $ zanuda > /dev/null
  $ echo $?
  0

`-fail-on warn` exits 1 on a finding of level Warn or above.
  $ zanuda -fail-on warn > /dev/null
  [1]

`-fail-on deny` ignores Warn findings.
  $ zanuda -fail-on deny > /dev/null

Findings suppressed by `.zanuda` do not count.
  $ echo 'ignore A.ml' > .zanuda
  $ zanuda -fail-on warn > /dev/null
