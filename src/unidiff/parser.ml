(*
commit:
  https://github.com/<owner>/<repo>/commit/<commit>.diff
  wget https://github.com/Kakadu/zanuda/commit/e4d35c2.diff

pull:
   wget https://github.com/Kakadu/zanuda/commit/e4d35c2.diff
*)

(*
doc: https://www.artima.com/weblogs/viewpost.jsp?thread=164293
https://github.com/reviewdog/reviewdog/blob/master/diff/parse.go
https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html
*)
open Angstrom

let title = fail "not implemented"
let hunk = fail "not implemented"
let main = title *> many hunk
