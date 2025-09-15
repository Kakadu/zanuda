  $ dune build @check
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File '_build/default/No_iface.ml' doesn't have corresponding .mli interface
