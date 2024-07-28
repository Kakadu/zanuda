let count =
  let last = ref 0 in
  (fun () -> incr last; !last)