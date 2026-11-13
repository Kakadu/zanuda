(* Exercised by the crapper's coverage run: some functions get hits, the
   rest stay at 0% *)
let () =
  ignore (Foo.add 1 2);
  ignore (Foo.even 4);
  ignore (Foo.loops (ref 0));
  ignore (Foo.Inner.twice 3);
  let c = new Foo.counter in
  ignore c#get;
  ignore (c#bump ())
