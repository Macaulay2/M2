--		Copyright 1994 by Daniel R. Grayson

olderror := error
erase quote error
error = args -> olderror apply(
     sequence args, x -> if class x === String then x else name x
     )
protect quote error

on = f -> (
     n := name f;
     depth := 0;
     totaltime := 0.;
     i := 0;
     if class f =!= Function then error("expected a function");
     x -> (
	  j := i;
	  i = i+1;
     	  << n << " (" << j << ")";
	  if depth > 0 then << " [" << depth << "]";
	  << " called with args : " << x << endl;
	  depth = depth + 1;
     	  r := timing f x;
	  timeused := r#0;
	  value := r#1;
	  depth = depth - 1;
	  if depth === 0 then totaltime = totaltime + timeused;
     	  << n << " (" << j << ")";
	  if depth > 0 then << " [" << depth << "]";
	  << " " << timeused << " seconds";
	  if depth === 0 then << ", total " << totaltime << " seconds";
	  << endl << "  and returned value : " << value << endl;
     	  value)
     )

assert = x -> if not x then error "assertion failed"

notImplemented = x -> error "not implemented yet"

benchmark = (s) -> (
     n := 1;
     while (
	  s1 := concatenate("timing scan(",string n,", i -> (",s,";null;null))");
	  s2 := concatenate("timing scan(",string n,", i -> (      null;null))");
	  collectGarbage();
	  value s1;
	  value s2;
	  collectGarbage();
	  value s1;
	  value s2;
	  collectGarbage();
	  t1 := first value s1;
	  t2 := first value s2;
	  t := t1 - t2;
	  t < 1 and t2 < 5)
     do (
	  n = if t > 0.01 then floor(2*n/t+.5) else 100*n;
	  );
     t/n)


