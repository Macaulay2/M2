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

document { quote on,
     TT "f = on f", " -- replaces the function ", TT "f", " by a version which 
     will print out its arguments and return value each time it's called,
     together with a sequence number so the two reports can be connected.",
     PARA,
     "This function is of only limited utility because it cannot be used
     with write-protected system functions.",
     PARA,
     "The reason we write ", TT "f = on f", " and not something like
     ", TT "f = on(x -> ...)", " is so the function handed to ", TO "on", "
     will know its name.  The name will appear in the display."
     }

assert = x -> if not x then error "assertion failed"

document { quote assert,
     TT "assert x", " -- prints an error message if x isn't true."
     }

notImplemented = x -> error "not implemented yet"
document { quote notImplemented,
     TT "notImplemented()", " -- print an error message that 
     says \"not implemented yet\"."
     }

document { quote errorDepth,
     TT "errorDepth i", " -- sets the error depth to i, which should be
     a small integer, returning the old value.",
     PARA,
     "During the backtrace after an error message, a position in interpreted
     code is displayed only if the value of ", TO "reloaded", " was at least
     as large as the error depth is now.  Typically, the error depth is set
     to 1 so that messages from code pre-interpreted and reloaded with 
     ", TO "loaddata", " will not appear in the backtrace."
     }

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

document { quote benchmark,
     TT "benchmark s", " -- produce an accurate timing for the code contained
     in the string ", TT "s", ".  The value returned is the number of seconds.",
     PARA,
     "The snippet of code provided will be run enough times to register
     meaningfully on the clock, and the garbage collector will be called
     beforehand."
     }
