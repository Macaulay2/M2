--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

autoload = method()
autoload(Symbol,String) := (sym,filename) -> (
     if value sym =!= sym then error ("symbol ", toString sym, " already has a value");
     sym <- f := x -> (
	  load filename;
	  if f === value sym 
	  then error("symbol '", toString sym, "' didn't acquire a new value in file '", filename, "'");
	  (value sym) x
	  );
     )

autoload(Function,String) := (sym,filename) -> null



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
