--		Copyright 1993-1999 by Daniel R. Grayson

autoload(Symbol,String) := (symbol,filename) -> (
     if value symbol =!= symbol then error ("symbol ", toString symbol, " already has a value");
     symbol <- f := x -> (
	  load filename;
	  if f === value symbol 
	  then error("symbol '", toString symbol, "' didn't acquire a new value in file '", filename, "'");
	  (value symbol) x
	  );
     protect symbol;
     )

autoload(Function,String) := (symbol,filename) -> null


