--		Copyright 1994 by Daniel R. Grayson

autoload(Symbol,String) := (symbol,filename) -> (
     if value symbol =!= symbol 
     then error ("symbol ", toString symbol, " already has a value");
     symbol <- x -> (
	  load filename; 
	  (value symbol) x
	  );
     protect symbol
     )

autoload(Function,String) := (symbol,filename) -> null


