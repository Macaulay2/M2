--		Copyright 1994 by Daniel R. Grayson

autoload(Symbol,String) := (symbol,filename) -> (
     if value symbol =!= symbol 
     then error ("symbol ", name symbol, " already has a value");
     symbol <- x -> (
	  load filename; 
	  (value symbol) x
	  );
     protect symbol
     )

autoload(Function,String) := (symbol,filename) -> null

document { quote autoload,
     TT "autoload(f,\"x\")", " -- arranges for a function ", TT "f", " to be 
     automatically loaded from the file named ", TT "x", " the first
     time it is used."
     }
