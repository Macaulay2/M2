--		Copyright 1993-1999 by Daniel R. Grayson

userSymbols = type -> (
     if type === () then type = Thing;
     tab := symbolTable();
     v := select(values tab,
	  symb -> (
	       hash symb > hash lastSystemSymbol  -- hash codes of symbols are sequential
	       and mutable symb
	       and instance(value symb,type)
	       )
	  );
     apply(sort(apply(v, symb -> (hash symb, symb))), (h,s) -> s))

listUserSymbols = new Command from (
     type -> stack apply(userSymbols type, s ->  string s | ": " | name class value s)
     )

clearedSymbol := "-- cleared symbol --"

clearOutput = new Command from (() -> (
     	  scan(keys outputSymbols, s -> (
	       	    remove(outputSymbols,s);
		    s <- clearedSymbol;
	       	    erase s))))

clearAll = new Command from (() -> (
     	  clearOutput();
     	  scan(userSymbols(), i -> (
		    i <- clearedSymbol;
		    erase i))))
