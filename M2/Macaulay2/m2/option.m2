--		Copyright 1993-1999 by Daniel R. Grayson

html Option := x -> toString x
text Option := x -> toString x

new HashTable from List := HashTable => (O,v) -> hashTable v
-- erase symbol hashTable

OptionTable = new Type of HashTable

installMethod(symbol ==>, OptionTable, Function, Function => (defaults,f) -> 
     args -> (
	  -- Common code for functions created with ==> to
	  -- process options and arguments.
	  options := new MutableHashTable from defaults;
	  op := (nam,value) -> (
	       if options#?nam then options#nam = value
	       else error("unrecognized option '", toString nam, "'");
	       );
	  args = select(deepSplice sequence args,
	       a -> (
		    if class a === Option then (op toSequence a;)
		    else if class a === OptionTable then scanPairs(a, op)
		    else true
		    )
	       );
	  (f (new OptionTable from options)) unSingleton args)
     )

installMethod(symbol ==>, List, Function, Function => 
     (o,f) -> (new OptionTable from o) ==> f
     )
