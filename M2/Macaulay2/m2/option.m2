--		Copyright 1993-1999 by Daniel R. Grayson

html Option := x -> toString x
text Option := x -> toString x

new HashTable from List := HashTable => (O,v) -> hashTable v
-- erase symbol hashTable

OptionTable = new Type of HashTable

 -- installMethod(symbol ==>, OptionTable, Function, Function => 
 --      (defaults,f) -> args -> (
 -- 	  -- Common code for functions created with ==> to
 -- 	  -- process options and arguments.
 -- 	  options := new MutableHashTable from defaults;
 -- 	  overrideOption := (key,value) -> (
 -- 	       if options#?key then options#key = value
 -- 	       else error("unrecognized option '", toString key, "'");
 -- 	       false);
 -- 	  args = select(deepSplice sequence args,
 -- 	       a -> (
 -- 		    if class a === Option then overrideOption toSequence a
 -- 		    else if class a === OptionTable then scanPairs(a, overrideOption)
 -- 		    else true
 -- 		    )
 -- 	       );
 -- 	  options = new OptionTable from options;
 -- 	  args = unSingleton args;
 -- 	  (f options) args )
 --      )

installMethod(symbol ==>, OptionTable, Function, Function => 
     (defaults,f) -> args -> (
	  -- Common code for functions created with ==> to
	  -- process options and arguments.
	  f ## override (defaults,args)
	  )
     )

installMethod(symbol ==>, List, Function, Function => 
     (o,f) -> (new OptionTable from o) ==> f
     )
