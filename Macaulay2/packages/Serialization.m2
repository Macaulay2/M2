newPackage (
     "Serialization",
     DebuggingMode => true,
     Headline => "reversible conversion of all Macaulay2 objects to strings")

-- this code is re-entrant

waterMark := hash symbol waterMark
export { "serialize", "reload" }

reload = Command ( x -> loadPackage ("Serialization",Reload => true) )

debug Core
    getAttribute' = getAttribute
    hasAttribute' = hasAttribute
    PrintNet' = PrintNet
    PrintNames' = PrintNames
    ReverseDictionary' = ReverseDictionary
dictionaryPath = delete(Core#"private dictionary",dictionaryPath)

serialize = x -> (
     h := new MutableHashTable;				    -- expressions for atoms
     k := new MutableHashTable;				    -- serial numbers for composite objects
     p := method();
     p Thing := x -> (					    -- remember to remove these methods later, to prevent a memory leak
	  if mutable x then (
	       if hash x < waterMark then (
		    if hasAttribute'(x,ReverseDictionary')
		    then h#x = toString getAttribute'(x,ReverseDictionary')
		    else error "serialize: encountered an older mutable object not assigned to a global variable";
		    )
	       else (
		    error "serialize: encountered a recent mutable object with no known serialization method";
		    )
	       )
	  else (
	       h#x = toExternalString x;
	       )
	  );
     p x;
     assert Thing#?p; remove(Thing,p);
     ( newClass(HashTable,h), newClass(HashTable,k) )
     )
