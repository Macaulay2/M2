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
     k := new MutableHashTable;				    -- serial numbers for composite mutable objects, to be remade in parallel
     m := new MutableHashTable; 			    -- serial numbers for composite immutable objects, to be remade in serial
     p := method(Dispatch => Thing);
     p Thing := x -> (					    -- remember to remove these methods later, to prevent a memory leak
	  if h#?x then return;
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
     p Function := x -> (
	  if h#?x then return;
	  if hash x < waterMark then (
	       if hasAttribute'(x,ReverseDictionary')
	       then h#x = toString getAttribute'(x,ReverseDictionary')
	       else error "serialize: encountered an older function not assigned to a global variable";
	       )
	  else (
	       error "serialize: encountered a recent function; functions cannot be serialized yet";
	       )
	  );
     p BasicList := x -> (
	  if m#?x then return;
	  m#x = #m;
	  scan(x,p);
	  );
     p HashTable := x -> (
	  if m#?x then return;
	  m#x = #m;
	  scanPairs(x,(k,v) -> (p k; p v));
	  );
     p MutableList := x -> (
	  if k#?x then return;
	  k#x = #k;
	  scan(x,p);
	  );
     p MutableHashTable := x -> (
	  if k#?x then return;
	  k#x = #k;
	  scan(pairs x,(k,v) -> (p k; p v));
	  );
     p x;
     assert Thing#?p; remove(Thing,p);
     ( newClass(HashTable,h), newClass(HashTable,k), newClass(HashTable,m) )
     )

end
loadPackage "Serialization"
reload
x = new MutableList; y = new MutableHashTable; x#0 = y; y#x = {4,5,6}; y#4 = x; y#y = y;
serialize y
