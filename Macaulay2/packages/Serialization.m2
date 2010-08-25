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
     k := new MutableHashTable;				    -- serial numbers of objects
     k':= new MutableHashTable;				    -- inverse function of k
     code1 := new MutableHashTable;			    -- initialization code for everything, by index
     code2 := new MutableHashTable;			    -- finalization code for mutable objects, by index
     newk := f -> x -> (
	  if k#?x then return k#x;
	  i := #k;
	  k #x = i;
	  k'#i = x;
	  f(x,i);
	  i);
     p := method(Dispatch => Thing);
     -- remember to remove these methods later, to prevent a memory leak:
     p Thing := newk ( (x,i) -> (
	       if mutable x then (
		    if hash x < waterMark then (
			 if hasAttribute'(x,ReverseDictionary')
			 then (
			      code1#i = toString getAttribute'(x,ReverseDictionary');
			      )
			 else error "serialize: encountered an older mutable object not assigned to a global variable";
			 )
		    else (
			 error "serialize: encountered a recent mutable object with no known serialization method";
			 )
		    )
	       else (
		    code1#i = toExternalString x;
		    )));
     p Function := newk ( (x,i) -> (
	       if hash x < waterMark then (
		    if hasAttribute'(x,ReverseDictionary')
		    then (
			 code1#i = toString getAttribute'(x,ReverseDictionary');
			 )
		    else error "serialize: encountered an older function not assigned to a global variable";
		    )
	       else (
		    error "serialize: encountered a recent function; functions cannot be serialized yet";
		    )
	       ));
     p BasicList := newk ( (x,i) -> (
	       p class x;
	       scan(x,p);
	       ));
     p HashTable := newk ( (x,i) -> (
	       p class x;
	       p parent x;
	       scanPairs(x,(k,v) -> (p k; p v));
	       ));
     p MutableList := newk ( (x,i) -> (
	       if mutable x and hash x < waterMark and hasAttribute'(x,ReverseDictionary') then (
		    code1#i = toString getAttribute'(x,ReverseDictionary');
		    )
	       else (
		    p class x;
		    scan(x,p);
		    )));
     p MutableHashTable := newk ( (x,i) -> (
	       if mutable x and hash x < waterMark and hasAttribute'(x,ReverseDictionary') then (
		    code1#i = toString getAttribute'(x,ReverseDictionary');
		    )
	       else (
		    p class x;
		    p parent x;
		    scan(pairs x,(k,v) -> (p k; p v));
		    )));
     p GlobalDictionary := newk ( (x,i) -> (
	       if hash x < waterMark and hasAttribute'(x,ReverseDictionary') then (
		    code1#i = toString getAttribute'(x,ReverseDictionary');
		    )
	       else (
		    scan(pairs x,(k,v) -> (p k; p v));
		    )));
     p x;
     assert Thing#?p; remove(Thing,p);
     k = newClass(HashTable,k);
     k' = newClass(HashTable,k');
     code1 = newClass(HashTable,code1);
     code2 = newClass(HashTable,code2);
     netList {
	  {"objects by index  (k)",k},
	  {"indices by object (k')",k'},
	  {"code by index (code1)",code1},
	  {"code by index (code2)",code2}
	  }
     )

end
loadPackage "Serialization"
reload
x = new MutableList; y = new MutableHashTable; x#0 = y; y#x = {4,[5,[6]]}; y#4 = x; y#y = y;
serialize y
