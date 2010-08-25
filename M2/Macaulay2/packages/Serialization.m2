newPackage (
     "Serialization",
     DebuggingMode => true,
     Headline => "reversible conversion of all Macaulay2 objects to strings")

-- this code is re-entrant

export { "serialize", "reload" }

reload = Command ( x -> loadPackage ("Serialization",Reload => true) )

debug Core
    waterMark' = waterMark
    waterMarkSymbol = symbol waterMark
    getAttribute' = getAttribute
    hasAttribute' = hasAttribute
    PrintNet' = PrintNet
    PrintNames' = PrintNames
    ReverseDictionary' = ReverseDictionary
dictionaryPath = delete(Core#"private dictionary",dictionaryPath)

s := x -> concatenate apply(deepSplice flatten deepSplice x, i -> if instance(i,ZZ) then ("o#",toString i) else i)

serialize = x -> (
     h := new MutableHashTable;	    -- objects in progress
     k := new MutableHashTable;				    -- serial numbers of objects
     k':= new MutableHashTable;				    -- inverse function of k
     code1 := new MutableHashTable;			    -- initialization code for everything, by index
     code2 := new MutableHashTable;			    -- finalization code for mutable objects, by index
     p := method(Dispatch => Thing);
     pp := f -> x -> (
	  if k#?x then return "o#" | toString k#x;
	  if h#?x then return "(error \"internal error\")";
     	  h#x = true;
	  t := f x;
     	  remove(h,x);
	  i := #k';
	  k'#i = x;
	  k#x = i;
	  r := "o#" | toString i;
	  if instance(t,String) then code1#i = r | "=" | t;
	  r);
     q := method(Dispatch => Thing);
     qq := f -> x -> (
	  t := f x;
	  if instance(t,String) then code2#(k#x) = t;
	  );
     -- remember to remove these methods later, to prevent a memory leak:
     p Thing := pp (x -> (
	       if mutable x then 
	       if hash x < waterMark' then 
	       if hasAttribute'(x,ReverseDictionary')
	       then toString getAttribute'(x,ReverseDictionary')
	       else error "serialize: encountered an older mutable object not assigned to a global variable"
	       else error "serialize: encountered a recent mutable object with no known serialization method"
	       else toExternalString x));
     q Thing := x -> null; 
     p Function := pp (x -> (
	       if hash x < waterMark' then
	       if hasAttribute'(x,ReverseDictionary')
	       then toString getAttribute'(x,ReverseDictionary')
	       else error "serialize: encountered an older function not assigned to a global variable"
	       else error "serialize: encountered a recent function; functions cannot be serialized yet"
	       ));
     p Symbol := pp (x -> (
	       if hash x < waterMark'
	       then "symbol " | toString x
	       else if value x =!= x
	       then "(symbol " | toString x | " <- " | p value x | ";" | "symbol " | toString x | ")"
	       else "symbol " | toString x
	       ));
     p BasicList := pp(x -> (
	       if class x === List
	       then s("{",between_"," apply(toList x,p),"}")
	       else if class x === Array
	       then s("[",between_"," apply(toList x,p),"]")
	       else s("newClass(",p class x,",{",between_"," apply(toList x,p),"})")));
     p HashTable := pp(x -> (
	       if parent x =!= Nothing
	       then s("newClass(", p class x,",", p parent x,",", "hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"})" )
	       else if class x =!= HashTable
	       then s("newClass(", p class x,",", "hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"})" )
	       else s("hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"}" )));
     p MutableList := pp (x -> (
	       if mutable x and hash x < waterMark' and hasAttribute'(x,ReverseDictionary') 
	       then toString getAttribute'(x,ReverseDictionary')
	       else ( scan(x,p); "newClass(" | p class x | ",{})")));
     q MutableList := qq (x -> concatenate between_"\n" for i from 1 to #x list ( j := #x-i; px := p x ; px | "#" | toString j | "=" | p x#j ));
     p MutableHashTable := pp (x -> (
	       if mutable x and hash x < waterMark' and hasAttribute'(x,ReverseDictionary') 
	       then toString getAttribute'(x,ReverseDictionary')
	       else (
		    scan(pairs x,(k,v) -> (p k; p v));
		    if parent x =!= Nothing
		    then "newClass(" | p class x | "," | p parent x | ",hashTable{})"
		    else "newClass(" | p class x | ",hashTable{})")));
     p GlobalDictionary := pp (x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary')
	       then toString getAttribute'(x,ReverseDictionary')
	       else (
		    scan(pairs x,(k,v) -> (p k; p v));
		    "new Dictionary")));
     p x;
     k = newClass(HashTable,k);
     k' = newClass(HashTable,k');
     code1 = newClass(HashTable,code1);
     scanKeys(k,q);
     code2 = newClass(HashTable,code2);
     assert Thing#?p; remove(Thing,p);
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
aa = 1234; x = new MutableList; y = new MutableHashTable; x#0 = y; x#1 = x; x#2 = 14; y#x = {4,[5,[6]]}; y#4 = x; y#y = hashTable{symbol aa=>4,b=>44};
serialize y
