newPackage (
     "Serialization",
     DebuggingMode => true,
     Headline => "reversible conversion of all Macaulay2 objects to strings")

-- this code is re-entrant

export { "serialize", "reload" }

reload = Command ( x -> loadPackage ("Serialization",Reload => true) )

debug Core
    generatorSymbols' = generatorSymbols
    waterMark' = waterMark
    waterMarkSymbol = symbol waterMark
    Attributes' = Attributes
    setAttribute' = setAttribute
    getAttribute' = getAttribute
    hasAttribute' = hasAttribute
    PrintNet' = PrintNet
    PrintNames' = PrintNames
    ReverseDictionary' = ReverseDictionary
dictionaryPath = delete(Core#"private dictionary",dictionaryPath)

w := x -> (scan(x,i -> assert (i =!= "")); x)

serializable = set toList (symbol a .. symbol Z)

serialize = x -> (
     h := new MutableHashTable;	    -- objects in progress
     k := new MutableHashTable;				    -- serial numbers of objects
     k':= new MutableHashTable;				    -- inverse function of k
     code1 := new MutableHashTable;			    -- initialization code for everything, by index
     code2 := new MutableHashTable;			    -- finalization code for mutable objects, by index
     p := method(Dispatch => Thing);
     q := method(Dispatch => Thing);
     pp := (X,f) -> (
	  if not X#?q then q X := x -> null;
	  p X := x -> (     -- remember to remove these methods later, to prevent a memory leak:
	       if debugLevel > 0 then (
		    stderr << "-- pp " << X << " ( " << f << " , " << x << ")" << endl;
		    );
	       if k#?x then return "s" | toString k#x;
	       if h#?x then return "(error \"internal error\")";
	       h#x = true;
	       t := f x;
	       assert( t =!= "" );
	       remove(h,x);
	       i := #k';
	       k'#i = x;
	       k#x = i;
	       r := "s" | toString i;
	       if instance(t,Sequence) then (
		    local f;
		    (t,f) = t;
		    f();
		    );
	       if instance(t,String) then (
		    if debugLevel > 0 then try t = t | " -- " | toString x;
		    code1#i = r | ":=" | t;
		    );
	       if hash x > waterMark' and hasAttribute'(x,ReverseDictionary') then p getAttribute'(x,ReverseDictionary');
	       r));
     qq := (X,f) -> (
	  q X := x -> (     -- remember to remove these methods later, to prevent a memory leak:
	       if debugLevel > 0 then (
		    stderr << "-- qq " << X << " ( " << f << " , " << x << ")" << endl;
		    );
	       t := f x;
	       if hash x > waterMark' and hasAttribute'(x,ReverseDictionary') then (
		    u := "globalAssignFunction(" | p getAttribute'(x,ReverseDictionary') | "," | p x | ")";
		    if t === null then t = u else t = t | "\n" | u;
		    );
	       if t =!= null then code2#(k#x) = t;
	       ));
     pp(Thing, x -> (
	       if mutable x then 
	       if hash x < waterMark' then 
	       if hasAttribute'(x,ReverseDictionary')
	       then toString getAttribute'(x,ReverseDictionary')
	       else error "serialize: encountered an older mutable object not assigned to a global variable"
	       else error "serialize: encountered a recent mutable object with no known serialization method"
	       else toExternalString x));
     qq(Thing, x -> null); 
     pp(Function, x -> (
	       if hasAttribute'(x,ReverseDictionary')
	       then toString getAttribute'(x,ReverseDictionary')
	       else error "serialize: encountered a function not assigned to a global variable"));
     pp(Symbol, x -> (
	       if value x =!= x and (hash x > waterMark' or serializable#?x) then p value x;
	       "global " | toString x));
     qq(Symbol, x -> if value x =!= x and (hash x > waterMark' or serializable#?x) then p x | "<-" | p value x);
     pp(BasicList, x -> (
	       if class x === List
	       then concatenate("{",between_"," apply(toList x,p),"}")
	       else if class x === Array
	       then concatenate("[",between_"," apply(toList x,p),"]")
	       else concatenate("newClass(",p class x,",{",between_"," apply(toList x,p),"})")));
     pp(Sequence, x -> (
	       if #x === 1 then "1:" | p x#0
	       else concatenate("(",between_"," apply(toList x,p),")")));
     pp(HashTable, x -> (
	       if parent x =!= Nothing
	       then concatenate("newClass(", p class x,",", p parent x,",", "hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"})" )
	       else if class x =!= HashTable
	       then concatenate("newClass(", p class x,",", "hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"})" )
	       else concatenate("hashTable {",between_"," apply(pairs x,(k,v) -> ("(",p k,",",p v,")")),"}" )));
     pp(MutableList, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then toString getAttribute'(x,ReverseDictionary')
	       else ( scan(x,p); "newClass(" | p class x | ",{})")));
     qq(MutableList, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then return;
	       concatenate between_"\n" w for i from 1 to #x list ( j := #x-i; px := p x ; px | "#" | toString j | "=" | p x#j )));
     pp(MutableHashTable, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then toString getAttribute'(x,ReverseDictionary')
	       else (
		    scan(pairs x,(k,v) -> (p k; p v));
		    if parent x =!= Nothing
		    then "newClass(" | p class x | "," | p parent x | ",hashTable{})"
		    else "newClass(" | p class x | ",hashTable{})")));
     qq(MutableHashTable, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then return;
	       concatenate between_"\n" w apply(pairs x,(k,v) -> (p x, "#", p k, "=", p v))
	       ));
     pp(GlobalDictionary, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then toString getAttribute'(x,ReverseDictionary')
	       else (
		    scan(pairs x,(k,v) -> (p k; p v; p value v));
		    "new Dictionary")));
     qq(GlobalDictionary, x -> (
	       if hash x < waterMark' and hasAttribute'(x,ReverseDictionary') then return;
	       concatenate between_"\n" w apply(pairs x,(nam,sym) -> (p x, "#", format nam, "=", p sym))
	       ));
     pp(Monoid, x -> toExternalString x);
     pp(PolynomialRing, x -> p coefficientRing x | " " | p monoid x);
     pp(QuotientRing, x -> p ambient x | "/" | p ideal x);
     pp(Ring, x -> (
	       if x#?generatorSymbols'
	       then (toExternalString x, () -> scan(x#generatorSymbols',p))
	       else toExternalString x));
     pp(Ideal, x -> (p ring x; toExternalString x));
     pp(Module, x -> (p ring x; toExternalString x));
     pp(RingElement, x -> (p ring x; toExternalString x));
     pp(Matrix, x -> concatenate("map(", p target x, ",", p source x, ",", toString entries x, ")"));
     p x;
     k = newClass(HashTable,k);
     k' = newClass(HashTable,k');
     code1 = newClass(HashTable,code1);
     scanKeys(k,q);
     code2 = newClass(HashTable,code2);
     assert Thing#?p; remove(Thing,p);
     if debugLevel == 101 then print netList {
	  {"objects by index  (k)",k},
	  {"indices by object (k')",k'},
	  {"code by index (code1)",code1},
	  {"code by index (code2)",code2}
	  };
     concatenate between_"\n" w flatten {
	  ///a:=value Core#"private dictionary"#"Attributes"///,
	  values code1,
	  last \ sort pairs code2,
	  p x
	  })

end
reload
restart
aa = "1234"; X = new Type of MutableList; x = new X; y = new MutableHashTable; x#0 = y; x#1 = x; x#2 = 14;
y#x = {44444,["5",[6]]}; y#4 = x; y#y = hashTable{symbol aa=>4,b=>44444};
aa
peek x
peek y
loadPackage "Serialization"
userSymbols()
serialize oo
"/tmp/y" << oo << close;
restart
value get "/tmp/y"
aa
peek x
peek y
X
