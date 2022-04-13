--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

needs "methods.m2"
needs "lists.m2"

typicalValues#class = Type
typicalValues#parent = Type
typicalValues#(symbol timing) = Time
typicalValues#(symbol local) = Symbol
typicalValues#(symbol global) = Symbol
typicalValues#(symbol symbol) = Symbol
typicalValues#(symbol ?) = Boolean

dummy := x -> error("dummy method function called")
installMethod(symbol #?, HashTable, Thing, Boolean => x -> (dummy x;))
installMethod(symbol #?, Set, Thing, Boolean => x -> (dummy x;))
installMethod(symbol #?, Database, String, Boolean => x -> (dummy x;))
installMethod(symbol #?, BasicList, ZZ, Boolean => x -> (dummy x;))
installMethod(symbol #?, String, ZZ, Boolean => x -> (dummy x;))
installMethod(symbol #, HashTable, Thing, Thing => x -> (dummy x;))
installMethod(symbol #, Database, String, String => x -> (dummy x;))
installMethod(symbol #, String, ZZ, Thing => x -> (dummy x;))
installMethod(symbol #, BasicList, ZZ, Thing => x -> (dummy x;))

-- installMethod(symbol #, File, ZZ => x -> (dummy x;))
installMethod(symbol #, Set, ZZ => x -> (dummy x;))
installMethod(symbol #, HashTable, ZZ => x -> (dummy x;))
installMethod(symbol #, BasicList, ZZ => x -> (dummy x;))
installMethod(symbol #, String, ZZ => x -> (dummy x;))

installMethod(symbol <, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol <=, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol =!=, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol ===, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol =>, Thing, Thing, Option => x -> (dummy x;))
installMethod(symbol >, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol >=, Thing, Thing, Boolean => x -> (dummy x;))

installMethod(symbol .., ZZ, ZZ, Sequence => x -> (dummy x;))
installMethod(symbol ..<, ZZ, ZZ, Sequence => x -> (dummy x;))
ancestor(Type,Type) := Boolean => ancestor
any(ZZ,Function) := any(BasicList,Function) := any(BasicList,BasicList,Function) := any(HashTable,Function) := Boolean => any
append(BasicList,Thing) := BasicList => append
prepend(Thing,BasicList) := BasicList => prepend
apply(BasicList,Function) := BasicList => apply
apply(BasicList,BasicList,Function) := BasicList => apply
apply(String,Function) := Sequence => apply
apply(BasicList,String,Function) := Sequence => apply
apply(String,BasicList,Function) := Sequence => apply
apply(String,String,Function) := Sequence => apply
apply(ZZ,Function) := List => apply
applyKeys(HashTable,Function) := HashTable => applyKeys
applyKeys(HashTable,Function,Function) := HashTable => applyKeys
applyPairs(HashTable,Function) := HashTable => applyPairs
applyValues(HashTable,Function) := HashTable => applyValues
atEndOfFile(File) := Boolean => atEndOfFile
isInputFile(File) := Boolean => isInputFile
isListener(File) := Boolean => isListener
isOpen(File) := Boolean => isOpen
isOpen(Database) := Boolean => isOpen
isOutputFile(File) := Boolean => isOutputFile
mutable(Thing) := Boolean => mutable
instance(Thing,Type) := Boolean => instance
concatenate Nothing := concatenate String := concatenate Symbol := concatenate ZZ := concatenate BasicList := String => concatenate
deepSplice BasicList := BasicList => deepSplice
drop(BasicList,ZZ) := drop(BasicList,List) := BasicList => drop
take(BasicList,ZZ) := take(BasicList,List) := BasicList => take
get File := get String := String => get
getc File := String => getc
getenv String := String => getenv
hashTable List := HashTable => hashTable
typicalValues#horizontalJoin = Net
horizontalJoin BasicList := Net => horizontalJoin
unstack Net := List => unstack
localDictionaries Function := List => localDictionaries
localDictionaries Symbol := List => localDictionaries
localDictionaries Pseudocode := List => localDictionaries
localDictionaries Dictionary := List => localDictionaries
values HashTable := List => values
merge(HashTable,HashTable,Function) := HashTable => merge
mergePairs(BasicList,BasicList,Function) := BasicList => mergePairs
mingle BasicList := List => mingle
openDatabase String := Database => openDatabase
openDatabaseOut String := Database => openDatabaseOut
openIn String := File => openIn
openOut String := File => openOut
openOutAppend String := File => openOutAppend
openInOut String := openInOut File := File => openInOut
openListener String := File => openListener
pack(BasicList,ZZ) := List => pack
pack(ZZ,BasicList) := List => pack
pack(String,ZZ) := List => pack
pack(ZZ,String) := List => pack
reverse BasicList := BasicList => reverse
reverse String := String => reverse
set VisibleList := Set => set
tally VisibleList := Tally => tally
tally String := Tally => tally
splice BasicList := BasicList => splice
typicalValues#stack = Net
stack BasicList := Net => stack
substring(String,ZZ) := String => substring
substring(String,ZZ,ZZ) := String => substring
substring(ZZ,String) := String => substring
substring(Sequence,String) := String => substring
substring(ZZ,ZZ,String) := String => substring
toList Set := toList BasicList := toList String := List => toList
toSequence BasicList := toSequence String := Sequence => toSequence
ascii String := List => ascii
ascii List := String => ascii
remove(HashTable,Thing) := Nothing => remove
echoOff File := Nothing => echoOff
echoOn File := Nothing => echoOn

-- close File := File =>
--      f -> close f		    -- this code is bypassed by built-in compiled code

-- closeIn File := File =>
--      f -> closeIn f		    -- this code is bypassed by built-in compiled code
-- closeOut File := File =>
--      f -> closeOut f		    -- this code is bypassed by built-in compiled code

kill File := Nothing => kill
kill ZZ := Nothing => kill
read File := String => read
read (File,ZZ) := String => read
read Sequence := String => read
read String := String => read
Function Thing := Thing => x -> (dummy x;)
scan(BasicList,Function) := scan(String,Function) := Nothing => scan
scan(BasicList,BasicList,Function) := Nothing => scan
scan(ZZ,Function) := Nothing => scan
scanPairs(HashTable,Function) := Nothing => scanPairs
lines(String,String) := List => lines
lines String := List => lines
linkFile(String,String) := Nothing => linkFile
fileMode(String) := ZZ => fileMode
fileMode(ZZ,File) := fileMode
fileMode(File) := fileMode
fileMode(ZZ,String) := fileMode
frames(Sequence) := frames
frames(Symbol) := frames
frames(Function) := frames
frames(Pseudocode) := frames
powermod(ZZ,ZZ,ZZ) := ZZ => powermod

chk := (type,key) -> if type#?key then (
     stderr << "-- method already installed:" << endl
     << "   " << code type#key << endl;
     error("method already installed: ",toString type," # ",toString key))


typval3f-*(Function,Type,Type)*- := (f,X,Z) -> (
     msg := toString f | "(" | toString X | ") => " | toString Z;
     chk(X, f);
     f(X) := Z => x -> (error("dummy method called: ", msg);)
     )
typval4f-*(Function,Type,Type,Type)*- := (f,X,Y,Z) -> (
     chk(youngest (X,Y), (f,X,Y));
     f(X,Y) := Z => x -> (dummy x;)
     )
typval5f-*(Function,Type,Type,Type,Type)*- := (f,X,Y,Z,W) -> (
     chk(youngest (X,Y,Z), (f,X,Y,Z));
     f(X,Y,Z) := W => x -> (dummy x;)
     )
typval3k-*(Keyword,Type,Type)*- := (f,X,Z) -> (
     chk(X, (f,X));
     installMethod(f, X, Z => x -> (dummy x;))
     )
typval4k-*(Keyword,Type,Type,Type)*- := (f,X,Y,Z) -> (
     chk(youngest(X,Y), (f,X,Y));
     installMethod(f, X, Y, Z => x -> (dummy x;))
     )

if member("--no-tvalues", commandLine) then end

-- numerical functions that will be wrapped
redefs := hashTable apply({acos, agm, asin, atan, atan2, BesselJ, BesselY, Beta, cos, cosh, cot, coth, csc, csch, Digamma, eint, erf, erfc, exp, expm1, Gamma, incompleteGamma, log, log1p, sec, sech, sin, sinh, sqrt, tan, tanh, zeta},
    f -> f => method());
variants := new MutableHashTable;

typval = x -> (
     if #x == 3 then (
	  if instance(x#0,Function) then typval3f x
	  else if instance(x#0,Keyword) then typval3k x
	  else error "typval: expected keyword or function"
	  )
     else if #x == 4 then (
	  if instance(x#0,Function) then typval4f x
	  else if instance(x#0,Keyword) then typval4k x
	  else error "typval: expected keyword or function"
	  )
     else if #x == 5 then typval5f x
     else error "typval: expected 3, 4, or 5 arguments";
     if redefs#?(x#0) then (
	 f' := x#0;
	 f := redefs#f';
	 args := drop(drop(x,-1),1);
	 installMethod append(prepend(f,args),last x => f');
	 if args === sequence RR then variants#(f,Number) = variants#(f,Constant) = f' @@ numeric
	 else if #args === 2 then (
	     if args#0 === RR then variants#(f,Number,args#1) = variants#(f,Constant,args#1) = (x,y) -> f'(numeric_(precision y) x,y);
	     if args#1 === RR then variants#(f,args#0,Number) = variants#(f,args#0,Constant) = (x,y) -> f'(x,numeric_(precision x) y);
	     if args === (RR,RR) then variants#(f,Number,Number) = variants#(f,Number,Constant) = variants#(f,Constant,Number) = variants#(f,Constant,Constant) = (x,y) -> f'(numeric x,numeric y); -- phew
	     );
	 )
     )

load "tvalues.m2"

scanPairs(redefs, (k,v) -> globalAssign(baseName k,v))
scanPairs(new HashTable from variants, (args,f) -> (
	installMethod append(args,f);
	undocumented args;
	))

-- TODO abs Constant

nilp := x -> (  -- degree of nilpotency
    R := ring x;
    k := R; while not isField k do k = baseRing k;
    f := map(R,k(monoid [getSymbol "X"]),{x});
    I := kernel f;
    if I == 0 or (l:=listForm I_0; #l>1) then infinity else l#0#0#0
    )

taylor := (f,g) -> f RingElement := x -> (
    try promote(f lift(x,RR),ring x)
    else try promote(f lift(x,CC),ring x)
    else (
	n := try nilp x else error (toString f | ": expected an algebra over QQ"); -- by now this is incorrect; e.g., ZZ/p allowed
	if n === infinity then error (toString f | ": undefined");
	g(x,n)
	)
    )

taylor (exp, (x,n) -> (
	s := 1; xx := 1;
	for k from 1 to n-1 do (
            xx = (1/k)*xx*x;
            s = s + xx;
            );
	s
	))

taylor (expm1, (x,n) -> (
	s := 0; xx := 1;
	for k from 1 to n-1 do (
            xx = (1/k)*xx*x;
            s = s + xx;
            );
	s
	))

sintaylor := (x,n) -> (
    s := x; xx := x;
    k := 3;
    while k<n do (
        xx = -(1/k/(k-1))*xx*x^2;
	s = s + xx;
	k=k+2;
        );
    s
    )
taylor (sin, sintaylor)

costaylor := (x,n) -> (
    s := 1; xx := 1;
    k := 2;
    while k<n do (
        xx = -(1/k/(k-1))*xx*x^2;
	s = s + xx;
	k=k+2;
        );
    s
    )
taylor (cos, costaylor)

taylor (tan, (x,n) -> sintaylor(x,n) * (costaylor(x,n))^-1)
taylor (sec, (x,n) -> (costaylor(x,n))^-1)

sinhtaylor := (x,n) -> (
    s := x; xx := x;
    k := 3;
    while k<n do (
        xx = (1/k/(k-1))*xx*x^2;
	s = s + xx;
	k=k+2;
        );
    s
    )
taylor (sinh, sinhtaylor)

coshtaylor := (x,n) -> (
    s := 1; xx := 1;
    k := 2;
    while k<n do (
        xx = (1/k/(k-1))*xx*x^2;
	s = s + xx;
	k=k+2;
        );
    s
    )
taylor (cosh, coshtaylor)

taylor (tanh, (x,n) -> sinhtaylor(x,n) * (coshtaylor(x,n))^-1)
taylor (sech, (x,n) -> (coshtaylor(x,n))^-1)

taylor (asin, (x,n) -> (
	s := x; xx := x;
	k := 3;
	while k<n do (
            xx = (k-2)/(k-1)*xx*x^2;
	    s = s + xx/k;
	    k=k+2;
            );
	s
	))

taylor (atan, (x,n) -> (
	s := x; xx := x;
	k := 3;
	while k<n do (
            xx = -xx*x^2;
	    s = s + xx/k;
	    k=k+2;
            );
	s
	))

taylor (log1p, (x,n) -> (
	s:=x; xx := x;
	k := 2;
	while k<n do (
        xx = -xx*x;
	s = s + xx/k;
	k=k+1;
        );
    s
    ))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
