--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

needs "methods.m2"
needs "lists.m2"

-- TODO: are these used in the documentation?
-- TODO: how to find all missing keywords?
-- TODO: is there a better place for these?
typicalValues#(symbol timing) = Time
typicalValues#(symbol elapsedTiming) = Time
typicalValues#(symbol threadLocal) = Symbol
typicalValues#(symbol local) = Symbol
typicalValues#(symbol global) = Symbol
typicalValues#(symbol symbol) = Symbol
typicalValues#(symbol ?) = Boolean

dummy := x -> error("dummy method function called")

append(BasicList,Thing) := BasicList => append
prepend(Thing,BasicList) := BasicList => prepend

atEndOfFile(File) := Boolean => atEndOfFile
echoOff File := Nothing => echoOff
echoOn File := Nothing => echoOn
fileMode(File) := fileMode
fileMode(String) := ZZ => fileMode
fileMode(ZZ,File) := fileMode
fileMode(ZZ,String) := fileMode
get File := get String := String => get
getc File := String => getc
isInputFile(File) := Boolean => isInputFile
isListener(File) := Boolean => isListener
isOpen(Database) := Boolean => isOpen
isOpen(File) := Boolean => isOpen
isOutputFile(File) := Boolean => isOutputFile
linkFile(String,String) := Nothing => linkFile
openIn String := File => openIn
openInOut String := openInOut File := File => openInOut
openListener String := File => openListener
openOut String := File => openOut
openOutAppend String := File => openOutAppend
kill File := Nothing => kill
kill ZZ := Nothing => kill
read File := String => read
read(File,ZZ) := String => read
read Sequence := String => read
read String := String => read
getenv String := String => getenv

deepSplice BasicList := BasicList => deepSplice
drop(BasicList,ZZ) := drop(BasicList,List) := BasicList => drop
take(BasicList,ZZ) := take(BasicList,List) := BasicList => take
take(Thing,ZZ) := take(Thing,List) := List => take

isMutable(Thing) := Boolean => isMutable
remove(MutableList,ZZ) := Nothing => remove
remove(Database,String) := Nothing => remove
remove(HashTable,Thing) := Nothing => remove
openDatabase String := Database => openDatabase
openDatabaseOut String := Database => openDatabaseOut

substring(String,ZZ) := String => substring
substring(String,ZZ,ZZ) := String => substring
substring(ZZ,String) := String => substring
substring(Sequence,String) := String => substring
substring(ZZ,ZZ,String) := String => substring
toSequence BasicList :=
toSequence String    :=
toSequence Thing     := Sequence => toSequence
lines(String,String) := List => lines
lines String := List => lines

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

-----------------------------------------------------------------------------
-- This function generates the file "tvalues.m2" from the files ../d/*.d
-- The file "tvalues.m2" should be distributed with binary distributions

typicalValuesSource := prefixDirectory | replace("PKG", "Core", currentLayout#"package") | "tvalues.m2"
typicalValuesFormat := "-- # typical value: *(.*?), ([^ ]*) *"

generateTypicalValues = (srcdir) -> (
    if not fileExists srcdir then error "unable to find the source code for the interpreter";
    printerr("Extracting typical values from ", relativizeFilename srcdir);
    ddfiles := select(readDirectory srcdir, file -> match("\\.dd?$", file));
    printerr("Generating typical values in ", relativizeFilename typicalValuesSource);
    outfile := openOut typicalValuesSource;
    for file in sort ddfiles do (
	try src := get(srcdir | file) else continue;
	comment := "-- typical values extracted from " | file;
	srcstring := stack apply(pairs lines src, (num, line) -> line | " -- " | file | ":" | num);
	-- TODO: separate method key (\\1) and output type (\\2)
	extracted := select(typicalValuesFormat | " -- (.*)$", "typval(\\1, \\2) -- \\3", toString srcstring);
	extracted  = apply(extracted, line -> first select("(.*?)--(.*?)$", "\\1" | pad_(91-#line) "\t-- \\2", line));
	if 0 < #extracted then outfile << comment << endl << stack extracted << endl);
    outfile << "-- DONE: generated based on " | version#"git description" << endl << close)

-- if missing or not successfully generated, tvalues.m2 is regenerated directly
if not fileExists typicalValuesSource or not match("-- DONE", get typicalValuesSource)
then generateTypicalValues(currentFileDirectory | "../d/")

-----------------------------------------------------------------------------
-- numerical functions that will be wrapped
redefs := hashTable apply({acos, agm, asin, atan, atan2, Beta, cos, cosh, cot, coth, csc, csch, Digamma, eint, erf, erfc, exp, expm1, Gamma, inverseErf, inverseRegularizedBeta, inverseRegularizedGamma, log, log1p, regularizedBeta, regularizedGamma, sec, sech, sin, sinh, sqrt, tan, tanh, zeta},
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
	 if args === sequence RR then variants#(f,Number) = f' @@ numeric
	 else if #args === 2 then (
	     if args#0 === RR then variants#(f,Number,args#1) = (x,y) -> f'(numeric_(precision y) x,y);
	     if args#1 === RR then variants#(f,args#0,Number) = (x,y) -> f'(x,numeric_(precision x) y);
	     if args === (RR,RR) then variants#(f,Number,Number) = (x,y) -> f'(numeric x,numeric y); -- phew
	     )
	 else if #args === 3 then (
	     if args#0 === RR then
		 variants#(f, Number,   args#1, args#2) =
		 (x,y,z) -> f'(numeric(min(precision y, precision z), x), y, z);
	     if args#1 === RR then
		 variants#(f, args#0, Number,   args#2) =
		 (x,y,z) -> f'(x, numeric(min(precision x, precision z), y), z);
	     if args#2 === RR then
		 variants#(f, args#0, args#1, Number)   =
		 (x,y,z) -> f'(x, y, numeric(min(precision x, precision y), z));
	     if args#0 === RR and args#1 === RR then
		 variants#(f, Number,   Number,   args#2) =
		 (x,y,z) ->
		     f'(numeric_(precision z) x, numeric_(precision z) y, z);
	     if args#0 === RR and args#2 === RR then
	         variants#(f, Number,   args#1, Number)   =
		 (x,y,z) ->
		     f'(numeric_(precision y) x, y, numeric_(precision y) z);
	     if args#1 === RR and args#2 === RR then
		 variants#(f, args#0, Number,   Number)   =
		 (x,y,z) ->
		     f'(x, numeric_(precision x) y, numeric_(precision x) z);
	     if args === (RR, RR, RR) then
		 variants#(f, Number,   Number,   Number)   =
		 (x,y,z) -> f'(numeric x, numeric y, numeric z);
	     );
	 )
     )

-- tvalues.m2 is loaded here
load typicalValuesSource

scanPairs(redefs, (k,v) -> globalAssign(baseName k,v))
scanPairs(new HashTable from variants, (args,f) -> (
	installMethod append(args,f);
	undocumented' args;
	))

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
