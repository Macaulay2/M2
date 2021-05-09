--		Copyright 1993-2003 by Daniel R. Grayson

nonnull = x -> select(x, i -> i =!= null)
nonempty = x -> select(x, i -> i =!= "")
dashes  = n -> concatenate (n:"-")
spaces  = n -> concatenate n

-- a first-in last-out list of symbol values
varstack = new MutableHashTable
pushvar = (sym,newval) -> (
     varstack#sym = if varstack#?sym then (value' sym,varstack#sym) else (value' sym, null);
     sym <- newval;
     )
popvar = (sym) -> if varstack#?sym then (
     c := varstack#sym;
     if c === null then error "internal error: empty stack";
     sym <- c#0;
     varstack#sym = c#1;
     )

SelfInitializingType = new Type of Type
SelfInitializingType.synonym = "self initializing type"
SelfInitializingType Thing := (T,z) -> new T from z

SelfInitializingType\VisibleList := (T,z) -> (i -> T i) \ z
List/SelfInitializingType := VisibleList/SelfInitializingType := (z,T) -> z / (i -> T i)
SelfInitializingType \\ Thing := (T,z) -> T z
Thing // SelfInitializingType := (z,T) -> T z

Bag = new SelfInitializingType of MutableList
Bag.synonym = "bag"
Bag ? Bag := (x,y) -> incomparable			    -- so we can sort with them

sortBy = f -> v -> last @@ last \ sort \\ (i -> (f i, new Bag from {i})) \ v
sortByName = x -> (sortBy toString) x
sortByHash = sortBy hash

centerString = (wid,s) -> (
     n := width s;
     if n === wid then s
     else (
     	  w := (wid-n+1)//2;
     	  horizontalJoin(spaces w,s,spaces(wid-w-n))))

if class oooo =!= Symbol then error "setup.m2 already loaded"

if class RawMutableMatrix =!= Type then error "where is RawMutableMatrix?"

OutputDictionary = new Dictionary
dictionaryPath = append(dictionaryPath,OutputDictionary)

-- references to the symbol 'User' occur before the corresponding package has been created...
getGlobalSymbol(PackageDictionary,"User")

-- the former behavior
-- getSymbol = nm -> (
--      if isGlobalSymbol nm then return getGlobalSymbol nm;
--      for d in dictionaryPath do if mutable d then return getGlobalSymbol(d,nm);
--      error "no mutable dictionary on path";
--      )

getSymbol = s -> (
     if instance(User,Symbol) then error "getSymbol used before package User created";
     getGlobalSymbol(User#"private dictionary", s))

if notify then stderr << "--loading " << minimizeFilename currentFileName << endl

match := X -> null =!= regex X -- defined as a method later

somethingElse = () -> error "something else needs to be implemented here"

rotateOutputLines := x -> (
     if ooo =!= null then global oooo <- ooo;
     if oo =!= null then global ooo <- oo;
     if x =!= null then global oo <- x;
     )

Thing#AfterEval = x -> (
     if x =!= null then (
     	  s := getGlobalSymbol(OutputDictionary,concatenate(interpreterDepth:"o",simpleToString lineNumber));
     	  s <- x;
	  );
     rotateOutputLines x;
     x)

Nothing#{Standard,Print} = identity

binaryOperators = join(fixedBinaryOperators,flexibleBinaryOperators)
prefixOperators = join(fixedPrefixOperators,flexiblePrefixOperators)
postfixOperators = join(fixedPostfixOperators,flexiblePostfixOperators)
flexibleOperators = join(flexibleBinaryOperators,flexiblePrefixOperators,flexiblePostfixOperators)
fixedOperators = join(fixedBinaryOperators,fixedPrefixOperators,fixedPostfixOperators)
allOperators = join(fixedOperators,flexibleOperators)

undocumentedkeys = new MutableHashTable
undocumented' = key -> undocumentedkeys#key = true

pathdo := (loadfun,path,filename,reportfun) -> (
     ret := null;
     if class filename =!= String then error "expected a string";
     newpath := (
	  if isStablePath filename then {
	       singledir := if isAbsolutePath filename then "" else currentFileDirectory
	       }
	  else path
	  );
     if null === scan(newpath, dir -> (
	       if class dir =!= String then error "member of 'path' not a string";
	       fullfilename := concatenate(dir, if dir#?0 and dir#-1 =!= "/" then "/", filename);
	       if debugLevel === 1011 then stderr << "checking for file " << fullfilename << endl;
	       if fileExists fullfilename then (
	       	    if debugLevel === 1011 then stderr << "found it" << endl;
		    filetime := fileTime fullfilename;
		    ret = loadfun fullfilename;
		    reportfun (fullfilename,filetime);
		    break true)
	       else (
	       	    if debugLevel === 1011 then stderr << "didn't find it" << endl;
		    )))
     then error splice("file not found",
	  if singledir === "" then ""
	  else if singledir =!= null then (" in \"",singledir,"\"")
	  else " on path",
	  ": \"", filename, "\"");
     ret)

tryload := (filename,loadfun,notify) -> pathdo(loadfun,path,filename, (fullfilename,filetime) -> markLoaded(fullfilename,filename,notify,filetime))
load = (filename) -> (
     -- could assert something here ...
     tryload(filename,simpleLoad,notify);
     )
input = (filename) -> (tryload(filename,simpleInput,false);)
needs = s -> if not filesLoaded#?s then load s else (
     (fullfilename,filetime) := filesLoaded#s;
     if filetime < fileTime fullfilename then load fullfilename)

lastLN := 0
lastWI := 0
     
loads := minimizeFilename concatenate(currentFileDirectory, "loadsequence")
if notify then stderr << "--about to read " << loads << endl
scan(select("^\\w+\\.m2", "$&", get loads), load)
if notify then stderr << "--read " << loads << endl

-- after this point, private global symbols, such as noinitfile, are no longer visible
protect Core.Dictionary
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
