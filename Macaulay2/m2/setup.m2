--		Copyright 1993-2003 by Daniel R. Grayson

if class Manipulator =!= Symbol then ( 
     printString(stderr, "warning: skipping setup.m2, already loaded\n"); flush stderr; 
     endInput )

if class Manipulator =!= Symbol then error "setup.m2 already loaded"

Symbols = new MutableHashTable
Function.GlobalAssignHook = (X,x) -> (
     if not Symbols#?x then Symbols#x = X;
     )
Function.GlobalReleaseHook = (X,x) -> (
     -- error concatenate("warning: ", X, " redefined");	    -- provisional, see definition below
     if Symbols#x === X then remove(Symbols,x);
     )
addStartFunction(
     () -> (
	  Function.GlobalReleaseHook = (X,x) -> (
	       if not writableGlobals#?X then stderr << "warning: " << toString X << " redefined" << endl;
	       if Symbols#x === X then remove(Symbols,x);
	       );
	  )
     )

-- manipulators

Manipulator = new Type of BasicList
Manipulator.synonym = "manipulator"
new Manipulator from Function := Manipulator => (Manipulator,f) -> new Manipulator from {f}
Manipulator.name = "Manipulator"
Manipulator Database := Manipulator File := (m,o) -> m#0 o

Manipulator.GlobalAssignHook = (X,x) -> if not Symbols#?x then Symbols#x = X
Manipulator.GlobalReleaseHook = (X,x) -> if Symbols#x === X then remove(Symbols,x)

Manipulator Nothing := (m,null) -> null
File << Manipulator := File => (o,m) -> m#0 o
List << Manipulator := File => (o,m) -> (scan(o, o -> m#0 o); o)
Nothing << Manipulator := (null,m) -> null

close = new Manipulator from simpleClose; erase symbol simpleClose
closeIn = new Manipulator from simpleCloseIn; erase symbol simpleCloseIn
closeOut = new Manipulator from simpleCloseOut; erase symbol simpleCloseOut
flush = new Manipulator from simpleFlush; erase symbol simpleFlush
endl = new Manipulator from simpleEndl; erase symbol simpleEndl

---------------------------------

if notify then stderr << "--loading setup.m2" << endl

match := X -> 0 < #(matches X)				    -- defined as a method later

if class phase === Symbol then phase = 0

erase symbol "--newline--"

protect AfterEval
protect AfterPrint
protect BeforePrint

rot := x -> (
     symbol oooo <- ooo;			  -- avoid GlobalAssignHook with <-
     symbol ooo <- oo;
     symbol oo <- x;
     )

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

outputSymbols = new MutableHashTable

outputLabel := ""

commonProcessing := x -> (
     outputLabel = concatenate(interpreterDepth():"o",string lineNumber());
     x = applyMethod(AfterEval,x);
     if x =!= null then (
     	  s := getGlobalSymbol outputLabel;
     	  outputSymbols#s = true;
     	  s <- x;
	  );
     rot x;
     x
     )

Thing.Print = x -> (
     x = commonProcessing x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl;			  -- double space
	  << outputLabel << " = " << (try net y else try string y else "--something--") << endl;
	  );
     applyMethod(AfterPrint,x);
     )

Thing.NoPrint = x -> (
     x = commonProcessing x;
     applyMethod(AfterNoPrint,x);
     )

loaded := new MutableHashTable
unmarkAllLoadedFiles = () -> loaded = new MutableHashTable  -- symbol will be erased in debugging.m2

markLoaded := (filename,origfilename) -> ( 
     loaded#origfilename = true; 
     if notify then (
	  filename = minimizeFilename filename;
	  stderr << "--loaded " << filename << endl
	  );
     )

isSpecial := filename -> filename#0 === "$" or filename#0 === "!"

tryload := (filename,loadfun) -> (
     if isAbsolutePath filename or isSpecial filename then (
	  if not fileExists filename then return false;
	  loadfun filename;
	  markLoaded(filename,filename);
	  true)
     else (
          if class path =!= List then error "expected 'path' to be a list (of strings)";
          {} =!= select(1,
	       if currentFileDirectory == "--startupString--/" then path
	       else prepend(currentFileDirectory, path),
	       dir -> (
		    if class dir =!= String then error "member of 'path' not a string";
		    fullfilename := dir | filename;
		    if not fileExists fullfilename then return false;
		    loadfun fullfilename;
		    markLoaded(fullfilename,filename);
		    true))))

load = (filename) -> (
     if not tryload(filename,simpleLoad) then error ("can't open file ", filename)
     )

input = (filename) -> (
     savenotify := notify;
     notify = false;
     if not tryload(filename,simpleInput) then error ("can't open file ", filename);
     notify = savenotify;
     )
erase symbol simpleInput

setnotify := () -> (
     -- notify = true			-- notify after initialization, commented out
     )
-- erase symbol notify    -- we have to erase this while we have a chance

needs = s -> if not loaded#?s then load s

new HashTable from List := HashTable => (O,v) -> hashTable v

load "loads.m2"
-- don't define any global variables below this point
setnotify()
addStartFunction(
     () -> (
	  loadDepth (1 + loadDepth());
	  errorDepth loadDepth();
	  if not member("-q",commandLine)
	  then (
	       tryload("init.m2", simpleLoad)
	       or
	       getenv "HOME" =!= "" and (
		    tryload(getenv "HOME" | "/init.m2", simpleLoad)
		    or
		    tryload(getenv "HOME" | "/.init.m2", simpleLoad)))))
erase symbol simpleLoad
newPackage User
protect Macaulay2Dictionary
