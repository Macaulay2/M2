--		Copyright 1993-2003 by Daniel R. Grayson

if class oooo =!= Symbol then error "setup.m2 already loaded"

--

PackageDictionary = new Dictionary
globalDictionaries = append(globalDictionaries,PackageDictionary)
getGlobalSymbol(PackageDictionary,"Macaulay2")

-----------

assert = x -> if not x then error "assertion failed"

addStartFunction(
     () -> (
	  Function.GlobalReleaseHook = (X,x) -> (
	       stderr << "warning: " << toString X << " redefined" << endl;
     	       remove(ReverseDictionary,x);
	       );
	  )
     )

---------------------------------

if notify then stderr << "--loading setup.m2" << endl

match := X -> 0 < #(matches X)				    -- defined as a method later

somethingElse = () -> error "something else needs to be implemented here"

oo <- ooo <- oooo <- null

rot := x -> (
     global oooo <- ooo;			  -- avoid GlobalAssignHook with <-
     global ooo <- oo;
     global oo <- x;
     )

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

OutputDictionary = new Dictionary
globalDictionaries = append(globalDictionaries,OutputDictionary)

commonProcessing := x -> (
     x = applyMethod(AfterEval,x);
     if x =!= null then (
     	  s := getGlobalSymbol(OutputDictionary,concatenate(interpreterDepth:"o",toString lineNumber));
     	  s <- x;
	  );
     rot x;
     x
     )

simpleToString := toString

debugError = identity

Thing.Print = x -> (
     x = commonProcessing x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl			  -- double space
	  << concatenate(interpreterDepth:"o") << lineNumber << " = " 
	  << (
	       try net y else (
		    global debugError <- x -> net y;
		    stderr << "--error in conversion of output to net: type 'debugError()' to see it; will try conversion to string" << endl << endl ;
		    try toString y else (
		    	 stderr << "--error in conversion of output to string" << endl << endl;
			 try simpleToString y else "--something unprintable--"))) << endl;
	  );
     applyMethod(AfterPrint,x);
     )

Thing.NoPrint = x -> (
     x = commonProcessing x;
     applyMethod(AfterNoPrint,x);
     )

loaded := new MutableHashTable
unmarkAllLoadedFiles = () -> loaded = new MutableHashTable  -- symbol will be erased in debugging.m2

markLoaded := (filename,origfilename,notify) -> ( 
     loaded#origfilename = true; 
     if notify then (
	  filename = minimizeFilename filename;
	  stderr << "--loaded " << filename << endl
	  );
     )

isSpecial := filename -> filename#0 === "$" or filename#0 === "!"

tryload := (filename,loadfun,notify) -> (
     ret := null;
     if isAbsolutePath filename or isSpecial filename then (
	  if fileExists filename then (
	       ret = loadfun filename;
	       markLoaded(filename,filename,notify);
	       ret)
	  else error("file doesn't exist: ", filename))
     else (
          if class path =!= List then error "expected 'path' to be a list (of strings)";
	  loaded := false;
          scan(
	       if currentFileDirectory == "--startupString--/" then path else prepend(currentFileDirectory, path),
	       dir -> (
		    if class dir =!= String then error "member of 'path' not a string";
		    fullfilename := dir | filename;
		    if fileExists fullfilename then (
		    	 ret = loadfun fullfilename;
		    	 markLoaded(fullfilename,filename,notify);
			 loaded = true;
			 break)));
	  if loaded then ret else error("file doesn't exist: ", filename)))

simpleLoad := load
load = (filename) -> tryload(filename,simpleLoad,notify)

simpleInput := input
input = (filename) -> tryload(filename,simpleInput,false)
needs = s -> if not loaded#?s then load s

currentPackage = null
UserDictionary = new Dictionary				    -- no package comes with this dictionary

load "loads.m2"
stderr << "--loaded setup.m2" << endl

globalDictionaries = prepend(UserDictionary,globalDictionaries)
notify = true
protect Macaulay2.Dictionary

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
