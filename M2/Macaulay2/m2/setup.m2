--		Copyright 1993-2003 by Daniel R. Grayson

if Function.?GlobalAssignHook then error "setup.m2, already loaded"
Function.GlobalAssignHook = (X,x) -> (
     if not Symbols#?x then Symbols#x = X;
     )
Function.GlobalReleaseHook = (X,x) -> (
     -- error concatenate("warning: ", X, " redefined");	    -- provisional, see definition below
     if Symbols#?x and Symbols#x === X then remove(Symbols,x);
     )
addStartFunction(
     () -> (
	  Function.GlobalReleaseHook = (X,x) -> (
	       if not writableGlobals#?X then stderr << "warning: " << toString X << " redefined" << endl;
	       if Symbols#x === X then remove(Symbols,x);
	       );
	  )
     )

---------------------------------

if notify then stderr << "--loading setup.m2" << endl

match := X -> 0 < #(matches X)				    -- defined as a method later

if class phase === Symbol then phase = 0

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

OutputDictionary = new Dictionary

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

Thing.Print = x -> (
     x = commonProcessing x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl;			  -- double space
	  << concatenate(interpreterDepth:"o") << lineNumber << " = " << (try net y else try toString y else try simpleToString y else "--something--") << endl;
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

setnotify := () -> (
     notify = true			-- notify after initialization
     )

needs = s -> if not loaded#?s then load s

new HashTable from List := HashTable => (O,v) -> hashTable v

load "loads.m2"
-- don't define any global variables below this point
stderr << "--loaded setup.m2" << endl
setnotify()
addStartFunction(
     () -> (
	  if not debuggingMode then errorDepth = loadDepth = loadDepth + 1;
	  if not member("-q",commandLine) then (
	       if fileExists "init.m2" then tryload("init.m2", simpleLoad, true)
	       else if getenv "HOME" =!= "" then (
		    fn := getenv "HOME" | "/init.m2";
		    if fileExists fn then tryload(fn, simpleLoad, true)
		    else (
		    	 fn = getenv "HOME" | "/.init.m2";
		    	 tryload(getenv "HOME" | "/.init.m2", simpleLoad, true))))))
newPackage Output
protect symbol Output
newPackage User
protect Macaulay2.Dictionary
