--		Copyright 1993-2003 by Daniel R. Grayson

if class oooo =!= Symbol then error "setup.m2 already loaded"

OutputDictionary = new Dictionary
globalDictionaries = append(globalDictionaries,OutputDictionary)

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

rot := x -> (
     if ooo =!= null then global oooo <- ooo;
     if oo =!= null then global ooo <- oo;
     if x =!= null then global oo <- x;
     )

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

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

timelimit := (t,f) -> (alarm t; r := f(); alarm 0; r)

printingTimeLimit = 20
errorPrintingTimeLimit := 3
symbol debugError <- identity				    -- use '<-' to bypass global assignment method
robustNet := y -> (
     try timelimit(printingTimeLimit, () -> net y) else (
	  global debugError <- x -> net y;
	  stderr << "--error in conversion of output to net: type 'debugError()' to see it; will try conversion to string" << endl << endl ;
	  try timelimit(errorPrintingTimeLimit, () -> toString y) else (
	       stderr << "--error in conversion of output to string" << endl << endl;
	       simpleToString y)))
Thing.Print = x -> (
     x = commonProcessing x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl			  -- double space
	  << concatenate(interpreterDepth:"o") << lineNumber << " = " 
	  << robustNet y << endl;
	  );
     applyMethod(AfterPrint,x);
     )

trunc := (wid,ht,s) -> (
     if wid > 0 and width s > wid then (
	  s = stack apply( unstack s, l -> if width l > wid then substring(l,0,wid-1) else l);
	  s = s | (stack ( height s + depth s : "$" ))^(height s - 1));
     if ht > 0 and height s + depth s  > ht then (
	  s = stack take(unstack s,ht-1);
	  s = s || concatenate(width s : "$"));
     s)
checkNet := n -> if class n === Net or class n === String then n else error "didn't format correctly"
checkString := n -> if class n === String then n else error "didn't format correctly"
silentRobustNet = (wid,ht,sec,y) -> (
     trunc(wid,ht,
	  try timelimit (sec, () -> if y === null then "null" else checkNet net y)
	  else 
	  try timelimit (sec, () -> checkString toString y)
	  else
	  simpleToString y))
silentRobustNetWithClass := (wid,ht,sec,y) -> (			    -- we know wid is at least 80
     part2 := horizontalJoin(" (of class ", silentRobustNet(wid//2,           ht,sec,class y), ")");
     part1 :=                               silentRobustNet(wid - width part2,ht,sec,      y);
     horizontalJoin(part1, part2));

hush := false
scan(binaryOperators, op -> 
     if not Thing#?(op,Thing,Thing) then installMethod(op, Thing, Thing, 
	  (x,y) -> (
	       line1 := concatenate("no method for ",
		    if op === symbol " " then "adjacent objects" else concatenate("binary operator ",op," applied to objects")
		    );
	       if hush then error line1;
	       ht := 8;
	       wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
	       preX := "            ";
	       preY := "       and  ";
	       wid = wid - width preX;
	       hush = true;					    -- prevent error message recursion
	       line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
	       line3 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
	       hush = false;
	       error toString stack(line1,line2,line3))))
scan( {(prefixOperators,"prefix"), (postfixOperators,"postfix")}, (ops,type) -> (
	  scan(ops, op -> 
	       if not Thing#?op then installMethod(op, Thing,
	  	    (x) -> (
			 line1 := concatenate("no method for ", concatenate(type," operator ",op));
			 if hush then error line1;
			 ht := 8;
			 wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
			 preX := "            ";
			 wid = wid - width preX;
			 hush = true;					    -- prevent error message recursion
			 line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
			 hush = false;
			 error toString stack(line1,line2))))))

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
