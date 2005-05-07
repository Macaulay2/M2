--		Copyright 1993-2003 by Daniel R. Grayson

assert = x -> if not x then error "assertion failed"

show = (msg) -> x -> (stderr << msg << x << endl; x)

if class oooo =!= Symbol then error "setup.m2 already loaded"

if class RawMutableMatrix =!= Type then error "where is RawMutableMatrix?"

OutputDictionary = new Dictionary
globalDictionaries = append(globalDictionaries,OutputDictionary)

--

PackageDictionary = new Dictionary
globalDictionaries = append(globalDictionaries,PackageDictionary)
assert( not isGlobalSymbol "Macaulay2Core" )
getGlobalSymbol(PackageDictionary,"Macaulay2Core")
--getGlobalSymbol(PackageDictionary,"User")

-----------

addStartFunction(
     () -> (
	  Function.GlobalReleaseHook = (X,x) -> (
	       stderr << "--warning: " << toString X << " redefined" << endl;
     	       remove(ReverseDictionary,x);
	       );
	  )
     )

---------------------------------

if notify then stderr << "--loading setup.m2" << endl

match := X -> 0 < #(matches X)				    -- defined as a method later

somethingElse = () -> error "something else needs to be implemented here"

rotateOutputLines := x -> (
     if ooo =!= null then global oooo <- ooo;
     if oo =!= null then global ooo <- oo;
     if x =!= null then global oo <- x;
     )

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

Thing.AfterEval = x -> (
     if x =!= null then (
     	  s := getGlobalSymbol(OutputDictionary,concatenate(interpreterDepth:"o",toString lineNumber));
     	  s <- x;
	  );
     rotateOutputLines x;
     x)

simpleToString := toString

timelimit := (t,f) -> (alarm t; r := f(); alarm 0; r)

printingTimeLimit = 20
errorPrintingTimeLimit := 3
symbol debugError <- identity				    -- use '<-' to bypass global assignment method
robustNet := y -> (
     fun := () -> net y;
     try timelimit(printingTimeLimit, fun) else (
	  global debugError <- fun;
	  stderr << endl << "--error or time limit reached in conversion of output to net: type 'debugError()' to run it again; will try conversion to string" << endl ;
	  try timelimit(errorPrintingTimeLimit, () -> toString y) else (
	       stderr << endl << "--error in conversion of output to string" << endl;
	       simpleToString y)))
Thing.Print = x -> (
     z := robustNet x;
     wrapper := lookup(symbol Wrap,class x);
     if wrapper =!= null then (
	  fun := () -> z = wrapper z;
	  try timelimit(printingTimeLimit, fun) else (
	       global debugError <- fun;
	       stderr << "--error or time limit reached in applying Wrap method to output; type 'debugError()' to see it" << endl << endl));
     << endl << concatenate(interpreterDepth:"o") << lineNumber << " = " << z << endl)
Nothing.Print = identity

truncNet := (wid,ht,s) -> (
     if wid > 0 and width s > wid then (
	  s = stack apply( unstack s, l -> if width l > wid then substring(l,0,wid-1) else l);
	  s = s | (stack ( height s + depth s : "$" ))^(height s - 1));
     if ht > 0 and height s + depth s  > ht then (
	  s = stack take(unstack s,ht-1);
	  s = s || concatenate(width s : "$"));
     s)
truncString := (wid,s) -> if wid > 0 and width s > wid then concatenate(substring(s,0,wid-1),"$") else s
checkNet := n -> if class n === Net or class n === String then n else error "didn't format correctly"
checkString := n -> if class n === String then n else error "didn't format correctly"
Nothing.Format = toString
String.Format = format
silentRobustNet = (wid,ht,sec,y) -> (
     truncNet(wid,ht,
	  try timelimit (sec, () -> checkNet if lookup(symbol Format,class y) =!= null then (lookup(symbol Format,class y)) y else net y)
	  else 
	  try timelimit (sec, () -> checkString toExternalString y)
	  else
	  simpleToString y))
silentRobustNetWithClass = (wid,ht,sec,y) -> (			    -- we know wid is at least 80
     part2 := horizontalJoin(" (of class ", silentRobustNet(wid//2,           ht,sec,class y), ")");
     part1 :=                               silentRobustNet(wid - width part2,ht,sec,      y);
     horizontalJoin(part1, part2));
silentRobustString = (wid,sec,y) -> (
     truncString(wid,
	  try timelimit (sec, () -> checkString toExternalString y)
	  else simpleToString y))
silentRobustStringWithClass = (wid,sec,y) -> (
     part2 := concatenate(" (of class ", silentRobustString(wid//2,           sec,class y), ")");
     part1 :=                            silentRobustString(wid - width part2,sec,      y);
     concatenate(part1, part2));

hush := false
commentGuardString := "--"
commentGuardNet := raise(horizontalJoin commentGuardString,-1)
commentGuardWidth := #commentGuardString
commentGuard := n -> if class n === String then concatenate(commentGuard,n) else (
     (stack((height n + depth n):commentGuardNet))^(height n) | n
     )
scan(binaryOperators, op -> (
	  if op === symbol "!=" then return;		    -- this one has an internal binary method
     	  opstring := if op === symbol " " then format toString op else toString op;
	  ht := 8;
	  preX := "            ";
	  if not Thing#?((op,symbol =),Thing,Thing) then installMethod((op,symbol =), Thing, Thing, Error => 
	       (x,y,z) -> (
	  	    preY := centerString(width preX, opstring);
     	       	    preZ := centerString(width preX, "=");
		    line1 := concatenate("no method for assignment to ",
			 if op === symbol " " then "adjacent objects" else concatenate("binary operator ",op," applied to objects")
			 );
		    if hush then error line1;
		    wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
		    wid = wid - commentGuardWidth - width preX;
		    hush = true;					    -- prevent error message recursion
		    line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
		    line3 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
		    line4 := preZ | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,z);
		    hush = false;
		    error toString stack(line1,commentGuard line2,commentGuard line3,commentGuard line4)));
	  if not Thing#?(op,Thing,Thing) then installMethod(op, Thing, Thing, Error => 
	       (x,y) -> (
		    line1 := concatenate("no method for ",
			 if op === symbol " " then "adjacent objects:" else concatenate("binary operator ",op," applied to objects:")
			 );
		    if hush then error line1;
	  	    preY := centerString(#preX, opstring);
		    wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
		    wid = wid - commentGuardWidth - width preX;
		    hush = true;					    -- prevent error message recursion
		    line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
		    line3 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
		    hush = false;
		    error toString stack(line1,commentGuard line2,commentGuard line3)))))
scan( {(prefixOperators,"prefix"), (postfixOperators,"postfix")}, (ops,type) -> 
     scan(ops, op -> (
	       ht := 8;
	       preX := "            ";
	       if not Thing#?(op,symbol =) then installMethod((op,symbol =), Thing, Error => 
		    (y,z) -> (
	       		 preY := centerString(width preX, toString op);
	       		 preZ := centerString(width preX, "=");
			 line1 := concatenate("no method for assignment to ", concatenate(type," operator ",op), " applied to:");
			 if hush then error line1;
			 wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
			 wid = wid - commentGuardWidth - width preX;
			 hush = true;					    -- prevent error message recursion
			 line2 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
			 line3 := preZ | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,z);
			 hush = false;
			 error toString stack(line1,commentGuard line2,commentGuard line3)));
	       if not Thing#?op then installMethod(op, Thing, Error => 
		    (x) -> (
			 line1 := concatenate("no method for ", concatenate(type," operator ",op), " applied to:");
			 if hush then error line1;
			 wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
			 wid = wid - commentGuardWidth - width preX;
			 hush = true;					    -- prevent error message recursion
			 line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
			 hush = false;
			 error toString stack(line1,commentGuard line2))))))

Thing.NoPrint = x -> (
     -- do nothing
     )

isSpecial := filename -> match( "^(\\$|!)", filename )

tryload := (filename,loadfun,notify) -> (
     ret := null;
     loaded := false;
     if class filename === String and (isAbsolutePath filename or isSpecial filename) then (
	  if fileExists filename then (
	       ret = loadfun filename;
	       markLoaded(filename,filename,notify);
	       return ret;)
	  else error("file doesn't exist: \"", filename, "\""));
     scan(path, dir -> if class dir =!= String then error "member of 'path' not a string");
     if class filename =!= String then error "expected a string";
     epath := if currentFileDirectory == "--startupString--/" then path else prepend(currentFileDirectory, path);
     scan(epath, dir -> (
		    if loaded then break;
		    fullfilename := dir | filename;
		    if fileExists fullfilename then (
			 ret = loadfun fullfilename;
			 markLoaded(fullfilename,filename,notify);
			 loaded = true;
			 break)));
     if loaded then ret else error("file not found on path: \"", toString filename, "\""))

load = (filename) -> tryload(filename,simpleLoad,notify)
input = (filename) -> tryload(filename,simpleInput,false)
needs = s -> if not filesLoaded#?s then load s

load "loads.m2"
if notify then stderr << "--loaded " << currentFileName << endl

protect Macaulay2Core.Dictionary

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
