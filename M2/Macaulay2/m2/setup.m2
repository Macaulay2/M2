--		Copyright 1993-2003 by Daniel R. Grayson

nonnull = x -> select(x, i -> i =!= null)
nonempty = x -> select(x, i -> i =!= "")
dashes  = n -> concatenate (n:"-")
spaces  = n -> concatenate n

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

--

-- references to the symbol 'User' occur before the corresponding package has been created...
getGlobalSymbol(PackageDictionary,"User")

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

match := X -> null =!= regex X -- defined as a method later

somethingElse = () -> error "something else needs to be implemented here"

rotateOutputLines := x -> (
     if ooo =!= null then global oooo <- ooo;
     if oo =!= null then global ooo <- oo;
     if x =!= null then global oo <- x;
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
	  alarm 0;
	  global debugError <- fun;
	  stderr << endl << "--error or time limit reached in conversion of output to net: type 'debugError()' to run it again; will try conversion to string" << endl ;
	  try timelimit(errorPrintingTimeLimit, () -> toString y) else (
	       alarm 0;
	       stderr << endl << "--error in conversion of output to string" << endl;
	       simpleToString y)))
Thing.Print = x -> (
     oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
     save := printWidth;
     if printWidth != 0 then printWidth = printWidth - #oprompt;
     z := robustNet x;
     printWidth = save;
     wrapper := lookup(symbol Wrap,class x);
     if wrapper =!= null then (
	  fun := () -> z = wrapper z;
	  try timelimit(printingTimeLimit, fun) else (
	       alarm 0;
	       global debugError <- fun;
	       stderr << "--error or time limit reached in applying Wrap method to output; type 'debugError()' to see it" << endl << endl));
     << endl << oprompt << z << endl;
     )
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
	  else (
	       alarm 0;
	       simpleToString y)))
silentRobustNetWithClass = (wid,ht,sec,y) -> (			    -- we know wid is at least 80
     part2 := horizontalJoin(" (of class ", silentRobustNet(wid//2,           ht,sec,class y), ")");
     part1 :=                               silentRobustNet(wid - width part2,ht,sec,      y);
     horizontalJoin(part1, part2));
silentRobustString = (wid,sec,y) -> (
     truncString(wid,
	  try timelimit (sec, () -> checkString toExternalString y)
	  else (
	       alarm 0;
	       simpleToString y)))
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

binaryOperators = join(fixedBinaryOperators,flexibleBinaryOperators)
prefixOperators = join(fixedPrefixOperators,flexiblePrefixOperators)
postfixOperators = join(fixedPostfixOperators,flexiblePostfixOperators)
flexibleOperators = join(flexibleBinaryOperators,flexiblePrefixOperators,flexiblePostfixOperators)
fixedOperators = join(fixedBinaryOperators,fixedPrefixOperators,fixedPostfixOperators)
allOperators = join(fixedOperators,flexibleOperators)

undocumentedkeys = new MutableHashTable
undocumented' = key -> undocumentedkeys#key = true

-- all this code should go!!
scan(flexibleBinaryOperators, op -> (
     	  opstring := toString op;
	  ht := 8;
	  preX := "            ";
	  if not Thing#?((op,symbol =),Thing,Thing) then (
	       undocumented' ((op,symbol =),Thing,Thing);
	       installMethod((op,symbol =), Thing, Thing, (x,y,z) -> (
			 preY := centerString(width preX, opstring);
			 preZ := centerString(width preX, "=");
			 line1 := concatenate("no method for assignment to ",
			      if op === symbol SPACE then "adjacent objects" else concatenate("binary operator ",op," applied to objects")
			      );
			 if hush then error line1;
			 wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
			 wid = wid - commentGuardWidth - width preX;
			 hush = true;					    -- prevent error message recursion
			 line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
			 line3 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
			 line4 := preZ | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,z);
			 hush = false;
			 error toString stack(line1,commentGuard line2,commentGuard line3,commentGuard line4))));
	  if not Thing#?(op,Thing,Thing) then (
	       undocumented' (op,Thing,Thing);
	       installMethod(op, Thing, Thing, (x,y) -> (
			 line1 := concatenate("no method for ",
			      if op === symbol SPACE then "adjacent objects:" else concatenate("binary operator ",op," applied to objects:")
			      );
			 if hush then error line1;
			 preY := centerString(#preX, opstring);
			 wid := max(printWidth,80);				    -- error might occur while printWidth is narrowed
			 wid = wid - commentGuardWidth - width preX;
			 hush = true;					    -- prevent error message recursion
			 line2 := preX | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,x);
			 line3 := preY | silentRobustNetWithClass(wid,ht,errorPrintingTimeLimit,y);
			 hush = false;
			 error toString stack(line1,commentGuard line2,commentGuard line3))))))
scan( {(flexiblePrefixOperators,"prefix"), (flexiblePostfixOperators,"postfix")}, (ops,type) -> 
     scan(ops, op -> (
	       ht := 8;
	       preX := "            ";
	       if not Thing#?(op,symbol =) then (
		    undocumented' ((op,symbol =), Thing);
		    installMethod((op,symbol =), Thing, (y,z) -> (
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
			      error toString stack(line1,commentGuard line2,commentGuard line3))));
	       if not Thing#?op then installMethod(op, Thing, (x) -> (
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

pathdo := (loadfun,path,filename,reportfun) -> (
     ret := null;
     if class filename =!= String then error "expected a string";
     if null === scan(
	  if isAbsolutePath filename or isSpecial filename then {""}
	  else if currentFileDirectory == "--startupString--/" then path
	  else prepend(currentFileDirectory, path),
	  dir -> (
	       if class dir =!= String then error "member of 'path' not a string";
	       fullfilename := dir | filename;
	       if fileExists fullfilename then (
		    ret = loadfun fullfilename;
		    reportfun fullfilename;
		    break true)))
     then error("file not found on path: \"", toString filename, "\"");
     ret)

tryload := (filename,loadfun,notify) -> pathdo(loadfun,path,filename, fullfilename -> markLoaded(fullfilename,filename,notify))
load = (filename) -> (tryload(filename,simpleLoad,notify);)
input = (filename) -> (tryload(filename,simpleInput,false);)
needs = s -> if not filesLoaded#?s then load s
     
load "loads.m2"

protect Core.Dictionary

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
