-- 	Copyright 1994 by Daniel R. Grayson

-- flag quote sequence

<< Thing := x -> stdio << x
File << Net := printString
File << String := printString
File << Symbol := printString	   		      -- provisional
File << Thing := (x,y) -> printString(x,string y)     -- provisional

Manipulator = new Type of BasicList
new Manipulator from Function := (Manipulator,f) -> new Manipulator from {f}
Manipulator.name = "Manipulator"
Manipulator Database := Manipulator File := (m,o) -> m#0 o
Manipulator Nothing := (m,null) -> null
File << Manipulator := (o,m) -> m#0 o
Nothing << Manipulator := (null,m) -> null

oldclose := close
erase quote close
close = new Manipulator from oldclose

oldcloseIn := closeIn
erase quote closeIn
closeIn = new Manipulator from oldcloseIn

oldcloseOut := closeOut
erase quote closeOut
closeOut = new Manipulator from oldcloseOut

oldflush := flush
erase quote flush
flush = new Manipulator from oldflush

oldendl := endl
erase quote endl
endl = new Manipulator from oldendl

if class path =!= List then path = { "." }

OS := "operating system"

pathSeparator = (
	if version#"operating system" === "MACOS" then ":" 
	else "/"
	)

dir := splice(apply(lines(commandLine#0, "/"), i -> if i === "" then i else toSequence lines(i, "\\")))

if #dir > 1
then (
     sourcedir := concatenate ( apply(#dir-2, i -> (dir#i,pathSeparator)), "m2");
     -- << "source dir = " << sourcedir << endl;
     path = join({sourcedir}, path);
     )

hasColon := s -> # ( lines  ( concatenate(" ",s," "), ":" ) ) =!= 1

isAbsolutePath := (
     if version#"operating system" === "MACOS" 
     then filename -> hasColon filename and substring(filename,0,1) =!= ":"     
     else filename -> pathSeparator === substring(filename,0,#pathSeparator)
     )

if class phase === Symbol then phase = 0

erase quote "--newline--"

protect AfterEval
protect AfterPrint
protect BeforePrint

rot := x -> (
     quote oooo <- ooo;			  -- avoid GlobalAssignHook with <-
     quote ooo <- oo;
     quote oo <- x;
     )

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

outputSymbols = new MutableHashTable

Print Thing := x -> (
     o := concatenate("o",string lineNumber());
     x = applyMethod(AfterEval,x);
     if x =!= null then (
     	  s := value concatenate("quote ",o);
     	  outputSymbols#s = true;
     	  s <- x;
	  );
     rot x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl;			  -- double space
	  << o << " = " << y << endl;
	  );
     applyMethod(AfterPrint,x);
     )

NoPrint Thing := x -> (
     o := concatenate("o",string lineNumber());
     x = applyMethod(AfterEval,x);
     s := value concatenate("quote ",o);
     s <- x;
     rot x;
     applyMethod(AfterNoPrint,x);
     )

String | String := concatenate
String | ZZ := (s,i) -> concatenate(s,string i)
ZZ | String := (i,s) -> concatenate(string i,s)

notify := true						    -- can change this for debugging
loaded := new MutableHashTable

markLoaded := (filename) -> ( 
     loaded#filename = true; 
     if notify then << "--loaded " << filename << endl;
     )

isSpecial := filename -> filename#0 === "$" or filename#0 === "!"

tryload := (filename,load) -> (
     -- if notify then << "--loading " << filename << endl;
     if isAbsolutePath filename or isSpecial filename then (
	  if load filename then (
	       markLoaded filename;
	       true)
	  else false)
     else (
          if class path =!= List
	  then error "expected 'path' to be a list of strings";
          {} =!= select(1,path, 
	       dir -> (
		    if class dir =!= String 
		    then error "member of 'path' not a string";
		    fn := (
			 if dir === "." then filename 
			 else dir | pathSeparator | filename
			 );
		    result := load fn;
		    if result then markLoaded fn;
		    result))))
///
if version#"operating system" === "MACOS"
then tryload = (filename,load) -> (
     if isAbsolutePath filename then (
	  if load filename then (
	       markLoaded filename;
	       true)
	  else false)
     else (
          if class path =!= List
	  then error "expected 'path' to be a list of strings";
          {} =!= select(1,path, 
	       dir -> (
		    if class dir =!= String 
		    then error "member of 'path' not a string";
		    fn := (
			 if dir === "." then filename 
			 else dir  | filename
			 );
		    result := load fn;
		    if result then markLoaded fn;
		    result))))
///

oldLoad := load
erase quote load
load = (filename) -> (
     if not tryload(filename,oldLoad) then error ("can't open file ", filename)
     )

oldinput := input
erase quote input
input = (filename) -> (
     oldnotify := notify;
     notify = false;
     if not tryload(filename,oldinput) then error ("can't open file ", filename);
     notify = oldnotify;
     )

needs = s -> if not loaded#?s then load s

startFunctions := {}
addStartFunction = g -> (
     startFunctions = append(startFunctions,g);
     g)
runStartFunctions = () -> scan(startFunctions, f -> f())
OLDENGINE = getenv("OLDENGINE") == "TRUE"
lastSystemSymbol = null
<< "--loading source code..." << endl
load "loads.m2"
notify = true
lastSystemSymbol = local privateSymbol
if OLDENGINE then (
     erase quote ZZZ;
     erase quote NewMonomialOrder;
     erase quote Component;
     erase quote GroupLex;
     erase quote GroupRevLex;
     erase quote MonomialOrdering;
     erase quote NCLex;
     erase quote Weights;
     erase quote newDegreesMonoid;
     erase quote newDegreesRing;
     erase quote newEngine;
     erase quote monomialOrdering;
     erase quote clone;
     )
erase quote OLDENGINE
erase quote outputSymbols
erase quote lastSystemSymbol

-- the last function restarted
addStartFunction(
     () -> (
	  if not member("-q",commandLine)
	  then (
	       tryload("init.m2",oldLoad)
	       or
	       getenv "HOME" =!= "" and tryload(concatenate(getenv "HOME", "/init.m2"),oldLoad)
	       )
	  )
     )
