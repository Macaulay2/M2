--		Copyright 1993-1999 by Daniel R. Grayson

-- flag symbol sequence

<< Thing := x -> stdio << x
File << Net    := 
File << String := 
File << Symbol := File => printString	   		      -- provisional
File << Thing  := File => (x,y) -> printString(x,string y)     -- provisional

if class Manipulator =!= Symbol then error ///attempted to load "setup.m2" a second time///

Symbols = new MutableHashTable
GlobalAssignHook Function := (X,x) -> (
     if not Symbols#?x then Symbols#x = X;
     )
GlobalReleaseHook Function := (X,x) -> (
     -- error concatenate("warning: ", X, " redefined");	    -- provisional, see definition below
     if Symbols#x === X then remove(Symbols,x);
     )

Manipulator = new Type of BasicList
new Manipulator from Function := Manipulator => (Manipulator,f) -> new Manipulator from {f}
Manipulator.name = "Manipulator"
Manipulator Database := Manipulator File := (m,o) -> m#0 o

GlobalAssignHook Manipulator := (X,x) -> if not Symbols#?x then Symbols#x = X
GlobalReleaseHook Manipulator := (X,x) -> if Symbols#x === X then remove(Symbols,x)

Manipulator Nothing := (m,null) -> null
File << Manipulator := File => (o,m) -> m#0 o
List << Manipulator := File => (o,m) -> (scan(o, o -> m#0 o); o)
Nothing << Manipulator := (null,m) -> null

oldclose := close
erase symbol close
close = new Manipulator from oldclose

oldcloseIn := closeIn
erase symbol closeIn
closeIn = new Manipulator from oldcloseIn

oldcloseOut := closeOut
erase symbol closeOut
closeOut = new Manipulator from oldcloseOut

oldflush := flush
erase symbol flush
flush = new Manipulator from oldflush

oldendl := endl
erase symbol endl
endl = new Manipulator from oldendl

if class path =!= List then path = { "." }

OS := "operating system"

pathSeparator = (
	if version#"operating system" === "MACOS" then ":" 
	else "/"
	)

progname := commandLine#0
-- under Windows when run from M2.bat the quotation marks are not removed.
-- quotation marks are needed because the path may contain spaces
if substring(progname,0,1) === "\"" then progname = substring(progname,1)

dir := splice(apply(lines(progname, "/"), i -> if i === "" then i else toSequence lines(i, "\\")))

if #dir > 1
then (
     sourcedir := concatenate ( apply(#dir-2, i -> (dir#i,pathSeparator)), "m2");
     -- << "source dir = " << sourcedir << endl;		    -- debugging
     path = join({sourcedir}, path);
     )

-- hasColon := s -> # ( lines  ( concatenate(" ",s," "), ":" ) ) =!= 1

isAbsolutePath := (
     if version#"operating system" === "MACOS"
     then filename -> substring(filename,0,1) =!= ":"
     else if version#"operating system" === "Windows-95-98-NT"
     then filename -> substring(filename,1,1) === ":"
     else filename -> pathSeparator === substring(filename,0,#pathSeparator)
     )

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

Print Thing := x -> (
     o := concatenate("o",string lineNumber());
     x = applyMethod(AfterEval,x);
     if x =!= null then (
     	  s := value concatenate("symbol ",o);
     	  outputSymbols#s = true;
     	  s <- x;
	  );
     rot x;
     y := applyMethod(BeforePrint,x);
     if y =!= null then (
	  << endl;			  -- double space
	  << o << " = " << net y << endl;
	  );
     applyMethod(AfterPrint,x);
     )

NoPrint Thing := x -> (
     o := concatenate("o",string lineNumber());
     x = applyMethod(AfterEval,x);
     s := value concatenate("symbol ",o);
     s <- x;
     rot x;
     applyMethod(AfterNoPrint,x);
     )

String | String := String => concatenate
String | ZZ := String => (s,i) -> concatenate(s,string i)
ZZ | String := String => (i,s) -> concatenate(string i,s)

notify := false						    -- can change this for debugging
loaded := new MutableHashTable

markLoaded := (filename) -> ( 
     loaded#filename = true; 
     if notify then stderr << "--loaded " << filename << endl;
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
		    -- << "trying to load " << fn << endl;		    -- debugging
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
erase symbol load
load = (filename) -> (
     if not tryload(filename,oldLoad) then error ("can't open file ", filename)
     )

oldinput := input
erase symbol input
input = (filename) -> (
     oldnotify := notify;
     notify = false;
     if not tryload(filename,oldinput) then error ("can't open file ", filename);
     notify = oldnotify;
     )

needs = s -> if not loaded#?s then load s

writableGlobals := new MutableHashTable
scan((
	  symbol oooo,
	  symbol ooo,
	  symbol oo,
	  symbol path,
	  -- symbol writeExamples,
	  -- symbol readExamples,
	  symbol phase,
	  symbol compactMatrixForm
	  ), x -> writableGlobals#x = true)

startFunctions := {}

addStartFunction = g -> (
     startFunctions = append(startFunctions,g);
     g)
runStartFunctions = () -> scan(startFunctions, f -> f())
OLDENGINE = getenv("OLDENGINE") == "TRUE"
lastSystemSymbol = null

if not version#"mp" then (
     erase symbol PutAnnotationPacket;
     erase symbol PutCommonMetaOperatorPacket;
     erase symbol PutCommonMetaTypePacket;
     erase symbol PutCommonOperatorPacket;
     erase symbol PutOperatorPacket;
     erase symbol WritePacket;
     erase symbol closeLink;
     erase symbol openLink;
     erase symbol writeMessage;
     erase symbol writePacket;
     erase symbol writeRawPacket;
     )

load "loads.m2"
notify = true
lastSystemSymbol = local privateSymbol

if OLDENGINE then (
     erase symbol ZZZ;
     erase symbol NewMonomialOrder;
     erase symbol Component;
     erase symbol GroupLex;
     erase symbol GroupRevLex;
     erase symbol MonomialOrdering;
     erase symbol NCLex;
     erase symbol newDegreesMonoid;
     erase symbol newDegreesRing;
     erase symbol newEngine;
     erase symbol monomialOrdering;
     remove(ZZ,newDegreesRing);
     remove(ZZ,newDegreesMonoid);
     erase symbol clone;
     )
erase symbol OLDENGINE
erase symbol outputSymbols
erase symbol lastSystemSymbol

if phase === 1 then scanPairs(symbolTable(),
     (name,sym) -> if not writableGlobals#?sym then protect sym
     )

GlobalReleaseHook Function := (X,x) -> (
     stderr << "warning: " << toString X << " redefined" << endl;
     if Symbols#x === X then remove(Symbols,x);
     )

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
