-- 	Copyright 1994 by Daniel R. Grayson

if not load "version.m2" then error "couldn't find file 'version.m2'"

pathSeparator = if version#"OS" === "MACOS" then ":" else "/"

newline = if version#"OS" === "MACOS" then "\r"   else
          if version#"OS" === "MS-DOS" then "\r\n" else
          "\n"				  -- UNIX

isAbsolutePath := (
     if version#"OS" === "MACOS" then (
	  filename -> false		  -- ???
	  )
     else (
	  filename -> (
	       substring(filename,0,1) === "/" or
	       substring(filename,0,2) === "./" or
	       substring(filename,0,3) === "../")))

path = { "." }

if class phase === Symbol then phase = 0

Manipulator = new Type of BasicList
new Manipulator from Function := (Manipulator,f) -> new Manipulator from {f}
Manipulator.name = quote Manipulator
Manipulator Database := Manipulator File := (m,o) -> m#0 o
Manipulator Nothing := (m,null) -> null
File << Manipulator := (o,m) -> m#0 o
Nothing << Manipulator := (null,m) -> null

oldclose := close
erase quote close
close = new Manipulator from oldclose

oldflush := flush
erase quote flush
flush = new Manipulator from oldflush

oldendl := endl
erase quote endl
endl = new Manipulator from oldendl

<< Thing := x -> stdout << x
File << Net := File << Symbol := File << String := printString
File << Thing := (x,y) -> printString(x,string y) -- provisional
Nothing << Thing := (x,y) -> null

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

outputSymbols := new MutableHashTable

Print Thing := x -> (
     o := concatenate("o",string lineNumber());
     x = applyMethod(AfterEval,x);
     o = evaluate concatenate("quote ",o);
     outputSymbols#o = true;
     o <- x;
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
     o = evaluate concatenate("quote ",o);
     o <- x;
     rot x;
     applyMethod(AfterNoPrint,x);
     )

String | String := concatenate
String | ZZ := (s,i) -> concatenate(s,string i)
ZZ | String := (i,s) -> concatenate(string i,s)

notify := false
loaded := new MutableHashTable
markLoaded := (filename) -> ( 
     loaded#filename = true; 
     if notify then << "--loaded " << filename << endl;
     )

tryload := (filename,load) -> (
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
			 else dir | pathSeparator | filename
			 );
		    result := load fn;
		    if result then markLoaded fn;
		    result))))

oldLoad := load
erase quote load
load = (filename) -> (
     if not tryload(filename,oldLoad) then error ("can't open file ", filename)
     )

oldinput := input
erase quote input
input = (filename) -> (
     if not tryload(filename,oldinput) then error ("can't open file ", filename)
     )

needs = s -> if not loaded#?s then load s

startFunctions := {}
addStartFunction = g -> (
     startFunctions = append(startFunctions,g);
     g)
runStartFunctions = () -> scan(startFunctions, f -> f())

-- erase space symbol
X := new Type of HashTable
X X := identity
erase (keys X)#0#0

load "loads.m2"

setrecursionlimit 300

userSymbols = type -> (			  -- last symbol introduced
     if type === () then type = Thing;
     tab := symbolTable();
     v := select(values tab,
	  symb -> (
	       hash symb > hash quote listUserSymbols  -- hash codes of symbols are sequential
	       and mutable symb
	       and instance(value symb,type)
	       )
	  );
     w := select(tab#"a" .. tab#"Y", 
	  symb -> (
	       value symb =!= symb
	       and instance(value symb,type)
	       ));
     apply(sort(apply(join(v,w), symb -> (hash symb, symb))), (h,s) -> s))

document { quote userSymbols,
     TT "userSymbols ()", " -- provides a list of variables defined by
     the user.",
     BR,
     NOINDENT, TT "userSymbols X", " -- limits the list to those variables whose
     values are instances of the class X.",
     PARA,
     "Protected variables are excluded from the list.",
     SEEALSO "listUserSymbols"
     }

listUserSymbols = new Command from (
     type -> scan(userSymbols type, s -> << s << " : " << class value s << endl)
     )

document { quote listUserSymbols,
     TT "listUserSymbols", " -- a command which displays a list of variables 
     defined by the user, along with their types.",
     BR,
     NOINDENT, TT "listUserSymbols X", " -- limits the list to those variables whose
     values are instances of X.",
     PARA,
     "This function is useful after using ", TO "loaddata", " to restore 
     a previous session.",
     SEEALSO ("userSymbols")
     }

clearAll = new Command from (
     () -> (
     	  scan(keys outputSymbols, s -> (
	       	    remove(outputSymbols,s);
	       	    erase s;
	       	    ));
     	  )
     )

document { quote clearAll,
     TT "clearAll()", " -- attempts to release memory by clearing the values
     retained by the output line symbols."
     }

-- the last function restarted
addStartFunction(
     () -> (
	  if not member("-q",commandLine)
	  then (
	       tryload("init.m2",oldLoad)
	       or tryload(concatenate(getenv "HOME", "/init.m2"),oldLoad)
	       )
	  )
     )

notify = true


