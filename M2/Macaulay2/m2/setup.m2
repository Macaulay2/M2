-- 	Copyright 1994 by Daniel R. Grayson


if class path =!= List then path = { "." }

OS := "operating system"

pathSeparator = (
	if version#OS === "MACOS"	then ":" 
	else
	if version#OS === "Windows NT"
	or version#OS === "MS-DOS"
	-- or version#OS === "CYGWIN32_NT"
	-- or version#OS === "CYGWIN32_95"
	then "\\"
	else "/"
	)

dir:= splice(apply(lines(commandLine#0, "/"), i -> toSequence lines(i, "\\")))
if #dir > 2 
then path = join({concatenate (apply(#dir-2, i -> (dir#i,pathSeparator)), "m2")}, path);

isAbsolutePath := filename -> pathSeparator === substring(filename,0,#pathSeparator)

if class phase === Symbol then phase = 0

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

oldflush := flush
erase quote flush
flush = new Manipulator from oldflush

oldendl := endl
erase quote endl
endl = new Manipulator from oldendl

erase quote "--newline--"

<< Thing := x -> stdout << x
File << Net := printString
File << String := printString
File << Symbol := printString	   		      -- provisional
File << Thing := (x,y) -> printString(x,string y)     -- provisional

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
     s := value concatenate("quote ",o);
     outputSymbols#s = true;
     s <- x;
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

<< "--loading source code..." << endl
load "loads.m2"

setrecursionlimit 300

clear = () -> (
     clearAll();
     scan(userSymbols(), i -> i <- i);
     )

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
     w := select(tab#"a" .. tab#"Z", 
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
     SEEALSO {"userSymbols"}
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

-- leave this at the END of setup, to get a complete list of options

document { quote Options,
     TT "Options", " -- an option used with ", TO "method", " to specify
     names of optional arguments and their default values.",
     PARA,
     NOINDENT,
     TT "f = method(Options => w)", " -- creates a method which accepts
     optional arguments.  Here 'w' is a list ", TT "{A=>a,B=>b,...}", " of
     optional argument names A,B,... and corresponding default values a,b,...",
     PARA,
     "When optional arguments are specified, the method functions installed
     should accept an additional argument to which will be passed a
     hash table of type ", TO "OptionTable", " containing the optional 
     argument names and their values.  The table will be stored in the
     ", TO "OptionsRegistry", " and can be recovered with the function
     ", TO "options", ".",
     EXAMPLE {
	  "f = method(Options => {Slope => 1, Intercept => 1})",
      	  "f RR := (x,options) -> options.Slope * x + options.Intercept",
      	  "f(5.,Slope=>100)",
	  },
     PARA,
     "Here is a complete list of symbols which are used as names of options:
     ",
     between( ",
     ",
     	  (i -> TO i) \ rsort keys set flatten (keys \ values OptionsRegistry)
	  ),
     ".",
     SEEALSO "method"
     }

-- if phase===1 or phase===2 or phase===4 then exportDocumentation()

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


