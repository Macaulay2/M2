-- startup.m2

-- this file gets incorporated into the executable file bin/Macaulay2 as the string 'startupString2'

--		Copyright 1993-2003 by Daniel R. Grayson

errorDepth = 0						    -- without this, we may see no error messages the second time through
debuggingMode = true

firstTime := not Array.?name

-- here we put local variables that might be used by the global definitions below
match := X -> 0 < #(matches X);

if firstTime then (
     -- all global definitions go here, because after loaddata is run, we'll come through here again
     -- with all these already done and global variables set to read-only
     Array.name = "Array";
     BasicList.name = "BasicList";
     BigReal.name = "BigReal";
     BigComplex.name = "BigComplex";
     Boolean.name = "Boolean";
     CacheTable.name = "CacheTable";
     Pseudocode.name = "Pseudocode";
     Database.name = "Database";
     Dictionary.name = "Dictionary";
     File.name = "File";
     Function.name = "Function";
     HashTable.name = "HashTable";
     List.name = "List";
     MutableHashTable.name = "MutableHashTable";
     MutableList.name = "MutableList";
     Net.name = "Net";
     Nothing.name = "Nothing";
     Option.name = "Option";
     QQ.name = "QQ";
     RR.name = "RR";
     RR.name = "RR";
     CC.name = "CC";					    -- new internal complex number class, to replace CC later
     Ring.name = "Ring";
     Sequence.name = "Sequence";
     String.name = "String";
     Symbol.name = "Symbol";
     Thing.name = "Thing";
     Time.name = "Time";
     Type.name = "Type";
     VisibleList.name = "VisibleList";
     ZZ.name = "ZZ";
     notify = false;

     normalPrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(newline, lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt; -- will print that many blanks, see interp.d
	  symbol currentPrompts <- normalPrompts;	    -- this avoids the warning about redefining a function
	  );
     examplePrompts = () -> (
	  normalPrompts();
	  ZZ.InputPrompt = lineno -> concatenate (newline, "\1i", toString lineno, " : ");
	  symbol currentPrompts <- examplePrompts;
	  );
     noPrompts = () -> (
	  ZZ.InputPrompt = lineno -> "";
	  ZZ.InputContinuationPrompt = lineno -> "";
	  symbol currentPrompts <- noPrompts;
	  );

     startFunctions := {};
     addStartFunction = f -> ( startFunctions = append(startFunctions,f); f);
     runStartFunctions = () -> scan(startFunctions, f -> f());

     endFunctions := {};
     addEndFunction = f -> ( endFunctions = append(endFunctions,f); f);
     runEndFunctions = () -> scan(endFunctions, f -> f());

     simpleExit := exit;
     exit = ret -> ( runEndFunctions(); simpleExit ret );

     File << Thing  := File => (x,y) -> printString(x,toString y);
     File << Net := File << Symbol := File << String := printString;
     << Thing := x -> stdio << x;
     String | String := String => concatenate;
     Function _ Thing := Function => (f,x) -> y -> f splice (x,y);
     String | String := String => concatenate;
     String | ZZ := String => (s,i) -> concatenate(s,toString i);
     ZZ | String := String => (i,s) -> concatenate(toString i,s);

     new HashTable from List := HashTable => (O,v) -> hashTable v;

     Manipulator = new Type of BasicList;
     Manipulator.synonym = "manipulator";
     new Manipulator from Function := Manipulator => (Manipulator,f) -> new Manipulator from {f};
     Manipulator.name = "Manipulator";
     Manipulator Database := Manipulator File := (m,o) -> m#0 o;

     Manipulator Nothing := (m,null) -> null;
     File << Manipulator := File => (o,m) -> m#0 o;
     List << Manipulator := File => (o,m) -> (scan(o, o -> m#0 o); o);
     Nothing << Manipulator := (null,m) -> null;

     close = new Manipulator from close;
     closeIn = new Manipulator from closeIn;
     closeOut = new Manipulator from closeOut;
     flush = new Manipulator from flush;
     endl = new Manipulator from endl;

     Thing.NoPrint = x -> null;
     Thing.Print = x ->  (
	  << newline << "o" << lineNumber << " = ";
	  try << x;
	  << newline << flush;
	  );

     first = x -> x#0;
     last = x -> x#-1;
     lines = x -> (
	  l := separate x;
	  if l#-1 === "" then drop(l,-1) else l);

     isAbsoluteExecPath = filename -> (
	  -- this is the way execvp(3) decides whether to search the path for an executable
	  match("/", filename)
	  );
     isAbsolutePathRegexp := (
	  -- this is the way execvp decides whether to search the path for a Macaulay 2 source file
	  if version#"operating system" === "Windows-95-98-NT" then "^(.:/|/)"
	  else "^/"
	  );
     isAbsolutePath = filename -> match(isAbsolutePathRegexp, filename);
     copyright = (
	  "Macaulay 2, version " | version#"VERSION" | newline
	  | "--Copyright 1993-2003, D. R. Grayson and M. E. Stillman" | newline
	  | "--Singular-Factory " | version#"factory version" | ", copyright 1993-2001, G.-M. Greuel, et al." | newline
	  | "--Singular-Libfac " | version#"libfac version" | ", copyright 1996-2001, M. Messollen" | newline
	  | "--NTL Library " | version#"ntl version" | ", copyright, Victor Shoup" | newline
     	  | "--GNU MP Library " | version#"gmp version"
	  );
     )

sourceHomeDirectory = null				    -- home directory of Macaulay 2
buildHomeDirectory  = null	       -- parent of the directory of the executable described in command line argument 0
prefixDirectory = null					    -- prefix directory, after installation, e.g., "/usr/local/"
encapDirectory = null	   -- encap directory, after installation, if present, e.g., "/usr/local/encap/Macaulay2-0.9.5/"

fullCopyright := false
matchpart := (regex,i,s) -> substring_((matches(regex, s))#i) s
notdir := s -> matchpart("[^/]*$",0,s)
dir := s -> (
     m := matches(".*/",s);
     if 0 == #m then "./" else substring_(m#0) s)
preload := true
noloaddata := false
nobanner := false;
nosetup := false
noinitfile := false
interpreter := commandInterpreter

getRealPath := fn -> (					    -- use this later if realpath doesn't work
     local s;
     while ( s = readlink fn; s =!= null ) do fn = if isAbsolutePath s then s else minimizeFilename(fn|"/../"|s);
     fn)

pathsearch := e -> (
     if not isAbsoluteExecPath e then (
	  -- we search the path, but we don't do it the same way execvp does, too bad.
	  PATH := separate(":",if "" =!= getenv "PATH" then getenv "PATH" else ".:/bin:/usr/bin");
	  PATH = apply(PATH, x -> if x === "" then "." else x);
	  scan(PATH, p -> if fileExists (p|"/"|e) then (e = p|"/"|e; break));
	  );
     e)

exe := (
     processExe := "/proc/" | toString processID() | "/exe";  -- this is a reliable way to get the executable in linux
     if fileExists processExe then realpath processExe
     else realpath pathsearch commandLine#0)
bindir := dir exe
bindirsuffix := LAYOUT#"bin";

setPrefixFromBindir := bindir -> if bindir =!= null then (
     if bindirsuffix === substring(bindir,-#bindirsuffix) then (
	  prefixdir := substring(bindir,0,#bindir-#bindirsuffix);
	  if fileExists(prefixdir | LAYOUT#"share") then prefixDirectory = realpath prefixdir | "/"))

if fileExists (bindir | "../c/scc1") then (
     -- we're running from the build directory
     buildHomeDirectory = minimizeFilename(bindir|"../");
     sourceHomeDirectory = (
	  if fileExists (buildHomeDirectory|"m2/setup.m2") then buildHomeDirectory 
	  else if fileExists(buildHomeDirectory|"srcdir") and fileExists(buildHomeDirectory|(first lines get (buildHomeDirectory|"srcdir")) | "/m2/setup.m2")
	  then buildHomeDirectory|(first lines get (buildHomeDirectory|"srcdir"))|"/" 
	  else null);
     ) else setPrefixFromBindir bindir

if prefixDirectory === null and sourceHomeDirectory === null then stderr << "warning: can't determine prefixDirectory or sourceHomeDirectory" << endl

if prefixDirectory =!= null and fileExists (prefixDirectory | "encapinfo") then (
     -- now get the second to last entry in the chain of symbolic links, which will be in the final prefix directory
     encapDirectory = prefixDirectory;
     prev := null;
     fn := pathsearch commandLine#0;
     local s;
     while ( s = readlink fn; s =!= null ) do (prev = fn; fn = if isAbsolutePath s then s else minimizeFilename(fn|"/../"|s););
     if prev =!= null then setPrefixFromBindir dir prev)

silence := arg -> null
notyeterr := arg -> error("command line option ", arg, " not re-implemented yet")
notyet := arg -> if preload then (
     << "warning: command line option " << arg << " not re-implemented yet" << newline << flush;
     )
obsolete := arg -> error ("warning: command line option ", arg, " is obsolete")
progname := notdir commandLine#0
usage := arg -> (
     << "usage:"             << newline
     << "    " << progname << " [option ...] [file ...]" << newline
     << "options:"  << newline
     << "    --help             print this brief help message and exit" << newline
     << "    --no-backtrace     print no backtrace after error" << newline
     << "    --copyright        display full copyright messasge" << newline
     << "    --no-debug         do not enter debugger upon error" << newline
     << "    --debug-M2         debug Macaulay2 itself" << newline
     << "    --dumpdata         read source code, dump data, exit (no init.m2)" << newline
     << "    --example-prompts  examples prompt mode" << newline
     << "    --fullbacktrace    print full backtrace after error" << newline
     << "    --no-loaddata      don't try to load the dumpdata file" << newline
     << "    --int              accept interrupts" << newline -- handled by M2lib.c
     << "    --notify           notify when loading source files during initialization" << newline
     << "    --no-prompts       print no input prompts" << newline;
     << "    --no-setup         don't try to load setup.m2" << newline
     << "    --print-width n    occasionally wrap output to this width" << newline
     << "    --silent           no startup banner" << newline
     << "    --stop             exit on error" << newline
     << "    --texmacs          TeXmacs session mode" << newline
     << "    --version          print version number and exit" << newline
     << "    -q                 don't load user's init.m2 file" << newline
     << "    -E '...'           evaluate expression '...' before initialization" << newline
     << "    -e '...'           evaluate expression '...' after initialization" << newline
     << "environment:"       << newline
     << "    M2ARCH             a hint to find the dumpdata file as" << newline
     << "                       bin/../cache/Macaulay2-$M2ARCH-data, where bin is the" << newline
     << "                       directory containing the Macaulay2 executable" << newline
     << "    EDITOR             default text editor" << newline
     << "    LOADDATA_IGNORE_CHECKSUMS	   (for debugging)" << newline
     ;exit 0)

loadSetup := () -> (
     -- try to load setup.m2
     if sourceHomeDirectory =!= null then load minimizeFilename(sourceHomeDirectory | "/m2/setup.m2")
     else if prefixDirectory =!= null then load minimizeFilename(prefixDirectory | LAYOUT#"m2" | "setup.m2")
     else error ("can't find file setup.m2; exe = ",exe,"; commandLine#0 = ",commandLine#0)
     )

dump := () -> (
     if not version#"dumpdata" then error "can't dump data with this version of Macaulay 2";
     arch := if getenv "M2ARCH" =!= "" then getenv "M2ARCH" else version#"architecture";
     fn := (
	  if buildHomeDirectory =!= null then concatenate(buildHomeDirectory , "cache/", "Macaulay2-", arch, "-data") else 
	  if prefixDirectory =!= null then concatenate(prefixDirectory, LAYOUT#"cache", "Macaulay2-", arch, "-data")	  
	  );
     if fn === null then error "can't find cache directory for dumpdata file";
     runEndFunctions();
     collectGarbage();
     stderr << "--dumping to " << fn << endl;
     dumpdata fn;
     stderr << "--success" << endl;
     exit 0;
     )

printWidth = 0

action := hashTable {
     "--help" => usage,
     "-h" => usage,
     "--" => obsolete,
     "-mpwprompt" => notyeterr,
     "-q" => arg -> noinitfile = true,
     "--silent" => arg -> nobanner = true,
     "--no-debug" => arg -> debuggingMode = false,
     "--dumpdata" => arg -> (noinitfile = noloaddata = true; if not preload then dump()),
     "--debug-M2" => arg -> (debuggingMode = true; errorDepth = 0),
     "-silent" => obsolete,
     "-tty" => notyet,
     "-n" => obsolete,
     "--int" => arg -> arg,
     "--copyright" => arg -> fullCopyright = true,
     "--no-prompts" => arg -> noPrompts(),
     "--notify" => arg -> notify = true,
     "-x" => obsolete,
     "--example-prompts" => arg -> examplePrompts(),
     "-s" => obsolete,
     "--fullbacktrace" => arg -> fullBacktrace = true,
     "--no-backtrace" => arg -> fullBacktrace = backtrace = false,
     "--stop" => arg -> (stopIfError = true; debuggingMode = false), -- see also M2lib.c
     "--no-loaddata" => arg -> noloaddata = true,
     "--no-setup" => arg -> nosetup = true,
     "--texmacs" => arg -> (
	  interpreter = topLevelTexmacs;
	  << TeXmacsBegin << "verbatim:" << " Macaulay 2 starting up " << endl << TeXmacsEnd << flush;
	  ),
     "--version" => arg -> ( << version#"VERSION" << newline; exit 0; )
     };

action2 := hashTable {
     "-E" => arg -> if preload then value arg,
     "-e" => arg -> if not preload then value arg,
     "--print-width" => arg -> printWidth atoi arg
     }

processCommandLineOptions := pl -> (			    -- two passes, based on value of 'preload'
     preload = pl;
     argno := 1;
     while argno < #commandLine do (
	  arg := commandLine#argno;
	  if action#?arg then action#arg arg
	  else if action2#?arg then (
	       if argno < #commandLine + 1
	       then (
		    argno = argno + 1;
		    action2#arg commandLine#argno
		    )
	       else error("command line option ", arg, " missing argument")
	       )
	  else if match("^-e",arg) and not preload then value substring(2,arg) -- to become obsolete
	  else if arg#0 == "-" then (
	       stderr << "error: unrecognized command line option: " << arg << endl;
	       usage();
	       exit 1;
	       )
	  else if not preload then load arg;
	  argno = argno+1;
	  );
     )

if firstTime then processCommandLineOptions true
if firstTime and not nobanner then stderr << (if fullCopyright then copyright else first separate copyright) << newline << flush
if firstTime and not noloaddata and version#"dumpdata" then (
     -- try to load dumped data
     arch := if getenv "M2ARCH" =!= "" then getenv "M2ARCH" else version#"architecture";
     datafile := minimizeFilename (
	  if buildHomeDirectory =!= null then concatenate(buildHomeDirectory, "/cache/Macaulay2-", arch, "-data")
	  else if prefixDirectory =!= null then concatenate(prefixDirectory, LAYOUT#"cache", "Macaulay2-", arch, "-data")
	  else concatenate("Macaulay2-", arch, "-data")
	  );
     if fileExists datafile then
     try (
	  stderr << "--loading cached memory data from " << datafile << newline << flush;
	  loaddata datafile
	  ) else (
	  stderr << "--warning: can not load data from " << datafile << newline << flush;
	  )
     )

path = {}
if sourceHomeDirectory  =!= null then path = append(path, sourceHomeDirectory|"m2/")
if buildHomeDirectory   =!= sourceHomeDirectory and buildHomeDirectory =!= null then path = join(path, {buildHomeDirectory|"m2/", buildHomeDirectory|"tutorial/final/"})
if prefixDirectory      =!= null then path = append(path, prefixDirectory | LAYOUT#"m2")
path = select(path, fileExists)
normalPrompts()
if firstTime and not nosetup then loadSetup()
errorDepth = loadDepth = loadDepth + 1
processCommandLineOptions false
runStartFunctions()
tryLoad := fn -> if fileExists fn then (load fn; true) else false
noinitfile or tryLoad "init.m2" or tryLoad (getenv "HOME" | "/init.m2") or tryLoad (getenv "HOME" | "/.init.m2")
n := interpreter()
if class n === ZZ and 0 <= n and n < 128 then exit n
if n === null then exit 0
debuggingMode = false
stopIfError = true
stderr << "error: can't interpret return value as an exit code" << endl
exit 1

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d && make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
