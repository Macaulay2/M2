-- startup.m2

-- this file gets incorporated into the executable file bin/Macaulay2 as the string 'startupString'

--		Copyright 1993-2003 by Daniel R. Grayson

errorDepth 0						    -- without this, we may see no error messages the second time through

firstTime := not Array.?name

-- here we put local variables that might be used by the global definitions below
match := X -> 0 < #(matches X);
oldstring := string; string := if class oldstring === Function then oldstring else toString
--

if firstTime then (
     -- all global definitions go here, because after loaddata is run, we'll come through here again
     -- with all these already done and global variables set to read-only
     Array.name = "Array";
     BasicList.name = "BasicList";
     BigReal.name = "BigReal";
     Boolean.name = "Boolean";
     CacheTable.name = "CacheTable";
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
     CCC.name = "CCC";					    -- new internal complex number class, to replace CC later
     Ring.name = "Ring";
     Sequence.name = "Sequence";
     String.name = "String";
     Symbol.name = "Symbol";
     Thing.name = "Thing";
     Time.name = "Time";
     Type.name = "Type";
     VisibleList.name = "VisibleList";
     ZZ.name = "ZZ";
     erase symbol Error;
     notify = false;

     normalPrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(newline, lastprompt = concatenate("i", string lineno, " : "));
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt; -- will print that many blanks, see interp.d
	  symbol currentPrompts <- normalPrompts;	    -- this avoids the warning about redefining a function
	  );
     examplePrompts = () -> (
	  normalPrompts();
	  ZZ.InputPrompt = lineno -> concatenate (newline, "\1i", string lineno, " : ");
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
     exit = ret -> ( runEndFunctions(); simpleExit ret );
     erase symbol simpleExit;

     File << Thing  := File => (x,y) -> printString(x,string y);
     File << Net := File << Symbol := File << String := printString;
     << Thing := x -> stdio << x;
     String | String := String => concatenate;
     Function _ Thing := Function => (f,x) -> y -> f splice (x,y);
     String | String := String => concatenate;
     String | ZZ := String => (s,i) -> concatenate(s,string i);
     ZZ | String := String => (i,s) -> concatenate(string i,s);

     Thing.NoPrint = x -> null;
     Thing.Print = x -> simpleFlush (
	  << newline << "o" << lineNumber() << " = ";
	  try << x;
	  << newline);

     first = x -> x#0;
     last = x -> x#-1;
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
     sourceHomeDirectory = null; -- home directory of Macaulay 2
     buildHomeDirectory  = null; -- parent of the directory of the executable described in command line argument 0

     copyright = (
	  "Macaulay 2, version " | version#"VERSION" | newline
	  | "--Copyright 1993-2003, D. R. Grayson and M. E. Stillman" | newline
	  | "--Singular-Factory " | version#"factory version" | ", copyright 1993-2001, G.-M. Greuel, et al." | newline
	  | "--Singular-Libfac " | version#"libfac version" | ", copyright 1996-2001, M. Messollen" | newline
	  | "--NTL Library " | version#"ntl version" | ", copyright, Victor Shoup" | newline
     	  | "--GNU MP Library " | version#"gmp version"
	  );
     )

fullCopyright := false
loadFile := if class load === Function then load else simpleLoad
matchpart := (regex,i,s) -> substring_((matches(regex, s))#i) s
notdir := s -> matchpart("[^/]*$",0,s)
dir := s -> (
     m := matches(".*/",s);
     if 0 == #m then "./" else substring_(m#0) s)
preload := true
noloaddata := false
nobanner := false;
nosetup := false
interpreter := topLevel

getRealPath := fn -> (
     s := realpath fn;
     if s =!= null then s
     else (
     	  while (
	       s = readlink fn;
	       s =!= null
	       ) 
	  do fn = if isAbsolutePath s then s else minimizeFilename(fn|"/../"|s);
     	  fn))

exe := (
     processExe := "/proc/" | string processID() | "/exe";  -- this is a reliable way to get the executable in linux
     if fileExists processExe then getRealPath processExe
     else (
	  e := commandLine#0;
	  if not isAbsoluteExecPath e then (
	       -- the only other choice is to search the path, but we don't do it the same way execvp does, too bad.
	       PATH := separate(":",if "" =!= getenv "PATH" then getenv "PATH" else ".:/bin:/usr/bin");
	       PATH = apply(PATH, x -> if x === "" then "." else x);
	       scan(PATH, p -> if fileExists (p|"/"|e) then (e = p|"/"|e; break));
	       );
	  getRealPath e))

buildHomeDirectory = minimizeFilename(dir exe|"../")

if fileExists (buildHomeDirectory|"m2/setup.m2"       ) then sourceHomeDirectory = buildHomeDirectory else
if fileExists (buildHomeDirectory|"srcdir/m2/setup.m2") then sourceHomeDirectory = getRealPath(buildHomeDirectory|"srcdir")|"/"

silence := arg -> null
notyeterr := arg -> error("command line option ", arg, " not re-implemented yet")
notyet := arg -> if preload then (
     << "warning: command line option " << arg << " not re-implemented yet" << newline; simpleFlush stdio;
     )
obsolete := arg -> error ("warning: command line option ", arg, " is obsolete")
progname := notdir commandLine#0
usage := arg -> (
     << "usage:"             << newline
     << "    " << progname << " [option ...] [file ...]" << newline
     << "options:"  << newline
     << "    --help             print brief help message and exit" << newline
     << "    --copyright        display full copyright messasge" << newline
     << "    --example-prompts  examples prompt mode" << newline
     << "    --no-loaddata      don't try to load the dumpdata file" << newline
     << "    --no-prompts       print no input prompts" << newline;
     << "    --no-setup         don't try to load setup.m2" << newline
     << "    --notify           notify when loading source files during initialization" << newline
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
     if sourceHomeDirectory === null then error ("can't find file setup.m2; exe = ",exe,"; commandLine#0 = ",commandLine#0);
     loadFile minimizeFilename(sourceHomeDirectory | "/m2/setup.m2")
     )

action := hashTable {
     "--help" => usage,
     "-h" => usage,
     "--" => obsolete,
     "-mpwprompt" => notyeterr,
     "-q" => silence,					    -- implemented in setup.m2
     "--silent" => arg -> nobanner = true,
     "-silent" => obsolete,
     "-tty" => notyet,
     "-n" => obsolete,
     "--copyright" => arg -> fullCopyright = true,
     "--no-prompts" => arg -> noPrompts(),
     "--notify" => arg -> notify = true,
     "-x" => obsolete,
     "--example-prompts" => arg -> examplePrompts(),
     "-s" => obsolete,
     "--stop" => arg -> stopIfError true,
     "--no-loaddata" => arg -> noloaddata = true,
     "--no-setup" => arg -> nosetup = true,
     "--texmacs" => arg -> (
	  interpreter = topLevelTexmacs;
	  << TeXmacsBegin << "verbatim:" << " Macaulay 2 starting up " << endl << TeXmacsEnd;
	  simpleFlush stdout;
	  ),
     "--version" => arg -> ( << version#"VERSION" << newline; exit 0; )
     };

action2 := hashTable {
     "-E" => arg -> if preload then value arg,
     "-e" => arg -> if not preload then value arg
     }

processCommandLineOptions := () -> (			    -- two passes, based on value of 'preload'
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
	  else if arg#0 == "-" then error("unrecognized command line option: ", arg)
	  else if not preload then loadFile arg;
	  argno = argno+1;
	  );
     )

---------------------------------

if firstTime then processCommandLineOptions()
preload = false

if firstTime and not nobanner then (
     stderr << (if fullCopyright then copyright else first separate copyright) << newline;
     simpleFlush stderr
     )

if firstTime and not noloaddata and version#"dumpdata" then (
     -- try to load dumped data
     arch := if getenv "M2ARCH" =!= "" then getenv "M2ARCH" else version#"architecture";
     datafile := minimizeFilename concatenate(buildHomeDirectory, "/cache/Macaulay2-",arch, "-data");
     if fileExists datafile then
     try (
	  stderr << "--loading cached memory data from " << datafile << newline; simpleFlush stderr;
	  loaddata datafile
	  ) else (
	  stderr << "warning: failed to load data from " << datafile << newline; simpleFlush stderr;
	  )
     )

path = {}
if sourceHomeDirectory  =!= null                then path = append(path, sourceHomeDirectory|"m2/");
if buildHomeDirectory   =!= sourceHomeDirectory then path = append(path, buildHomeDirectory |"m2/");
path = select(path, fileExists);

-- this might have to be fixed later to get the caches for the packages directory
documentationPath = apply(path, d -> minimizeFilename(d|"/cache/doc/"))
    documentationPath = select(documentationPath, fileExists)

normalPrompts()
if firstTime and not nosetup then loadSetup()
runStartFunctions()
errorDepth loadDepth()
stopIfError false					    -- this is also set in interp.d!
processCommandLineOptions()
if interpreter() then exit 0 else exit 1
