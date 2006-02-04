-- startup.m2

-- this file gets incorporated into the executable file bin/Macaulay2 as the string 'startupString2'

--		Copyright 1993-2003 by Daniel R. Grayson

errorDepth = 0						    -- without this, we may see no error messages the second time through
debuggingMode = true
stopIfError = false
gotarg := arg -> any(commandLine, s -> s == arg)
if gotarg "--stop" then stopIfError = true

firstTime := class PackageDictionary === Symbol

-- here we put local variables that might be used by the global definitions below
match := X -> null =!= regex X

if regex(".*/","/aa/bb") =!= {(0, 4)}
or regex(".*/","aabb") =!= null
then error "regex regular expression library not working"

if firstTime then (
     -- all global definitions go here, because after loaddata is run, we'll come through here again
     -- with all these already done and global variables set to read-only
     filesLoaded = new MutableHashTable;
     loadedFiles = new MutableHashTable;
     ReverseDictionary = new MutableHashTable;
     PrintNames = new MutableHashTable;
     Thing.AfterEval = identity;
     scan(
	  {symbol Array, symbol BasicList, symbol RRR, symbol CCC,
		symbol Boolean, symbol CacheTable, symbol Pseudocode, symbol Database,
		symbol Dictionary, symbol File, symbol Function, symbol HashTable,
		symbol List, symbol MutableHashTable, symbol MutableList, symbol Net,
		symbol Nothing, symbol Option, symbol QQ, symbol RR, symbol RR, symbol CC,
		symbol Ring, symbol Sequence, symbol String, symbol Symbol, symbol Thing,
		symbol Error, symbol MissingMethod,
		symbol Time, symbol Type, symbol VisibleList, symbol ZZ},
	  s -> ReverseDictionary#(value s) = s		    -- get an early start for debugging
	  );

     notify = false;

     slimPrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(lastprompt = concatenate(interpreterDepth:"i", toString lineno, ": "));
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt;
	  symbol currentPrompts <- slimPrompts;
	  );
     hush = () -> (
	  Thing.BeforePrint = identity;
	  Thing.Print = identity;
	  Thing.NoPrint = identity;
	  Thing.AfterPrint = identity;
	  Thing.AfterNoPrint = identity;
	  slimPrompts();
	  );
     hush();
     normalPrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(newline, lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt; -- will print that many blanks, see interp.d
	  symbol currentPrompts <- normalPrompts;	    -- this avoids the warning about redefining a function
	  );
     returnPrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(newline, lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "), newline, #lastprompt);
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt; -- will print that many blanks, see interp.d
	  symbol currentPrompts <- normalPrompts;	    -- this avoids the warning about redefining a function
	  );
     noPrompts = () -> (
	  ZZ.InputPrompt = lineno -> "";
	  ZZ.InputContinuationPrompt = lineno -> "";
	  symbol currentPrompts <- noPrompts;
	  );
     ctrlA := ascii 1;
     examplePrompts = () -> (
	  lastprompt := "";
	  ZZ.InputPrompt = lineno -> concatenate(ctrlA, lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
	  ZZ.InputContinuationPrompt = lineno -> #lastprompt;
	  symbol currentPrompts <- examplePrompts;
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
     String | ZZ := String => (s,i) -> concatenate(s,toString i);
     ZZ | String := String => (i,s) -> concatenate(toString i,s);

     new HashTable from List := HashTable => (O,v) -> hashTable v;

     Manipulator = new Type of BasicList;
     Manipulator.synonym = "manipulator";
     new Manipulator from Function := Manipulator => (Manipulator,f) -> new Manipulator from {f};
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

     Thing.Print = x ->  (
	  << newline << concatenate(interpreterDepth:"o") << lineNumber << " = ";
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
	  | "--Copyright 1993-2004, D. R. Grayson and M. E. Stillman" | newline
	  | "--Singular-Factory " | version#"factory version" | ", copyright 1993-2001, G.-M. Greuel, et al." | newline
	  | "--Singular-Libfac " | version#"libfac version" | ", copyright 1996-2001, M. Messollen" | newline
	  | "--NTL Library " | version#"ntl version" | ", copyright, Victor Shoup" | newline
     	  | "--GNU MP Library " | version#"gmp version"
	  );

     use = identity;				  -- temporary, until methods.m2
     globalAssignFunction = (X,x) -> (
	  if not ReverseDictionary#?x then ReverseDictionary#x = X;
	  use x;
	  );
     globalReleaseFunction = (X,x) -> if ReverseDictionary#?x and ReverseDictionary#x === X then remove(ReverseDictionary,x);
     globalAssignment = X -> (
	  if instance(X, VisibleList) then apply(X,globalAssignment)
	  else if instance(X,Type) then (
	       X.GlobalAssignHook = globalAssignFunction; 
	       X.GlobalReleaseHook = globalReleaseFunction;
	       )
	  else error "expected a type";
	  );
     globalAssignment {Type,Function};
     applicationDirectory = () -> homeDirectory | packageSuffix;
     )

sourceHomeDirectory = null				    -- home directory of Macaulay 2
buildHomeDirectory  = null	       -- parent of the directory of the executable described in command line argument 0
prefixDirectory = null					    -- prefix directory, after installation, e.g., "/usr/local/"
packagePath = null
encapDirectory = null	   -- encap directory, after installation, if present, e.g., "/usr/local/encap/Macaulay2-0.9.5/"

fullCopyright := false
matchpart := (pat,i,s) -> substring_((regex(pat, s))#i) s
notdir := s -> matchpart("[^/]*$",0,s)
dir := s -> ( m := regex(".*/",s); if 0 == #m then "./" else substring_(m#0) s)
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
	  then minimizeFilename(buildHomeDirectory|(first lines get (buildHomeDirectory|"srcdir"))|"/")
	  else null);
     ) else setPrefixFromBindir bindir

if prefixDirectory =!= null and fileExists (prefixDirectory | "encapinfo") then (
     -- now get the second to last entry in the chain of symbolic links, which will be in the final prefix directory
     encapDirectory = prefixDirectory;
     prev := null;
     fn := pathsearch commandLine#0;
     local s;
     while ( s = readlink fn; s =!= null ) do (prev = fn; fn = if isAbsolutePath s then s else minimizeFilename(fn|"/../"|s););
     if prev =!= null then setPrefixFromBindir dir prev)

phase := 1

silence := arg -> null
notyeterr := arg -> error("command line option ", arg, " not re-implemented yet")
notyet := arg -> if phase == 1 then (
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
     << "    --dumpdata         read source code, dump data if so configured, exit (no init.m2)" << newline
     << "    --fullbacktrace    print full backtrace after error" << newline
     << "    --no-loaddata      don't try to load the dumpdata file" << newline
     << "    --int              don't handle interrupts" << newline -- handled by M2lib.c
     << "    --notify           notify when loading files during initialization" << newline
     << "    --no-prompts       print no input prompts" << newline;
     << "    --no-readline      don't use readline" << newline;
     << "    --no-setup         don't try to load setup.m2" << newline
     << "    --prefix DIR       set prefixDirectory" << newline
     << "    --print-width n    set printWidth=n (the default is the window width)" << newline
     << "    --silent           no startup banner" << newline
     << "    --stop             exit on error" << newline
     << "    --texmacs          TeXmacs session mode" << newline
     << "    --version          print version number and exit" << newline
     << "    -q                 don't load user's init.m2 file or use packages in home directory" << newline
     << "    -E '...'           evaluate expression '...' before initialization" << newline
     << "    -e '...'           evaluate expression '...' after initialization" << newline
     << "    -x                 example prompts, don't use readline" << newline
     << "environment:"       << newline
     << "    M2ARCH             a hint to find the dumpdata file as" << newline
     << "                       bin/../cache/Macaulay2-$M2ARCH-data, where bin is the" << newline
     << "                       directory containing the Macaulay2 executable" << newline
     << "    EDITOR             default text editor" << newline
     << "    LOADDATA_IGNORE_CHECKSUMS	   (for debugging)" << newline
     ;exit 0)

markLoaded = (filename,origfilename,notify) -> ( 
     filename = minimizeFilename filename;
     filesLoaded#origfilename = filename; 
     loadedFiles##loadedFiles = filename; 
     if notify then stderr << "--loaded " << filename << endl;
     )

tryLoad := (ofn,fn) -> if fileExists fn then (
     simpleLoad fn;
     markLoaded(fn,ofn,notify);
     true) else false

loadSetup := () -> (
     sourceHomeDirectory =!= null and tryLoad("setup.m2", minimizeFilename(sourceHomeDirectory | "/m2/setup.m2"))
     or
     prefixDirectory =!= null and tryLoad("setup.m2", minimizeFilename(prefixDirectory | LAYOUT#"m2" | "setup.m2"))
     or
     error ("can't find file setup.m2\n\trunning M2: ",exe,"\n\t$0 = ",commandLine#0)
     )

showMaps := () -> (
     if version#"operating system" === "SunOS" then (
	  stack lines get ("!/usr/bin/pmap "|processID())
	  )
     else if version#"operating system" === "Linux" and fileExists("/proc/"|toString processID()|"/maps") then (
	  stack lines get("/proc/"|toString processID()|"/maps")
	  )
     else error "memory maps not available"
     )

dump := () -> (
     if not version#"dumpdata" then error "not configured for dumping data with this version of Macaulay 2";
     arch := if getenv "M2ARCH" =!= "" then getenv "M2ARCH" else version#"architecture";
     fn := (
	  if buildHomeDirectory =!= null then concatenate(buildHomeDirectory , "cache/", "Macaulay2-", arch, "-data") else 
	  if prefixDirectory =!= null then concatenate(prefixDirectory, LAYOUT#"cache", "Macaulay2-", arch, "-data")	  
	  );
     if fn === null then error "can't find cache directory for dumpdata file";
     fntmp := fn | ".tmp";
     stderr << "--preparing to dump to " << fntmp << endl;
     stderr << "--memory maps: " << showMaps() << endl;
     runEndFunctions();
     collectGarbage();
     stderr << "--dumping to " << fntmp << endl;
     interpreterDepth = 0;
     dumpdata fntmp;
     moveFile(fntmp,fn,Verbose=>true);
     stderr << "--success" << endl;
     exit 0;
     )

action := hashTable {
     "--help" => usage,
     "-h" => usage,
     "--" => obsolete,
     "-mpwprompt" => notyeterr,
     "-q" => arg -> noinitfile = true,
     "--silent" => arg -> nobanner = true,
     "--no-debug" => arg -> debuggingMode = false,
     "--dumpdata" => arg -> (noinitfile = noloaddata = true; if phase == 3 then dump()),
     "-silent" => obsolete,
     "-tty" => notyet,
     "-n" => obsolete,
     "--int" => arg -> arg,
     "--copyright" => arg -> if phase == 1 then fullCopyright = true,
     "--no-prompts" => arg -> if phase == 1 then noPrompts(),
     "--no-readline" => arg -> arg,			    -- handled in d/stdio.d
     "--notify" => arg -> if phase <= 2 then notify = true,
     "-x" => arg -> examplePrompts(),
     "-s" => obsolete,
     "--no-backtrace" => arg -> if phase == 1 then backtrace = false,
     "--stop" => arg -> (if phase == 1 then stopIfError = true; debuggingMode = false;), -- see also M2lib.c and tokens.d
     "--no-loaddata" => arg -> if phase == 1 then noloaddata = true,
     "--no-setup" => arg -> if phase == 1 then nosetup = true,
     "--texmacs" => arg -> if phase == 3 then (
	  interpreter = topLevelTexmacs;
	  << TeXmacsBegin << "verbatim:" << " Macaulay 2 starting up " << endl << TeXmacsEnd << flush;
	  ),
     "--version" => arg -> ( << version#"VERSION" << newline; exit 0; )
     };

action2 := hashTable {
     "-E" => arg -> if phase == 2 then value arg,
     "-e" => arg -> if phase == 3 then value arg,
     "--print-width" => arg -> if phase == 3 then printWidth = value arg,
     "--prefix" => arg -> if phase == 1 then (
	  if arg === "" or not match("/$",arg) then arg = arg | "/";
	  prefixDirectory = arg;
	  )
     }

processCommandLineOptions := phase0 -> (			    -- 3 passes
     ld := loadDepth;
     loadDepth = loadDepth + 1;
     phase = phase0;
     argno := 1;
     while argno < #commandLine do (
	  arg := commandLine#argno;
	  if action#?arg then action#arg arg
	  else if action2#?arg then (
	       if argno < #commandLine + 1
	       then action2#arg commandLine#(argno = argno + 1)
	       else error("command line option ", arg, " missing argument")
	       )
	  else if match("^-e",arg) and phase == 2 then value substring(2,arg) -- to become obsolete
	  else if arg#0 == "-" then (
	       stderr << "error: unrecognized command line option: " << arg << endl;
	       usage();
	       exit 1;
	       )
	  else if phase == 3 then if class load === Function then load arg else simpleLoad arg;
	  argno = argno+1;
	  );
     loadDepth = ld;
     )

if firstTime then processCommandLineOptions 1

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
	  if notify then stderr << "--loading cached memory data from " << datafile << newline << flush;
	  loaddata datafile
	  ) else (
	  if notify then stderr << "--warning: can not load data from " << datafile << newline << flush;
	  )
     )

packageSuffix = if version#"operating system" === "Darwin" then "Library/Application Support/Macaulay2/" else ".Macaulay2/"

path = {}
scan(commandLine, arg -> if arg === "-q" or arg === "--dumpdata" then noinitfile = true)

homeDirectory = getenv "HOME" | "/"

if not noinitfile then (
     path = join(
	  {applicationDirectory() | "local/" | LAYOUT#"datam2", applicationDirectory() | "code/"},
	  path);
     )

if packagePath === null then if prefixDirectory =!= null then packagePath = { prefixDirectory } else packagePath = { }
if not noinitfile then (
     packagePath = prepend(applicationDirectory() | "local/", packagePath);
     )

if sourceHomeDirectory  =!= null then path = join(path, {sourceHomeDirectory|"m2/",sourceHomeDirectory|"packages/"})
if buildHomeDirectory   =!= null then (
     path = join(path, {buildHomeDirectory|"tutorial/final/"});
     if buildHomeDirectory =!= sourceHomeDirectory then path = join(path, {buildHomeDirectory|"m2/"})
     )
if prefixDirectory      =!= null then (
     path = join(path, {prefixDirectory | LAYOUT#"m2", prefixDirectory | LAYOUT#"datam2"});
     )
if sourceHomeDirectory  =!= null then path = join(path, {sourceHomeDirectory|"test/", sourceHomeDirectory|"test/engine/"})
-- path = select(path, fileExists)
normalPrompts()

if firstTime and not nosetup then (
     loadSetup();
     )
processCommandLineOptions 2
runStartFunctions()
errorDepth = loadDepth
if not noinitfile then (
     -- the location of init.m2 is documented in the node "initialization file"
     tryLoad ("init.m2", applicationDirectory() | "init.m2");
     tryLoad ("init.m2", "init.m2");
     );
errorDepth = loadDepth+1				    -- anticipate loadDepth being 2 later
processCommandLineOptions 3
interpreterDepth = 0
n := interpreter()					    -- loadDepth is incremented by commandInterpreter
if class n === ZZ and 0 <= n and n < 128 then exit n
if n === null then exit 0
debuggingMode = false
stopIfError = true
stderr << "error: can't interpret return value as an exit code" << endl
exit 1

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d && make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
