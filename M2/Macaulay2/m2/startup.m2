-- startup.m2

-- this file gets incorporated into the executable file bin/M2 as the string 'startupString2'

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

-- we do this bit *before* "debug Core", so that Core (the symbol, not the package), which may not be there yet, ends up in the right dictionary
if firstTime then (
     assert = x -> (
	  if class x =!= Boolean then error "'assert' expected true or false";
	  if not x then error "assertion failed");
     PackageDictionary = new Dictionary;
     dictionaryPath = append(dictionaryPath,PackageDictionary);
     assert( not isGlobalSymbol "Core" );
     PackageDictionary#("Package$Core") = getGlobalSymbol(PackageDictionary,"Core");
     )

-- we need access to the private symbols -- (we remove the Core private dictionary later.)
if not firstTime then debug Core

toString := value getGlobalSymbol if firstTime then "simpleToString" else "toString"

-- this next bit has to be *parsed* after the "debug" above, to prevent the symbols from being added to the User dictionary
if firstTime then (
     -- all global definitions go here, because after loaddata is run, we'll come through here again
     -- with all these already done and global variables set to read-only

     filesLoaded = new MutableHashTable;
     loadedFiles = new MutableHashTable;
     notify = false;
     nobanner = false;
     texmacsmode = false;
     restarting = false;
     restarted = false;

     markLoaded = (fullfilename,origfilename,notify) -> ( 
	  fullfilename = minimizeFilename fullfilename;
	  filesLoaded#origfilename = fullfilename; 
	  loadedFiles##loadedFiles = toAbsolutePath fullfilename; 
	  if notify then stderr << "--loaded " << fullfilename << endl;
	  );
     normalPrompts = () -> (
	  lastprompt := "";
	  ZZ#{Standard,InputPrompt} = lineno -> concatenate(newline, lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
	  ZZ#{Standard,InputContinuationPrompt} = lineno -> #lastprompt; -- will print that many blanks, see interp.d
	  symbol currentPrompts <- normalPrompts;	    -- this avoids the warning about redefining a function
	  );
     normalPrompts();
     noPrompts = () -> (
	  ZZ#{Standard,InputPrompt} = lineno -> "";
	  ZZ#{Standard,InputContinuationPrompt} = lineno -> "";
	  symbol currentPrompts <- noPrompts;
	  );

     startFunctions := {};
     addStartFunction = f -> ( startFunctions = append(startFunctions,f); f);
     runStartFunctions = () -> scan(startFunctions, f -> f());

     endFunctions := {};
     addEndFunction = f -> ( endFunctions = append(endFunctions,f); f);
     runEndFunctions = () -> (
	  save := endFunctions;
	  endFunctions = {};
	  scan(save, f -> f());
	  endFunctions = save;
	  );

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
     Manipulator Database := Manipulator File := Manipulator NetFile := (m,o) -> m#0 o;

     Manipulator Nothing := (m,null) -> null;
     File << Manipulator := File => (o,m) -> m#0 o;
     NetFile << Manipulator := File => (o,m) -> m#0 o;
     Nothing << Manipulator := (null,m) -> null;

     TeXmacsBegin = ascii 2;
     TeXmacsEnd   = ascii 5;

     close = new Manipulator from close;
     closeIn = new Manipulator from closeIn;
     closeOut = new Manipulator from closeOut;
     flush = new Manipulator from flush;
     endl = new Manipulator from endl;

     Thing#{Standard,Print} = x ->  (
	  << newline << concatenate(interpreterDepth:"o") << lineNumber << " = ";
	  try << x;
	  << newline << flush;
	  );

     first = x -> x#0;
     last = x -> x#-1;
     lines = x -> (
	  l := separate x;
	  if l#-1 === "" then drop(l,-1) else l);

     isFixedExecPath = filename -> (
	  -- this is the way execvp(3) decides whether to search the path for an executable
	  match("/", filename)
	  );
     re := "/";						    -- /foo/bar
     if version#"operating system" === "Windows-95-98-NT" 
     then re = re | "|.:/";				    -- "C:/FOO/BAR"
     re = re | "|\\$";					    -- $www.uiuc.edu:80
     re = re | "|!";					    -- !date
     isAbsolutePathRegexp := "^(" | re | ")";		    -- whether the path will work from any directory and get to the same file
     re = re | "|\\./";					    -- ./foo/bar
     re = re | "|\\.\\./";				    -- ../foo/bar
     isStablePathRegexp   := "^(" | re | ")";               -- whether we should search only in the current directory (or current file directory)
     isAbsolutePath = filename -> match(isAbsolutePathRegexp, filename);
     isStablePath = filename -> match(isStablePathRegexp, filename);
     concatPath = (a,b) -> if isAbsolutePath b then b else a|b;

     toAbsolutePath = pth -> if pth =!= "stdio" and not isAbsolutePath pth then "/" | relativizeFilename("/", pth) else pth;

     copyright = (
	  "Macaulay 2, version " | version#"VERSION" | newline
	  | "--Copyright 1993-2008, D. R. Grayson and M. E. Stillman" | newline
	  | "--GC " | version#"gc version" | ", by H. Boehm and Alan J. Demers" | newline
	  | "--Singular-Factory " | version#"factory version" | ", by G.-M. Greuel et al." | newline
	  | "--Singular-Libfac " | version#"libfac version" | ", by M. Messollen" | newline
	  | "--NTL " | version#"ntl version" | ", by V. Shoup" | newline
     	  | "--GNU MP " | version#"gmp version" | ", by T. Granlund et al." | newline
     	  | "--MPFR " | version#"mpfr version" | ", by Free Software Foundation" | newline
	  | "--BLAS and LAPACK 3.0" | ", by J. Dongarra et al."
	  );

     scan(
	  { ("factory version", "3.0.2"), ("libfac version", "3.0.1") },
	  (k,v) -> if version#k < v then stderr << "--warning: old " << k << " " << version#k << " < " << v << endl);

     use = identity;				  -- temporary, until methods.m2

     Attributes = new MutableHashTable;
     -- values are hash tables with keys Symbol, String, Net (as symbols); replaces ReverseDictionary and PrintNames
     setAttribute = (val,attr,x) -> (
	  if Attributes#?val then Attributes#val else Attributes#val = new MutableHashTable
	  )#attr = x;
     hasAnAttribute = (val) -> Attributes#?val;
     hasAttribute = (val,attr) -> Attributes#?val and Attributes#val#?attr;
     getAttribute = (val,attr) -> Attributes#val#attr;
     getAttributes = (attr0) -> (
	  r := new MutableHashTable;
	  scan(values Attributes, tab -> scan(pairs tab, (attr,x) -> if attr === attr0 then r#x = true));
	  keys r);
     removeAttribute = (val,attr) -> remove(Attributes#val,attr);
     protect PrintNet;
     protect PrintNames;
     protect ReverseDictionary;

     globalAssign = (s,v) -> if v =!= value s then (
	  X := class value s;
	  m := lookup(GlobalReleaseHook,X);
	  if m =!= null then m(s,value s);
	  Y := class v;
	  n := lookup(GlobalAssignHook,Y);
	  if n =!= null then n(s,v);
	  s <- v);
     globalAssignFunction = (X,x) -> (
	  if not hasAttribute(x,ReverseDictionary) then setAttribute(x,ReverseDictionary,X);
	  use x;
	  );
     globalReleaseFunction = (X,x) -> (
	  if hasAttribute(x,ReverseDictionary)
	  and getAttribute(x,ReverseDictionary) === X
	  then removeAttribute(x,ReverseDictionary)
	  );
     globalAssignment = X -> (
	  if instance(X, VisibleList) then apply(X,globalAssignment)
	  else if instance(X,Type) then (
	       X.GlobalAssignHook = globalAssignFunction; 
	       X.GlobalReleaseHook = globalReleaseFunction;
	       )
	  else error "expected a type";
	  );
     globalAssignment {Type,Function};
     scan(dictionaryPath, dict -> (
	       scan(pairs dict, (nm,sym) -> (
			 x := value sym;
			 if instance(x, Function) or instance(x, Type) then setAttribute(x,ReverseDictionary,sym)))));

     applicationDirectorySuffix = () -> (
	  if version#"operating system" === "MacOS" then "Library/Application Support/Macaulay2/" else ".Macaulay2/"
	  );
     applicationDirectory = () -> (
	  if instance(applicationDirectorySuffix, Function)
	  then homeDirectory | applicationDirectorySuffix()
	  else homeDirectory | applicationDirectorySuffix
	  );

     dumpdataFile = null;
     )

sourceHomeDirectory = null				    -- home directory of Macaulay 2
buildHomeDirectory  = null	       -- parent of the directory of the executable described in command line argument 0
prefixDirectory = null					    -- prefix directory, after installation, e.g., "/usr/local/"
encapDirectory = null	   -- encap directory, after installation, if present, e.g., "/usr/local/encap/Macaulay2-0.9.5/"

fullCopyright := false
matchpart := (pat,i,s) -> substring_((regex(pat, s))#i) s
notdir := s -> matchpart("[^/]*$",0,s)
dir := s -> ( m := regex(".*/",s); if 0 == #m then "./" else substring_(m#0) s)
noloaddata := false
nosetup := false
noinitfile = false
interpreter := commandInterpreter

getRealPath := fn -> (					    -- use this later if realpath doesn't work
     local s;
     while ( s = readlink fn; s =!= null ) do fn = if isAbsolutePath s then s else minimizeFilename(fn|"/../"|s);
     fn)

pathsearch := e -> (
     if not isFixedExecPath e then (
	  -- we search the path, but we don't do it the same way execvp does, too bad.
	  PATH := separate(":",if "" =!= getenv "PATH" then getenv "PATH" else ".:/bin:/usr/bin");
	  PATH = apply(PATH, x -> if x === "" then "." else x);
	  scan(PATH, p -> if fileExists (p|"/"|e) then (e = p|"/"|e; break));
	  );
     e)

exe := (
     processExe := "/proc/self/exe";  -- this is a reliable way to get the executable in linux
     if fileExists processExe then realpath processExe
     else realpath pathsearch commandLine#0)
bindir := dir exe
bindirsuffix := LAYOUT#"bin";

setPrefixFromBindir := bindir -> if bindir =!= null then (
     if bindirsuffix === substring(bindir,-#bindirsuffix) then (
	  prefixdir := substring(bindir,0,#bindir-#bindirsuffix);
 	  setup := prefixdir | LAYOUT#"m2" | "setup.m2";
	  if fileExists setup then prefixDirectory = realpath prefixdir | "/"
	  else (
	      stderr
	      << "--warning: expected setup file here: " << setup << endl
	      << "--:        because we seem to be running an " << version#"M2 name" << " found in " << bindir << endl
	      << "--:        Perhaps " << version#"M2 name" << " has been moved or copied to a different directory." << endl;
	      error "file setup.m2 not found"))
     else (
	  stderr
	  << "--warning: expected directory " << bindir << " to end with " << bindirsuffix << endl
     	  << "--:        because we seem to be running an " << version#"M2 name" << " found there." << endl
	  << "--:        Perhaps " << version#"M2 name" << " has been moved or copied to a different directory." << endl;
	  error "file setup.m2 not found"))

if fileExists (bindir | "../c/scc1") then (
     -- we're running from the build directory
     buildHomeDirectory = minimizeFilename(bindir|"../");
     sourceHomeDirectory = (
	  if fileExists (buildHomeDirectory|"m2/setup.m2") then buildHomeDirectory 
	  else (
	       srcdirfile := buildHomeDirectory|"srcdir";
	       if fileExists srcdirfile then (
		    srcdir := minimizeFilename (concatPath(buildHomeDirectory,first lines get srcdirfile)|"/");
		    setup := srcdir | "m2/setup.m2";
		    if fileExists setup then srcdir
		    else ( stderr << "--warning: file missing: " << setup << endl; )
		    )
	       else ( stderr << "--warning: file missing: " << srcdirfile << endl; )));
     ) else (
     -- we hope we're running from an installed version with files still in the right place
     setPrefixFromBindir bindir)

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
     << "                       and when evaluating command line arguments" << newline
     << "    --no-prompts       print no input prompts" << newline;
     << "    --no-readline      don't use readline" << newline;
     << "    --no-setup         don't try to load setup.m2 or to loaddata" << newline
     << "    --no-personality   don't set the personality and re-exec M2 (linux only)" << newline
     << "    --prefix DIR       set prefixDirectory" << newline
     << "    --print-width n    set printWidth=n (the default is the window width)" << newline
     << "    --restarted        used internally to indicate this is a restart" << newline
     << "    --script           as first argument, interpret second argument as name of a script" << newline
     << "                       implies --stop, --no-debug, --silent and -q" << newline
     << "                       see scriptCommandLine" << newline
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
     ;)

tryLoad := (ofn,fn) -> if fileExists fn then (
     r := simpleLoad fn;
     markLoaded(fn,ofn,notify);
     true) else false

loadSetup := () -> (
     sourceHomeDirectory =!= null and tryLoad("setup.m2", minimizeFilename(sourceHomeDirectory | "/m2/setup.m2"))
     or
     prefixDirectory =!= null and tryLoad("setup.m2", minimizeFilename(prefixDirectory | LAYOUT#"m2" | "setup.m2"))
     or
     error "file setup.m2 not found"
     )

showMaps := () -> (
     if version#"operating system" === "SunOS" then (
	  stack lines get ("!/usr/bin/pmap "|processID())
	  )
     else if version#"operating system" === "Linux" and fileExists("/proc/"|toString processID()|"/maps") then (
	  stack lines get("/proc/"|toString processID()|"/maps")
	  )
     else "memory maps not available"
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
     fnmaps := fn | ".maps";
     fnmaps << showMaps() << endl << close;
     runEndFunctions();
     dumpdataFile = toAbsolutePath fn;					    -- so we know after "loaddata" where we put the file
     collectGarbage();
     interpreterDepth = 0;
     stderr << "--dumping to " << fntmp << endl;
     dumpdata fntmp;
     stderr << "--success" << endl;
     moveFile(fntmp,fn,Verbose=>true);
     exit 0;
     )

argno := 1

action := hashTable {
     "-h" => arg -> (usage(); exit 0),
     "-mpwprompt" => notyeterr,
     "-n" => obsolete,
     "-q" => arg -> noinitfile = true,
     "-s" => obsolete,
     "-silent" => obsolete,
     "-tty" => notyet,
     "--copyright" => arg -> if phase == 1 then fullCopyright = true,
     "--dumpdata" => arg -> (noinitfile = noloaddata = true; if phase == 3 then dump()),
     "--help" => arg -> (usage(); exit 0),
     "--int" => arg -> arg,
     "--no-backtrace" => arg -> if phase == 1 then backtrace = false,
     "--no-debug" => arg -> debuggingMode = false,
     "--no-loaddata" => arg -> if phase == 1 then noloaddata = true,
     "--no-personality" => arg -> arg,
     "--no-prompts" => arg -> if phase == 2 then noPrompts(),
     "--no-readline" => arg -> arg,			    -- handled in d/stdio.d
     "--no-setup" => arg -> if phase == 1 then noloaddata = nosetup = true,
     "--notify" => arg -> if phase <= 2 then notify = true,
     "--no-tty" => arg -> arg,			    -- handled in d/stdio.d
     "--script" => arg -> error "--script option should be first argument, of two",
     "--silent" => arg -> nobanner = true,
     "--stop" => arg -> (if phase == 1 then stopIfError = true; debuggingMode = false;), -- see also M2lib.c and tokens.d
     "--restarted" => arg -> restarted = true,
     "--texmacs" => arg -> (
	  if phase == 1 then (
	       if not restarted then << TeXmacsBegin << "verbatim:";
	       )
	  else if phase == 2 then (
	       printWidth = 80;
	       )
	  else if phase == 3 then (
	       texmacsmode = true;
	       topLevelMode = TeXmacs;
	       addEndFunction(() -> if texmacsmode then (
			 if restarting 
			 then stderr << "Macaulay 2 restarting..." << endl << endl << flush
			 else (
			      stderr << "Macaulay 2 exiting" << flush;
			      << TeXmacsEnd << endl << flush)));
	       )
	  ),
     "--version" => arg -> ( << version#"VERSION" << newline; exit 0; )
     };

valueNotify := arg -> (
     if notify then stderr << "--evaluating command line argument " << argno << ": " << format arg << endl;
     value arg)

action2 := hashTable {
     "-E" => arg -> if phase == 2 then valueNotify arg,
     "-e" => arg -> if phase == 3 then valueNotify arg,
     "--print-width" => arg -> if phase == 2 then printWidth = value arg,
     "--prefix" => arg -> if phase == 1 or phase == 2 then (
	  if not match("/$",arg) then arg = arg | "/";
	  prefixDirectory = arg;
	  )
     }

scriptCommandLine = {}

processCommandLineOptions := phase0 -> (			    -- 3 passes
     ld := loadDepth;
     loadDepth = loadDepth + 1;
     phase = phase0;
     argno = 1;
     if commandLine#?1 and commandLine#1 == "--script" then (
	  if phase <= 2 then (
	       clearEcho stdio;
	       debuggingMode = false;
	       stopIfError = noinitfile = nobanner = true;
	       )
	  else if phase == 3 then (
	       if not commandLine#?2 then error "script file name missing";
	       arg := commandLine#2;
	       scriptCommandLine = drop(commandLine,2);
	       if instance(load, Function) then load arg else simpleLoad arg;
	       exit 0))
     else (
	  if notify then stderr << "--phase " << phase << endl;
	  while argno < #commandLine do (
	       arg = commandLine#argno;
	       if action#?arg then action#arg arg
	       else if action2#?arg then (
		    if argno < #commandLine + 1
		    then action2#arg commandLine#(argno = argno + 1)
		    else error("command line option ", arg, " missing argument")
		    )
	       else if arg#0 == "-" then (
		    stderr << "error: unrecognized command line option: " << arg << endl;
		    usage();
		    exit 1;
		    )
	       else if phase == 3 then if instance(load, Function) then load arg else simpleLoad arg;
	       argno = argno+1;
	       );
	  loadDepth = ld;
	  ))

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
     if fileExists datafile then (
	  if notify then stderr << "--loading cached memory data from " << datafile << newline << flush;
     	  try loaddata(notify,datafile);
	  if notify then stderr << "--warning: unable to load data from " << datafile << newline << flush;
	  )
     )

scan(commandLine, arg -> if arg === "-q" or arg === "--dumpdata" then noinitfile = true)
homeDirectory = getenv "HOME" | "/"

path = (x -> select(x, i -> i =!= null)) deepSplice {
	  if not noinitfile then (
	       applicationDirectory() | "local/" | LAYOUT#"packages", 
	       applicationDirectory() | "local/" | LAYOUT#"datam2",
	       applicationDirectory() | "code/"
	       ),
	  if prefixDirectory =!= null then (
	       prefixDirectory | LAYOUT#"packages",
	       prefixDirectory | LAYOUT#"m2", 
	       prefixDirectory | LAYOUT#"datam2"),
	  if sourceHomeDirectory =!= null then (
	       sourceHomeDirectory|"m2/",
	       sourceHomeDirectory|"packages/"
	       ),
	  if buildHomeDirectory =!= null then (
	       buildHomeDirectory|"tutorial/final/",
	       if buildHomeDirectory =!= sourceHomeDirectory then (
		    buildHomeDirectory|"m2/",
		    buildHomeDirectory|"packages/")),
	  if sourceHomeDirectory =!= null then (
	       sourceHomeDirectory|"test/", 
	       sourceHomeDirectory|"test/engine/")
	  }

if firstTime then normalPrompts()

printWidth = fileWidth stdio

if firstTime and not nosetup then loadSetup()

-- remove the Core private dictionary -- it was added by "debug" above
-- and install a local way to use private global symbols
local core
if not nosetup then (
     dictionaryPath = select(dictionaryPath, d -> d =!= Core#"private dictionary");
     core = nm -> value Core#"private dictionary"#nm;
     ) else (
     core = nm -> value getGlobalSymbol nm
     )

processCommandLineOptions 2
(core "runStartFunctions")()
errorDepth = loadDepth
if class Core =!= Symbol and not core "noinitfile" then (
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
