-- TODO: parallelize testing
-- TODO: merge with capture.m2?

needs "system.m2"

-- see resetCounters and installPackage
numExampleErrors = 0 -- FIXME: this is not reentrant

--test limits
utest := opt -> (
    cmd := "ulimit " | opt | "; ";
    if chkrun("2>/dev/null >/dev/null "|cmd) == 0 then cmd else "")
ulimit := utest "-c unlimited" | utest "-t 700" | utest "-m 850000" | utest "-s 8192" | utest "-n 512"

M2statusRegexp := "^--status:"
statusLines := file -> select(lines file, s -> match(M2statusRegexp,s))

testLocation := inf -> (
    infcontents := get inf;
    -- TODO: is this feature ever used?
    scan(statusLines infcontents, printerr);
    m := regex("^-- test source: ([^\n]+)", infcontents);
    if m =!= null then substring(m#1, infcontents)
    else toString new FilePosition from (inf, 0, 1))

M2errorRegexp := "^[^:\n]+:[0-9]+:[0-9]+:(\\([0-9]+\\)):\\[[0-9]+\\]: "
aftermatch := (pat,str) -> (
    m := regex(pat,str);
    if m === null then "" else substring(m#0#0,str))

logLocation := tmpf -> concatenate(
    toString new FilePosition from (tmpf, 0, 1),
    ":(" | loadDepth | ")",
    ":[" | recursionDepth() | "]",
    ": ", aftermatch(M2errorRegexp, get tmpf))

describeReturnCode := r -> (
    if r % 256 == 0 then "exited with status code " | toString (r // 256)
    else "killed by signal " | toString (r % 128) | if r & 128 =!= 0 then " (core dumped)" else "")

runString = (teststring, pkg, usermode) -> (
    tfn := temporaryFileName();
    inf := tfn | ".m2";
    tmpf := tfn | ".tmp";
    outf := tfn | ".out";
    rm := fn -> if fileExists fn then removeFile fn;
    rmall := () -> rm \ {inf, tmpf, outf};
    inf << teststring << endl << close;
    ret := runFile(inf, hash teststring, outf, tmpf, pkg, identity, usermode, {});
    if ret then (rm inf; rm outf;);
    ret)

-- prefixes
SetUlimit      = 1 << 20 -* sets ulimits *-
GCMAXHEAP      = 1 << 21 -* sets GC_MAXIMUM_HEAP_SIZE=400M *-
GCSTATS        = 1 << 22 -* sets GC_PRINT_STATS=1 *-
GCVERBOSE      = 1 << 23 -* sets GC_PRINT_VERBOSE_STATS=1 *-
-- arguments
ArgQ           = 1 <<  0 -* add -q *-
ArgInt         = 1 <<  1 -* add --int *-
ArgNoBacktrace = 1 <<  2 -* add --no-backtrace *-
ArgNoDebug     = 1 <<  3 -* add --no-debug *-
ArgNoPreload   = 1 <<  4 -* add --no-preload *-
ArgNoRandomize = 1 <<  5 -* add --no-randomize *-
ArgNoReadline  = 1 <<  6 -* add --no-readline *-
ArgNoCore      = 1 <<  7 -* add --no-core *-
ArgNoThreads   = 1 <<  8 -* add --no-threads *-
ArgNoTTY       = 1 <<  9 -* add --no-tty *-
--no-tvalues was 1 << 10, now unused
ArgNotify      = 1 << 11 -* add --notify *-
ArgSilent      = 1 << 12 -* add --silent *-
ArgStop        = 1 << 13 -* add --stop *-
ArgPrintWidth  = 1 << 14 -* add --print-width 77 *-
ArgPrintWidthN = 77
-- suffixes
SetInputFile   = 1 << 30 -* add <inf *-
SetOutputFile  = 1 << 31 -* add >>tmpf *-
SetCaptureErr  = 1 << 32 -* add 2>&1 *-
-- used by CMake to signal whether check should use capture
NoCapture      = 1 << 63 -* don't use capture *-
-- used by CMake to modify defaultMode rather than overriding it
InvertArgs     = 1 << 64 -* negate the effect of argumentMode *-

-* by default, the following commandline fixtures are used *-
defaultMode  = (SetUlimit + GCMAXHEAP + ArgQ
    + ArgNoRandomize + ArgNoReadline + ArgSilent + ArgStop
    + ArgPrintWidth + SetInputFile + SetOutputFile + SetCaptureErr)
-* making this global, so it can be edited after entering debug Core *-
argumentMode = defaultMode

-- inf       input file
-- inputhash has of input file
-- outf      output file
-- tmpf      temp file
-- pkg
-- announcechange
-- usermode
-- examplefiles
-- returns false if error
runFile = (inf, inputhash, outf, tmpf, pkg, announcechange, usermode, examplefiles) -> (
     announcechange();
     if fileExists outf then removeFile outf;
     pkgname := toString pkg;
     tmpf << "-- -*- M2-comint -*- hash: " << inputhash << endl << close; -- must match regular expression below
     rundir := temporaryFileName() | "-rundir/";
     makeDirectory rundir;
     -* The bits in the binary representation of argmode determine arguments to add.
        If InvertArgs is set, argumentMode modifies the defaultMode rather than overriding them. *-
     argmode := if 0 < argumentMode & InvertArgs then defaultMode ^^ argumentMode else argumentMode;
     -* returns (" "|arg) if all bits in m are set in argmode *-
     readmode := (m, arg) -> if argmode & m == m then " " | arg else "";
     cmd := readmode(SetUlimit, ulimit);
     cmd = cmd | " cd " | rundir | ";";
     cmd = cmd | readmode(GCMAXHEAP,      if match("--enable-debug", version#"configure arguments")
	       	    	      	   	  then "GC_MAXIMUM_HEAP_SIZE=800M"
	       	    	      	   	  else "GC_MAXIMUM_HEAP_SIZE=400M");
     cmd = cmd | readmode(GCSTATS,        "GC_PRINT_STATS=1");
     cmd = cmd | readmode(GCVERBOSE,      "GC_PRINT_VERBOSE_STATS=1");
     cmd = cmd | " " | format(bindir | "M2-binary");
     if argmode =!= defaultMode or not usermode then
     cmd = cmd | readmode(ArgQ,           "-q");
     cmd = cmd | readmode(ArgInt,         "--int");
     cmd = cmd | readmode(ArgNoBacktrace, "--no-backtrace");
     cmd = cmd | readmode(ArgNoDebug,     "--no-debug");
     if pkgname != "Macaulay2Doc" then -- TODO: eventually remove this line
     cmd = cmd | readmode(ArgNoPreload,   "--no-preload");
     cmd = cmd | readmode(ArgNoRandomize, "--no-randomize");
     cmd = cmd | readmode(ArgNoReadline,  "--no-readline");
     cmd = cmd | readmode(ArgNoCore,      "--no-core");
     cmd = cmd | readmode(ArgNoThreads,   "--no-threads");
     cmd = cmd | readmode(ArgNoTTY,       "--no-tty");
     -- readmode is empty for 1 << 10
     cmd = cmd | readmode(ArgNotify,      "--notify");
     cmd = cmd | readmode(ArgSilent,      "--silent");
     cmd = cmd | readmode(ArgStop,        "--stop");
     cmd = cmd | readmode(ArgPrintWidth,  "--print-width " | ArgPrintWidthN);
     cmd = cmd | concatenate apply(srcdirs, d -> (" --srcdir ",
	     format relativizeFilename(rundir, d)));
     -- TODO: fix capture, add preloaded packages to Macaulay2Doc, then delete the following two lines
     needsline := concatenate(" -e 'needsPackage(",format pkgname,",Reload=>true,FileName=>",format pkg#"source file",")'");
     cmd = cmd | if not isMember(pkgname, {"Macaulay2Doc", "Core", "User"}) then needsline else "";
     cmd = cmd | readmode(SetInputFile,   "<" | format inf);
     cmd = cmd | readmode(SetOutputFile,  ">>" | format toAbsolutePath tmpf);
     cmd = cmd | readmode(SetCaptureErr,  "2>&1");
     for fn in examplefiles do copyFile(fn,rundir | baseFilename fn);
     r := run cmd;
     if r == 0 then (
	  scan(reverse findFiles rundir, f -> if isDirectory f then removeDirectory f else removeFile f);
	  moveFile(tmpf,outf);
	  return true;
	  );
     if gotarg "--check" then if r // 255 == 2 then exit 1 else return false;
     -- print debugging info
     printerr "running failed";
     printerr("error log:  " | logLocation tmpf);
     printerr("input code: " | testLocation inf);
     if debugLevel > 0 then
     stderr << commentize "full command:" << endl << cmd << endl;
     stderr << pad("*** error: M2 " | describeReturnCode r, printWidth - 32) << flush;
     -- cleanup run directory
     if # findFiles rundir == 1 then removeDirectory rundir
     else stderr << rundir << ": error: files remain in temporary run directory after program exits abnormally" << endl;
     -- detect user interrupts
     if r // 256 == 2 then error "interrupted";
     numExampleErrors += 1;
     return false;
     )
