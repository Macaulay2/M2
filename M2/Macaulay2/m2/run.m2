-- TODO: parallelize testing

-- TODO: get rid of these
(hadError, numErrors) = (false, 0);

--test limits
utest := opt -> (
    cmd := "ulimit " | opt | "; ";
    if chkrun("2>/dev/null >/dev/null "|cmd) == 0 then cmd else "")
ulimit := utest "-c unlimited" | utest "-t 700" | utest "-m 850000" | utest "-s 8192" | utest "-n 512"

M2statusRegexp := "^--status:"
statusLines := file -> select(lines file, s -> match(M2statusRegexp,s))

M2errorRegexp := "^[^:\n]+:[0-9]+:[0-9]+:(\\([0-9]+\\)):\\[[0-9]+\\]: "
aftermatch := (pat,str) -> (
    m := regex(pat,str);
    if m === null then "" else substring(m#0#0,str))

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
    ret := runFile(inf, hash teststring, outf, tmpf, "test results", pkg, t->t, usermode, {});
    if ret then (rm inf; rm outf;);
    ret)

-- prefixes
SetUlimit      := 1 << 20 -* sets ulimits *-
GCMAXHEAP      := 1 << 21 -* sets GC_MAXIMUM_HEAP_SIZE=400M *-
GCSTATS        := 1 << 22 -* sets GC_PRINT_STATS=1 *-
GCVERBOSE      := 1 << 23 -* sets GC_PRINT_VERBOSE_STATS=1 *-
-- arguments
ArgQ           := 1 <<  0 -* add -q *-
ArgInt         := 1 <<  1 -* add --int *-
ArgNoBacktrace := 1 <<  2 -* add --no-backtrace *-
ArgNoDebug     := 1 <<  3 -* add --no-debug *-
ArgNoPreload   := 1 <<  4 -* add --no-preload *-
ArgNoRandomize := 1 <<  5 -* add --no-randomize *-
ArgNoReadline  := 1 <<  6 -* add --no-readline *-
ArgNoSetup     := 1 <<  7 -* add --no-setup *-
ArgNoThreads   := 1 <<  8 -* add --no-threads *-
ArgNoTTY       := 1 <<  9 -* add --no-tty *-
ArgNoTValues   := 1 << 10 -* add --no-tvalues *-
ArgNotify      := 1 << 11 -* add --notify *-
ArgSilent      := 1 << 12 -* add --silent *-
ArgStop        := 1 << 13 -* add --stop *-
ArgPrintWidth  := 1 << 14 -* add --print-width 77 *-
-- suffixes
SetInputFile   := 1 << 30 -* add <inf *-
SetOutputFile  := 1 << 31 -* add >>tmpf *-
SetCaptureErr  := 1 << 32 -* add 2>&1 *-

-* by default, the following commandline fixtures are used *-
defaultMode := (SetUlimit + GCMAXHEAP + ArgQ + ArgInt
    + ArgNoRandomize + ArgNoReadline + ArgSilent + ArgStop
    + ArgPrintWidth + SetInputFile + SetOutputFile + SetCaptureErr)
-* making this global, so it can be edited after entering debug Core *-
argumentMode = defaultMode

-- inf       input file
-- inputhash has of input file
-- outf      output file
-- tmpf      temp file
-- desc      description to print
-- pkg
-- announcechange
-- usermode
-- examplefiles
-- returns false if error
runFile = (inf, inputhash, outf, tmpf, desc, pkg, announcechange, usermode, examplefiles) -> (
     announcechange();
     stderr << "--making " << desc << ( if debugLevel > 0 then " in file " | outf else "" ) << endl;
     if fileExists outf then removeFile outf;
     pkgname := toString pkg;
     tmpf << "-- -*- M2-comint -*- hash: " << inputhash << endl << close; -- must match regular expression below
     rundir := temporaryFileName() | "-rundir/";
     makeDirectory rundir;
     -* The bits in the binary representation of argmode determine arguments to add.
        If the 64th bit is set, argumentMode modifies the defaultMode rather than overriding them. *-
     argmode := if 0 < argumentMode & (1<<64) then xor(defaultMode, argumentMode) else argumentMode;
     -* returns (" "|arg) if all bits in m are set in argmode *-
     readmode := (m, arg) -> if argmode & m == m then " " | arg else "";
     cmd := readmode(SetUlimit, ulimit);
     cmd = cmd | " cd " | rundir | ";";
     cmd = cmd | readmode(GCMAXHEAP,      "GC_MAXIMUM_HEAP_SIZE=400M");
     cmd = cmd | readmode(GCSTATS,        "GC_PRINT_STATS=1");
     cmd = cmd | readmode(GCVERBOSE,      "GC_PRINT_VERBOSE_STATS=1");
     cmd = cmd | " " | format toAbsolutePath commandLine#0;
     if argmode =!= defaultMode or not usermode then
     cmd = cmd | readmode(ArgQ,           "-q");
     cmd = cmd | readmode(ArgInt,         "--int");
     cmd = cmd | readmode(ArgNoBacktrace, "--no-backtrace");
     cmd = cmd | readmode(ArgNoDebug,     "--no-debug");
     cmd = cmd | readmode(ArgNoPreload,   "--no-preload");
     cmd = cmd | readmode(ArgNoRandomize, "--no-randomize");
     cmd = cmd | readmode(ArgNoReadline,  "--no-readline");
     cmd = cmd | readmode(ArgNoSetup,     "--no-setup");
     cmd = cmd | readmode(ArgNoThreads,   "--no-threads");
     cmd = cmd | readmode(ArgNoTTY,       "--no-tty");
     cmd = cmd | readmode(ArgNoTValues,   "--no-tvalues");
     cmd = cmd | readmode(ArgNotify,      "--notify");
     cmd = cmd | readmode(ArgSilent,      "--silent");
     cmd = cmd | readmode(ArgStop,        "--stop");
     cmd = cmd | readmode(ArgPrintWidth,  "--print-width 77");
     cmd = cmd | concatenate apply(srcdirs, d -> (" --srcdir ", format d));
     needsline := concatenate(" -e 'needsPackage(\"",pkgname,"\", Reload => true, FileName => \"",pkg#"source file","\")'");
     cmd = cmd | if pkgname != "Macaulay2Doc" then needsline else "";
     cmd = cmd | readmode(SetInputFile,   "<" | format inf);
     cmd = cmd | readmode(SetOutputFile,  ">>" | format toAbsolutePath tmpf);
     cmd = cmd | readmode(SetCaptureErr,  "2>&1");
     if debugLevel > 0 then stderr << cmd << endl;
     for fn in examplefiles do copyFile(fn,rundir | baseFilename fn);
     r := run cmd;
     if r == 0 then (
	  scan(reverse findFiles rundir, f -> if isDirectory f then (
		    -- under cygwin, it seems to take a random amount of time before the system knows the directory is no longer in use:
		    try removeDirectory f
		    else (
			 stderr << "--warning: *** removing a directory failed, waiting..." << endl;
			 sleep 1;
			 try removeDirectory f
			 else (
			      stderr << "--warning: *** removing a directory failed again, waiting..." << endl;
			      sleep 4;
			      removeDirectory f
			      )
			 )
		    ) else removeFile f);
	  moveFile(tmpf,outf);
	  return true;
	  );
     stderr << cmd << endl;
     stderr << tmpf << ":0:1: (output file) error: Macaulay2 " << describeReturnCode r << endl;
     stderr << aftermatch(M2errorRegexp,get tmpf);
     stderr << inf  << ":0:1: (input file)" << endl;
     scan(statusLines get inf, x -> stderr << x << endl);
     if # findFiles rundir == 1
     then removeDirectory rundir
     else stderr << rundir << ": error: files remain in temporary run directory after program exits abnormally" << endl;
     stderr << "M2: *** Error " << (if r<256 then r else r//256) << endl;
     if r == 2 then error "interrupted";
     hadError = true;
     numErrors = numErrors + 1;
     return false;
     )
