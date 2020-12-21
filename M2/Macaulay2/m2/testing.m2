-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

sourceFileStamp = () -> concatenate(
    "--", toAbsolutePath currentFileName, ":", toString currentLineNumber(), ": location of test code")

-----------------------------------------------------------------------------
-- TEST
-----------------------------------------------------------------------------

TEST = method()
TEST List   := testlist   -> TEST \ testlist
TEST String := teststring -> (
    n := currentPackage#"test number";
    currentPackage#"test inputs"#n = (
        minimizeFilename currentFileName, currentLineNumber(),
        concatenate(sourceFileStamp(), newline, teststring));
    currentPackage#"test number" = n + 1;)
-- TODO: support test titles
TEST(String, String) := (title, teststring) -> (
    n := currentPackage#"test number"; () -> check(n - 1, currentPackage))

-----------------------------------------------------------------------------
-- check
-----------------------------------------------------------------------------

-- returns false if the inputs or the package are not known to behave well with capture
-- also see the one in examples.m2
isCapturableTest := (inputs, pkg) -> (
    not match("no-capture-flag", inputs) -- this flag is really necessary, but only sometimes
    -- FIXME: these are workarounds to prevent bugs, in order of priority for being fixed:
    and not match("(end|exit|restart)",                       inputs) -- these commands interrupt the interpreter
    and not match("(gbTrace|read|run|stderr|stdio|print|<<)", inputs) -- stderr and prints are not handled correctly
    and not match("([Cc]ommand|schedule|thread|Task)",        inputs) -- remove when threads work more predictably
    and not match("(installMethod|load|export|newPackage)",   inputs) -- exports may land in the package User
    and not match("(GlobalAssignHook|GlobalReleaseHook)",     inputs) -- same as above
    and not match({"ThreadedGB", "RunExternalM2"},     pkg#"pkgname") -- TODO: eventually remove
    )

checkMessage := (verb, n, pkgname, filename, lineno) -> (
    stderr
    << commentize(verb, " check(", toString n, ", ", format pkgname, ") from source:") << endl
    << pad("   " | filename | ":" | lineno - 1 | ":1:", 76)                            << flush)

captureTestResult := (n, pkg, usermode) -> (
    stdio << flush; -- just in case previous timing information hasn't been flushed yet
    (filename, lineno, teststring) := pkg#"test inputs"#n;
    -- try capturing in the same process
    if isCapturableTest(teststring, pkg) then (
	checkMessage("capturing", n, pkg#"pkgname", filename, lineno);
	(err, output) := capture(teststring, UserMode => false, Package => pkg);
	if err then printerr "capture failed; trying again in an external process ..." else return true);
    -- fallback to using an external process
    checkMessage("running", n, pkg#"pkgname", filename, lineno);
    runString(teststring, pkg, usermode))

check = method(Options => {UserMode => null, Verbose => false})
check String  := opts -> pkg -> check(-1, pkg, opts)
check Package := opts -> pkg -> check(-1, pkg, opts)

check(ZZ, String)  := opts -> (n, pkg) -> check(n, needsPackage (pkg, LoadDocumentation => true), opts)
check(ZZ, Package) := opts -> (n, pkg) -> (
    if not pkg.Options.OptionalComponentsPresent then (
        stderr << "--warning: optional components required for " <<
            toString pkg << " tests are not present; skipping" << endl;
        return);
    --
    use pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    --
    errorList := {};
    (hadError, numErrors) = (false, 0);
    scan(if n == -1 then keys pkg#"test inputs" else {n}, k -> (
            ret := elapsedTime captureTestResult(k, pkg, if opts.UserMode === null then not noinitfile else opts.UserMode);
            if ret then errorList = append(errorList, k)));
    if hadError then error("test", if numErrors > 1 then "s" else "",
        " #", demark(", ", toString \ errorList),
        " of package ", toString pkg, " failed",
        if opts.Verbose then (
            numTests := if n == -1 then pkg#"test number" else 1;
            ":" | newline |
            concatenate(apply(if n == -1 then errorList else {0}, k ->
                newline | get("!tail " | temporaryDirectory() |
                toString(temporaryFilenameCounter + 2 * (k - numTests)) |
                ".tmp")))
        ) else "");)
