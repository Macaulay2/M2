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

captureTestResult := (desc, teststring, pkg, usermode) -> (
    stdio << flush; -- just in case previous timing information hasn't been flushed yet
    -- try capturing in the same process
    if isCapturable(teststring, pkg, true) then (
	stderr << commentize pad("capturing " | desc, 72) << flush;
	(err, output) := capture(teststring, Package => pkg, UserMode => usermode);
	if err then printerr "capture failed; retrying in a subprocess ..." else return true);
    -- fallback to using an external process
    stderr << commentize pad("running " | desc, 72) << flush;
    runString(teststring, pkg, usermode))

check = method(Options => {UserMode => null, Verbose => false})
check String  := opts -> pkg -> check(-1, pkg, opts)
check Package := opts -> pkg -> check(-1, pkg, opts)

check(ZZ, String)  := opts -> (n, pkg) -> check(n, needsPackage (pkg, LoadDocumentation => true), opts)
check(ZZ, Package) := opts -> (n, pkg) -> (
    if not pkg.Options.OptionalComponentsPresent then (
	printerr("warning: optional components required for ", toString pkg, " tests are not present; skipping"); return);
    usermode := if opts.UserMode === null then not noinitfile else opts.UserMode;
    --
    use pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    tests := if n == -1 then toList(0 .. pkg#"test number" - 1) else {n};
    --
    errorList := {};
    (hadError, numErrors) = (false, 0);
    scan(tests, k -> (
	    (filename, lineno, teststring) := pkg#"test inputs"#k;
	    desc := "check(" | toString k | ", " | format pkg#"pkgname" | ")";
	    ret := elapsedTime captureTestResult(desc, teststring, pkg, usermode);
	    if not ret then errorList = append(errorList, k)));
    outfile := k -> temporaryDirectory() | toString(temporaryFilenameCounter + 2 * (k - #tests - min tests)) | ".tmp";
    if hadError then (
	if opts.Verbose then apply(errorList, k -> (
		(filename, lineno, teststring) := pkg#"test inputs"#k;
		stderr << filename << ":" << lineno - 1 << ":1: error:" << endl;
		printerr get("!tail " | outfile k)));
	error("test(s) #", demark(", ", toString \ errorList), " of package ", toString pkg, " failed.")))
