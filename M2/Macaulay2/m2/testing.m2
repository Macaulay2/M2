-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

sourceFileStamp = (filename, linenum) -> concatenate(
    "--", toAbsolutePath filename, ":", toString linenum, ": location of test code")

-----------------------------------------------------------------------------
-- TEST
-----------------------------------------------------------------------------

TEST = method(Options => {FileName => false})
TEST List   := opts -> testlist   -> apply(testlist, test -> TEST(test, opts))
TEST String := opts -> teststring -> (
    n := currentPackage#"test number";
    currentPackage#"test inputs"#n = if opts.FileName then (
        minimizeFilename teststring, 1,
        concatenate(sourceFileStamp(teststring, 1), newline, get teststring)
        ) else (
        minimizeFilename currentFileName, currentLineNumber(),
        concatenate(sourceFileStamp(currentFileName, currentLineNumber()),
            newline, teststring));
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
	(err, output) := capture(teststring, PackageExports => pkg, UserMode => usermode);
	if err then printerr "capture failed; retrying ..." else return true);
    -- fallback to using an external process
    stderr << commentize pad("running " | desc, 72) << flush;
    runString(teststring, pkg, usermode))

loadTestDir := pkg -> (
    if pkg#?"test directory loaded" then return;
    testDir := pkg#"package prefix" |
        replace("PKG", pkg#"pkgname", currentLayout#"packagetests");
    if fileExists testDir then (
        tmp := currentPackage;
        currentPackage = pkg;
        TEST(sort apply(select(readDirectory testDir, file ->
            match("\\.m2$", file)), test -> testDir | "/" | test),
            FileName => true);
        currentPackage = tmp;
        pkg#"test directory loaded" = true;
    ) else pkg#"test directory loaded" = false;
)

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

    if pkg#"pkgname" == "Core" then loadTestDir(pkg);

    errorList := {};
    (hadError, numErrors) = (false, 0);
    scan(tests, k -> (
	    (filename, lineno, teststring) := pkg#"test inputs"#k;
	    desc := "check(" | toString k | ", " | format pkg#"pkgname" | ")";
	    ret := elapsedTime captureTestResult(desc, teststring, pkg, usermode);
	    if not ret then errorList = append(errorList,
		 (k, temporaryFilenameCounter - 2))));
    outfile := k -> temporaryDirectory() | toString k | ".tmp";
    if hadError then (
	if opts.Verbose then apply(last \ errorList, k -> (
		(filename, lineno, teststring) := pkg#"test inputs"#k;
		stderr << filename << ":" << lineno - 1 << ":1: error:" << endl;
		printerr get("!tail " | outfile k)));
	error("test(s) #", demark(", ", toString \ first \ errorList), " of package ", toString pkg, " failed.")))
