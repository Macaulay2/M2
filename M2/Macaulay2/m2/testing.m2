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

checkmsg := (verb, desc) ->
    stderr << commentize pad(pad(verb, 10) | desc, 72) << flush;

captureTestResult := (desc, teststring, pkg, usermode) -> (
    stdio << flush; -- just in case previous timing information hasn't been flushed yet
    if match("no-check-flag", teststring) then (
	checkmsg("skipping", desc);
	return true);
    -- TODO: remove this when capture uses ArgQ
    if usermode === not noinitfile then
    -- try capturing in the same process
    if isCapturable(teststring, pkg, true) then (
	checkmsg("capturing", desc);
	-- TODO: adjust and pass argumentMode, instead. This can be done earlier, too.
	-- Note: UserMode option of capture is not related to UserMode option of check
	(err, output) := capture(teststring, PackageExports => pkg, UserMode => false);
	if err then printerr "capture failed; retrying ..." else return true);
    -- fallback to using an external process
    checkmsg("running", desc);
    runString(teststring, pkg, usermode))

loadTestDir := pkg -> (
    testDir := pkg#"package prefix" |
        replace("PKG", pkg#"pkgname", currentLayout#"packagetests");
    if fileExists testDir then (
        tmp := currentPackage;
        currentPackage = pkg;
        TEST(sort apply(select(readDirectory testDir, file ->
            match("\\.m2$", file)), test -> testDir | test),
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
	printerr("warning: skipping tests; ", toString pkg, " requires optional components"); return);
    usermode := if opts.UserMode === null then not noinitfile else opts.UserMode;
    --
    use pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    if not pkg#?"test directory loaded" then loadTestDir pkg;
    tests := if n == -1 then toList(0 .. pkg#"test number" - 1) else {n};
    if #tests == 0 then printerr("warning: ", toString pkg,  " has no tests");
    --
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
	if opts.Verbose then apply(errorList, (j, k) -> (
		(filename, lineno, teststring) := pkg#"test inputs"#j;
		stderr << filename << ":" << lineno - 1 << ":1: error:" << endl;
		printerr getErrors(outfile k)));
	error("test(s) #", demark(", ", toString \ first \ errorList), " of package ", toString pkg, " failed.")))

checkAllPackages = () -> (
    tmp := argumentMode;
    argumentMode = defaultMode - SetCaptureErr - SetUlimit -
	if noinitfile then 0 else ArgQ;
    fails := for pkg in sort separate(" ", version#"packages") list (
	stderr << HEADER1 pkg << endl;
	if runString("check(" | format pkg | ", Verbose => true)",
	    Core, false) then continue else pkg) do stderr << endl;
    argumentMode = tmp;
    if #fails > 0 then printerr("package(s) with failing tests: ",
	demark(", ", fails));
    return #fails;
)
