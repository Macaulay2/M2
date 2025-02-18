needs "packages.m2"
needs "code.m2"
needs "run.m2"

-----------------------------------------------------------------------------
-- TestInput
-----------------------------------------------------------------------------
TestInput = new SelfInitializingType of HashTable
TestInput.synonym = "test input"

code TestInput := code @@ locate
toString TestInput := T -> T#"code"
locate TestInput := T -> T#"location"
net TestInput := lookup(net, Function)
precedence TestInput := lookup(precedence, Function)
editMethod TestInput := editMethod @@ locate
capture TestInput := opt -> T -> capture(toString T, opt)

-----------------------------------------------------------------------------
-- TEST
-----------------------------------------------------------------------------

-- TEST is a keyword that takes an object as input and determines its
-- location.  It then passes the object and its location to addTest.
addTest = method()
addTest(String, FilePosition) := (str, loc) -> (
    n := #currentPackage#"test inputs";
    currentPackage#"test inputs"#n = TestInput {
	"location" => loc,
	"code" => concatenate("-- test source: ", toString loc, newline, str),
	"package" => currentPackage,
	"number" => n})
-- the following is not called by TEST, but called directly when we want to
-- add a test from a file (used by loadTestDir)
addTest String := filename -> addTest(get filename,
    new FilePosition from(filename, 1, 1))
-- TODO: support test titles

-----------------------------------------------------------------------------
-- check
-----------------------------------------------------------------------------

checkmsg := (verb, desc) ->
    stderr << commentize pad(pad(verb, 10) | desc, printWidth - 36) << flush;

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
    testDir := (
	-- prioritize Core tests from source repository
	if (pkg === Core and topSrcdir =!= null and (
		dir := minimizeFilename(topSrcdir | "Macaulay2/tests/normal/");
		isDirectory dir)) then dir
	else pkg#"package prefix" | replace("PKG", pkg#"pkgname",
	    currentLayout#"packagetests"));
    pkg#"test directory loaded" =
    if fileExists testDir then (
	printerr("loading tests from ", testDir);
        tmp := currentPackage;
        currentPackage = pkg;
	scan(sort select(readDirectory testDir, file -> match("\\.m2$", file)),
	    test -> addTest(testDir | test));
        currentPackage = tmp;
        true) else false)

tests = method()
tests Package := pkg -> (
    if not pkg#?"test directory loaded" then loadTestDir pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    previousMethodsFound = new NumberedVerticalList from pkg#"test inputs"
    )
tests String := pkg -> tests needsPackage(pkg, LoadDocumentation => true)
tests(ZZ, Package) := tests(ZZ, String) := (i, pkg) -> (tests pkg)#i

check = method(Options => {UserMode => null, Verbose => false})
check String  :=
check Package := opts -> pkg -> check({}, pkg, opts)
check(ZZ, String)  :=
check(ZZ, Package) := opts -> (n, pkg) -> check({n}, pkg, opts)
check(List, String)  := opts -> (L, pkg) -> check(L, needsPackage (pkg, LoadDocumentation => true), opts)
check(List, Package) := opts -> (L, pkg) -> (
    if not pkg.Options.OptionalComponentsPresent then (
	printerr("warning: skipping tests; ", toString pkg, " requires optional components"); return);
    usermode := if opts.UserMode === null then not noinitfile else opts.UserMode;
    --
    use pkg;
    tmp := previousMethodsFound;
    inputs := tests pkg;
    previousMethodsFound = tmp;
    testKeys := if L == {} then toList(0..#inputs-1) else L;
    if #testKeys == 0 then printerr("warning: ", toString pkg,  " has no tests");
    --
    errorList := for k in testKeys list (
	    if not inputs#?k then error(pkg, " has no test #", k);
	    teststring := inputs#k#"code";
	    desc := "check(" | toString k | ", " | format pkg#"pkgname" | ")";
	    ret := elapsedTime captureTestResult(desc, teststring, pkg, usermode);
	    if not ret then (k, temporaryFilenameCounter - 2) else continue);
    outfile := errfile -> temporaryDirectory() | errfile | ".tmp";
    if #errorList > 0 then (
	if opts.Verbose then apply(errorList, (k, errfile) -> (
		stderr << concatenate(printWidth:"=") << endl;
		stderr << locate inputs#k << " error:" << endl;
		printerr getErrors(outfile errfile)));
	stderr << concatenate(printWidth:"=") << endl;
	printerr("Summary: ", toString(#errorList), " test(s) failed in package ", pkg#"pkgname", ":");
	printerr netList(Boxes => false, HorizontalSpace => 2,
	    apply(first \ errorList, i -> { "Test #"|i|".", toString locate tests_i pkg }));
	error("repeat failed tests with:", newline,
	    "  check({", demark(", ", toString \ first \ errorList), "}, ", format pkg#"pkgname", ")")))
check TestInput := opts -> t -> check({t#"number"}, t#"package", opts)
check ZZ := opts -> n -> check(previousMethodsFound#n, opts)
check List := opts -> T -> scan(T, t -> check(t, opts))

checkAllPackages = () -> (
    tmp := argumentMode;
    argumentMode = defaultMode - SetCaptureErr - SetUlimit -
	if noinitfile then 0 else ArgQ;
    fails := for pkg in sort separate(" ", version#"packages") list (
	stderr << HEADER2 pkg << endl;
	if runString("check(" | format pkg | ", Verbose => true)",
	    Core, false) then continue else pkg) do stderr << endl;
    argumentMode = tmp;
    if #fails > 0 then printerr("package(s) with failing tests: ",
	demark(", ", fails));
    #fails)
