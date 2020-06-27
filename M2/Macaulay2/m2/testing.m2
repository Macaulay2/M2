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
    currentPackage#"test inputs"#n = (currentFileName, currentLineNumber(),
        concatenate(sourceFileStamp(), newline, teststring));
    currentPackage#"test number" = n + 1;)
-- TODO: support test titles
TEST(String, String) := (title, teststring) -> (
    n := currentPackage#"test number"; () -> check(n - 1, currentPackage))

-----------------------------------------------------------------------------
-- check
-----------------------------------------------------------------------------
-- TODO: Why LoadDocumentation => true?

prep := pkg -> (
    use pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    (hadError, numErrors) = (false, 0); pkg)

onecheck = (n, pkg, usermode) -> (
     (filename, lineno, teststring) := pkg#"test inputs"#n;
     stderr << "-- running test " << n << " of package " << pkg << " on line " << lineno << " in file " << filename << endl;
     stderr << "--    rerun with: check_" << n << " \"" << pkg << "\"" << endl;
     runString(teststring, pkg, usermode);
     )

check = method(Options => {UserMode => null})
check String  := opts -> pkg -> check(needsPackage (pkg, LoadDocumentation => true), opts)
check Package := opts -> pkg -> (
    pkg = prep pkg;
    scan(keys pkg#"test inputs", n -> onecheck(n, pkg, if opts.UserMode === null then not noinitfile else opts.UserMode));
    if hadError then error(toString numErrors, " error(s) occurred running tests for package ", toString pkg);)

check(ZZ, String)  := opts -> (n, pkg) -> check(n, needsPackage (pkg, LoadDocumentation => true), opts)
check(ZZ, Package) := opts -> (n, pkg) -> (
    pkg = prep pkg;
    onecheck(n, pkg, if opts.UserMode === null then not noinitfile else opts.UserMode);
    if hadError then error("test #", toString n, " of package ", toString pkg, " failed");)
