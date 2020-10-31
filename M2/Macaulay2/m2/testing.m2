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
-- TODO: Why LoadDocumentation => true?

prep := pkg -> (
    use pkg;
    if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"pkgname", LoadDocumentation => true, Reload => true);
    (hadError, numErrors) = (false, 0); pkg)

onecheck = (n, pkg, usermode) -> (
     (filename, lineno, teststring) := pkg#"test inputs"#n;
     stderr << "-* running test " << n << " of package " << pkg << " in file:" << endl;
     stderr << "   " << filename << ":" << lineno << ":1:" << endl;
     stderr << "   rerun with: check_" << n << " \"" << pkg << "\" *-" << endl;
     runString(teststring, pkg, usermode);
     )

check = method(Options => {UserMode => null, Verbose => false})
check String  := opts -> pkg -> check(-1, pkg, opts)
check Package := opts -> pkg -> check(-1, pkg, opts)

check(ZZ, String)  := opts -> (n, pkg) -> check(n, needsPackage (pkg, LoadDocumentation => true), opts)
check(ZZ, Package) := opts -> (n, pkg) -> (
    if not pkg.Options.OptionalComponentsPresent then (
        stderr << "--warning: optional components required for " <<
            toString pkg << " tests are not present; skipping" << endl;
        return);
    pkg = prep pkg;
    errorList := {};
    scan(if n == -1 then keys pkg#"test inputs" else {n}, k -> (
            previousNumErrors := numErrors;
            onecheck(k, pkg, if opts.UserMode === null then not noinitfile else opts.UserMode);
            if numErrors > previousNumErrors then
                errorList = append(errorList, k)));
    if hadError then error("test", if numErrors > 1 then "s" else "",
        " #", demark(", ", toString \ errorList),
        " of package ", toString pkg, " failed",
        if opts.Verbose then (
            numTests := if n == -1 then pkg#"test number" else 1;
            ":" | newline |
            concatenate(apply(errorList, k ->
                newline | get("!tail " | temporaryDirectory() |
                toString(temporaryFilenameCounter + 2 * (k - numTests)) |
                ".tmp")))
        ) else "");)
