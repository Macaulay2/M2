--- status: Rewritten August 2020
--- author(s): Mike, Mahrud
--- notes: functions below are all defined in testing.m2

doc ///
Node
  Key
     symbol TEST
  Headline
    add a test for a package
  Usage
    TEST s
  Inputs
    s:String -- containing Macaulay2 code
  Consequences
    Item
      registers the string @TT "s"@ as a test of the @TO2 {"currentPackage", "current package"}@.
  Description
    Text
      This function should only occur in the documentation section of a package; i.e., after @TO beginDocumentation@.
      Use @TO check@ to run all of the tests associated to a package.

      For an example, see @TO "SimpleDoc :: docExample"@ and @TO "an example of a package"@.

      If a test should be skipped when running @TO "check"@, e.g., it is
      known to fail under certain circumstances, then the comment
      @TT "-* no-check-flag *-"@ may be added to @TT "s"@.
  Caveat
    When creating tests, try to ensure that they run relatively quickly.
  SeeAlso
    assert
    beginDocumentation
    check
///

doc ///
Node
  Key
     check
    (check, Package)
    (check, String)
    (check, ZZ, Package)
    (check, ZZ, String)
    (check, List, Package)
    (check, List, String)
    (check, List)
    (check, TestInput)
    (check, ZZ)
    [check, UserMode]
    [check, Verbose]
  Headline
    perform tests of a package
  Usage
    check pkg
    check(i, pkg)
    check(L, pkg)
  Inputs
    pkg:{Package,String}
      the package to test
    i:ZZ
      the index of a test to run
    L:List
      containing indices of tests to run, or {\tt {}} to run all tests
    UserMode=>Boolean
      if true, do not use the @TT "-q"@ in arguments to the Macaulay2 executable when running tests,
      thereby allowing it to load the user's @TO "initialization file"@, allowing it to load packages
      previously installed in the user's @TO2 {"applicationDirectory", "application directory"}@
      and allowing packages it loads to read their configuration files from the user's
      @TO2 {"applicationDirectory", "application directory"}@. If false, the @TT "-q"@ argument is added.
      If @TO "null"@, then add @TT "-q"@ if it appears as an option in @TO "commandLine"@.
    Verbose=>Boolean
      if true, then print the output of all failing tests
  Consequences
    Item
      the tests in the package @TT "pkg"@ are run (in separate Macaulay2 processes, with the random number
      seed initialized to 0), and any errors are reported.
    Item
      if @TT "i"@ is given, only the i-th test in the package is run and any errors are reported.
  Description
    Text
      It is important for package authors to provide tests to ensure that the package is functioning properly.
      One provides tests using the @TO symbol TEST@ function following the @TO beginDocumentation@ call in the source
      of the package.

      Optionally, one can store all tests in a @TT "tests.m2"@ file under the auxiliary subdirectory of
      the package and load the file from the main package source file.

      For example, to run the tests for the @TO "FirstPackage::FirstPackage"@ package, use:
    Example
      needsPackage "FirstPackage" -* no-capture-flag *-
      check_1 FirstPackage
      check FirstPackage
    Text
      Alternatively, if the package is installed somewhere accessible, one can do the following.
    Example
      check_1 "FirstPackage"
      check "FirstPackage"
    Text
      A @TO TestInput@ object (or a list of such objects) can also be run directly.
    Example
      tests(1, "FirstPackage")
      check oo
      tests "FirstPackage"
      check oo
    Text
      If only an integer is passed as an argument, then the test with that index from the last call to @TO tests@
      is run.
    Example
      tests "FirstPackage"
      check 1
  Caveat
    Currently, if the package was only partially loaded because the documentation was
    obtainable from a database (see @TO "beginDocumentation"@), then the package will be reloaded,
    this time completely, to ensure that all tests are considered; this may affect user objects
    of types declared by the package, as they may be not usable by the new instance of the
    package. In a future version, either the tests and the documentation will both be cached, or neither will.
  SeeAlso
    "packages"
    "creating a package"
    symbol TEST
    installPackage
    loadPackage
    tests
///

undocumented {(generateAssertions, List)}
document {
    Key => {
	generateAssertions,
       (generateAssertions, String)
    },
    Headline => "generate assert statements from experimental input",
    Usage => "generateAssertions x",
    Inputs => { "x" => { "a string whose non-comment non-blank lines are Macaulay2 expressions to be evaluated" } },
    Outputs => { { "a net whose lines are assert statements that assert that the expressions evaluate to the expected value, just computed" }},
    EXAMPLE {
	"generateAssertions ///
	2+2
	2^20
	///",
	///value \ unstack oo///
    }
}
