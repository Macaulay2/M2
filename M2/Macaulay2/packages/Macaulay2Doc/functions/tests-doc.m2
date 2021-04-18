doc ///
  Key
    tests
    (tests, Package)
    (tests, String)
    TestInput
    (code, TestInput)
    (locate, TestInput)
    (toString, TestInput)
    (net, TestInput)
  Headline
    locate a package's tests
  Usage
    tests pkg
  Inputs
    pkg:Package
      or @ofClass String@
  Outputs
    :HashTable
  Description
    Text
      Returns @ofClass HashTable@ containing the tests for the given
      package.  Each key of this hash table is an integer, which would
      be passed as the first argument of @TO check@ to run the test.
      Each value is a @TT "TestInput"@ object.  These are printed with
      the location of the file so that you may quickly jump to the
      source code of the test when using Emacs.
    Example
      tests "FirstPackage"
      t = oo#0
    Text
      The @TO locate@ and @TO code@ functions do the expected thing
      when given a @TT "TestInput"@ object.
    Example
      locate t
      code t
    Text
      Passing only the key of the desired test to @TO code@ is
      supported as well.
    Example
      code 0
    Text
      You may also pass a @TT "TestInput"@ object to @TO edit@ to open the
      code in your favorite editor.
///
