doc ///
  Key
    tests
    (tests, Package)
    (tests, String)
    (tests, ZZ, Package)
    (tests, ZZ, String)
    TestInput
    (code, TestInput)
    (locate, TestInput)
  Headline
    locate a package's tests
  Usage
    tests pkg
    tests(i, pkg)
  Inputs
    i:ZZ
    pkg:{Package, String}
  Outputs
    :{NumberedVerticalList, TestInput}
  Description
    Text
      When an integer is not provided, this returns all the tests
      for the given package.  The position of each element would
      be passed as the first argument of @TO check@ to run the test.
      Each value is a @TT "TestInput"@ object.  These are printed with
      the location of the file so that you may quickly jump to the
      source code of the test when using Emacs.
    Example
      tests "FirstPackage"
    Text
      If the test number is also provided, then the corresponding
      @TT "TestInput"@ object is returned.
    Example
      t = tests(0, "FirstPackage")
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
