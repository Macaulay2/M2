--- status: Rewritten July 2020
--- author(s): Mahrud
--- notes: functions below are all defined in exam.m2

doc ///
Node
  Key
     capture
    (capture, Net)
    (capture, List)
    (capture, String)
    [capture, UserMode]
    [capture, PackageExports]
  Headline
    evaluate Macaulay2 code and capture the output
  Usage
    (err, output) := capture str
  Inputs
    str:{Net,List,String}
      the Macaulay2 code to be evaluated
    UserMode=>Boolean
      indicates whether currently loaded packages and exported symbols
      should be accessible while @TT "str"@ is being evaluated.
    PackageExports=>List
      of packages to be loaded before evaluating @TT "str"@.  The current package, as given by @TO currentPackage@, will be included.
  Outputs
    err:Boolean
      whether the evaluation was interrupted because of an error
    output:String
      the result of the evaluation
  Description
    Text
      This function evaluates the given Macaulay2 code within the same process in order to
      speed up tasks such as generating example results or checking the package tests.
    Example
      (err, output) = capture examples(resolution, Ideal) -* no-capture-flag *-
      assert not err
    Text
      If @TT "UserMode => false"@ given, the dictionaries available to the user will not be
      affected by the evaluation and @TO collectGarbage@ is called afterwards.

      Errors occurred while evaluating @TT "str"@ do not cause an error outside of @TT "capture"@.
    Example
      (err, output) = capture //// stderr << "Checking a false statement:" << endl; 1/0 ////
      assert err
    Text
      Certain examples or tests might not be compatible with @TT "capture"@. In such cases,
      use the string @CODE "-* no-capture-flag *-"@ somewhere within @TT "str"@ so that the
      code is run in a separate Macaulay2 subprocess instead.
  SeeAlso
    check
    examples

Node
  Key
     examples
    (examples, Thing)
  Headline
    list the examples in documentation
  Usage
    examples s
  Inputs
    s:Thing
      a descriptor for a documentation node, acceptable by @TO makeDocumentTag@
  Outputs
    :Net
      containing examples of code provided in the documentation of @TT "s"@
  Description
    Text
      The output is returned as a @TO Net@ of height 0, so the examples will be
      displayed indented by just white space, allowing immediate entry.
    Example
      ex := examples(resolution, Ideal)
      last capture ex -* no-capture-flag *-
    Text
      Alternatively, one could use @TO "print"@ to display them with no indentation.
    Example
      print ex
  SeeAlso
    "reading the documentation"
    capture
    EXAMPLE
    help

Node
  Key
    EXAMPLE
   (EXAMPLE, PRE)
   (EXAMPLE, String)
   (EXAMPLE, VisibleList)
  Headline
    hypertext EXAMPLE item
  Usage
    EXAMPLE x
  Inputs
    x:{String,VisibleList,PRE}
      containing strings or objects of class @TO "PRE"@
  Outputs
    :TABLE
      a table containing the examples
  Description
    Text
      If the table is included in the input provided to @TO "document"@, the input string will be
      evaluated by @TO "installPackage"@ and the result will be displayed in the documentation.
      Each object of class @TO "PRE"@ will be inserted unchanged into the documentation as example output.
      For example, the code
    Pre
      EXAMPLE { "1+1" }
    Text
      produces a display that looks like this:
    Code
      EXAMPLE { "1+1" }
  SeeAlso
    examples
    hypertext
    document
    installPackage
///
