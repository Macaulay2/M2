doc ///
Node
  Key
    "writing documentation"
    "conventions for documentation"
  Description
    Text
      Documentation for user defined @TO "packages"@ and Macaulay2 itself is written using two main methods:

    Subnodes
      :Macaulay2 documentation methods
        :the @TO (document, List)@ function, using a list-based @TO2("hypertext list format", "hypertext")@ markup format;
        :the @TO "SimpleDoc :: doc(String)"@ function, using a string-based @TO "SimpleDoc :: SimpleDoc"@ format.
    Text
      It is then formatted via @TO "installPackage"@ as the documentation built into Macaulay2,
      the online HTML documentation, and the info pages. Much of the format and structure of the
      documentation is automatically generated. Each documentation entry must be part of a package,
      and occur after the @TO beginDocumentation@ section of the package.
    Text
      @HEADER3 "Documentation templates"@

      Each documentation entry is either an overview topic, or documentation of an individual feature,
      such as a symbol, a function name, a function call (that is, a function name, together with
      specific types of its arguments), an optional argument to a function, or a package.

      The easiest way to write documentation for an entry is to start with an examples as a template.
      See the documentation on either of the documentation methods above for templates and examples.

    Text
      @HEADER3 "The documentation writing cycle"@

      Start with the package that you wish to document, and select one, or several of the above
      examples or templates. Cycle through the following steps as you refine your documentation.

      @OL {
          {"Edit your documentation entries as desired"},
          {"Generate the html pages for your package, using ", PRE////installPackage("YourPackage")////},
          {"Review the generated HTML documentation using ", TO viewHelp, " which displays this page in a browser."}
          }@
    Text
      @HEADER3 "Documentation style conventions"@

      There are a few stylistic conventions that should be noted. While not hard and fast rules,
      keeping these stylistic conventions in mind makes for easier reading by users.

      @UL {
          {"Lowercase is used for all titles, unless a proper noun is being used."},
          {"Use ", TO TO, " to reference any Macaulay2 function, option, or variable occurring in the documentation as a hyperlink."},
          {"If one needs to refer to the ", TT "i", "-th coefficient of some object, then use the format as given here."},
          {TO Inputs, ", ", TO Outputs, ", and ", TO Consequences, " should not end with periods."}
          }@
  SeeAlso
    viewHelp
    installPackage
    "SimpleDoc :: SimpleDoc"
  Subnodes
    "hypertext list format"
    document
    EXAMPLE
    SYNOPSIS

Node
  Key
    "hypertext list format"
  Description
    Text
      Documentation text is composed of a list of text and hypertext items. A single text string, even though
      it is not a list, is generally accepted as a hypertext list. Macaulay2 examples may be included in the
      list using the @TO EXAMPLE@ tag. See @TO "Text :: Text"@ for the full list of hypertext types.

      Each element of the list may be a text string, or one of the elements below.
      The following items are used by themselves:

      @UL {
          TOH BR,
          TOH HR,
          }@

      Items that take a text string (or other hypertext list) as argument:

      @UL {
          TOH HEADER1,
          TOH HEADER2,
          TOH HEADER3,
          TOH HEADER4,
          TOH HEADER5,
          TOH HEADER6,
          TOH SUBSECTION,
          TOH PARA,
          TOH SPAN,
          TOH ITALIC,
          TOH TT,
          TOH EM,
          TOH PRE,
          TOH SUB,
          TOH SUP,
          }@

      Items that place hyperlinks into the documentation:

      @UL {
          TOH HREF,
          TOH TO,
          TOH TO2,
          TOH TOH
          }@

      Other useful hypertext elements:

      @UL {
          TOH OL,
          TOH UL,
          TOH LI,
          TOH DL,
          TOH DT,
          TOH DD,
          TOH CODE,
          TOH EXAMPLE
          }@

    Text
      @SUBSECTION "Example"@

      For example, the hypertext list:
    Code
      EXAMPLE{ PRE ////{  HR{}, "When referring to a ", EM "Macaulay2", " identifier such as ", TT "matrix",
              ", use the TT element, or use a cross-reference, as in ", TO matrix, ".  Incorporate ", EM "Macaulay2",
              " examples (during ", TO (installPackage,String), ") as illustrated here.", EXAMPLE "matrix{{1,2},{3,4}}", HR{} }////}
    Text
      when used as the input to @TO document@ produces:
    Code
      {  HR{}, "When referring to a ", EM "Macaulay2", " identifier such as ", TT "matrix",
          ", use the TT element, or use a cross-reference, as in ", TO matrix, ".  Incorporate ", EM "Macaulay2",
          " examples (during ", TO (installPackage,String), ") as illustrated here.", EXAMPLE "matrix{{1,2},{3,4}}", HR{} }
  SeeAlso
    html
    net
    info
    Hypertext
    (show, Hypertext)
    "writing documentation"

Node
  Key
    beginDocumentation
  Headline
    start documentation section of a package
  Usage
    beginDocumentation()
  Consequences
    Item
      Initiates the documentation section of a package.
      If the documentation has previously been processed and stored, then the rest of the file
      after the invocation of @TT "beginDocumentation"@ will be skipped. Otherwise the packages
      @TO "SimpleDoc :: SimpleDoc"@ and @TO "Text :: Text"@ will be loaded and the rest of the
      file will be loaded.
  Description
    Text
      Documentation for a package, and tests for the package, should be placed after this point
      in a package file. This way, documentation can be loaded separately, Macaulay2 examples
      in the documentation can be run, and the whole documentation can be stored in a database.

      For an example, see @TO "an example of a package"@.

      To write documentation without using the function @TT "beginDocumentation"@, which is just
      an optimization, use @TO needsPackage@ to load the packages @TO "SimpleDoc :: SimpleDoc"@
      and @TO "Text :: Text"@.
  SeeAlso
    installPackage
    TEST
    check
///
