--- status: Rewritten June 2020
--- author(s): Mahrud Sayrafi
--- notes: functions below are all defined in help.m2
--- FIXME: help "Macaulay2" doesn't do what this page ways

undocumented {"Body"} -- about

doc ///
  Key
     help
    (help, Array)
    (help, DocumentTag)
    (help, List)
    (help, Sequence)
    (help, String)
    (help, Symbol)
    (help, Thing)
  Headline
    view documentation nodes
  Description
    Text
      Various ways to get help:

      @UL {
          (TT "help \"Macaulay2\"",          " -- displays the base of the online documentation tree."),
          (TT "help X",                      " -- displays the online documentation for ", TT "X", "."),
          (TT "help methods T",              " -- displays help messages about the methods that take an object of class ", TT "T", "as input."),
          (TT "help methods res",            " -- displays help messages about various uses the function ", TT "res", "."),
          (TT "help methods symbol **",      " -- displays help messages about various uses and meanings of the operator ", TT "**", "."),
          (TT "help methods (map, Module)",  " -- displays help messages about various ways to use the function ", TT "map", " and a module."),
          (TT "help methods (symbol **, T)", " -- displays help messages about various ways to use the operator ", TT "**", " and an object of class ", TT "T", "."),
          (TT "help methods (X, Y)",         " -- displays help messages about the methods usable with an object of class ", TT "X", " and an object of class ", TT "Y", "."),
          (TT "help apropos \"hilbert\"",    " -- displays help messages about all functions whose name contains the string ", TT "hilbert", "."),
          (TT "help about X",                " -- displays documentation nodes from all installed packages whose keys contain ", TT "X", "."),
          (TT "help about(X, Body => true)", " -- displays documentation nodes from all installed packages whose keys or contents contain ", TT "X", ".")
          }@

      The @TT "help"@ command is used to display online documentation, as in the following suggestions.
      Use @TO viewHelp@ to display the corresponding documentation in your web browser.

      @UL {
          TT "help",
          TT "help ideal",
          TT "help(ideal, List)"
          }@

      Some other potential help topics:

      @UL {
          TT "help \"monomial orderings\"",
          TT "help \"Gröbner bases\"",
          TT "help \"multigraded polynomial rings\""
          }@
  SeeAlso
    "initial help"
    (symbol?, Symbol)
    viewHelp
    infoHelp
    apropos
    about
    code
    methods
    examples
///

doc ///
  Key
     viewHelp
    (viewHelp, Thing)
    (viewHelp, String)
  Headline
    view online documentation in a web browser
  Usage
    viewHelp
    viewHelp X
  Inputs
    X:Thing
      a descriptor for a documentation node (see below for examples)
  Consequences
    Item
      The given documentation page is displayed in your default web browser, as determined
      by either @TT "open"@ on macOS or @TT "xdg-open"@ on Linux distributions.
      As backup for when neither @TT "open"@ nor @TT "xdg-open"@ is available,
      the environmental variable @TT "WWWBROWSER"@ or @TT "firefox"@ is used.

     If no argument is given to @TT "viewHelp"@ then the top page of your local html
     documentation is displayed.
  Description
    Text
      Some example uses:

      @UL {
          (TT "viewHelp",                            " -- top of local copy of the documentation, including installed packages"),
          (TT "viewHelp \"Macaulay2\"",              " -- top of Macaulay2 doc"),
          (TT "viewHelp ideal",                      " -- online doc for the 'ideal' function"),
          (TT "viewHelp \"matrices\"",               " -- overview of matrices in Macaulay2"),
          (TT "viewHelp (ideal, List)",              " -- online doc for ideal(List) method"),
          (TT "viewHelp (diff, Matrix, Matrix)",     " -- online doc for the diff function taking two matrices as arguments"),
          (TT "viewHelp [gb, DegreeLimit]",          " -- view doc for the optional argument DegreeLimit to gb function"),
          (TT "viewHelp (symbol**, Matrix, Matrix)", " -- view doc for Matrix**Matrix")
          }@
  Caveat
    The @TO help@ command allows other possible arguments, such as @TT "help methods ideal"@,
    but for @TT "viewHelp"@ the argument @TT "X"@ must refer to only one web page.
  SeeAlso
    infoHelp
    help
    about
    apropos
    makePackageIndex
///

doc ///
  Key
    infoHelp
  Headline
    view documentation in Info format
  Usage
    infoHelp X
  Inputs
    X:Thing
      a descriptor for a documentation node (see below for examples)
  Consequences
    Item
      The given documentation page is displayed using info, if you are running Macaulay2 in a terminal window.
  Description
    Text
      Some example uses:

      @UL {
          (TT "infoHelp \"Macaulay2\"",              " -- top of Macaulay2 doc"),
          (TT "infoHelp ideal",                      " -- online doc for the 'ideal' function"),
          (TT "infoHelp \"matrices\"",               " -- overview of matrices in Macaulay2"),
          (TT "infoHelp (ideal, List)",              " -- online doc for ideal(List) method"),
          (TT "infoHelp (diff, Matrix, Matrix)",     " -- online doc for the diff function taking two matrices as arguments"),
          (TT "infoHelp [gb, DegreeLimit]",          " -- view doc for the optional argument DegreeLimit to gb function"),
          (TT "infoHelp (symbol**, Matrix, Matrix)", " -- view doc for Matrix**Matrix")
          }@

      While in the @TT "info"@ program, there are many ways to navigate and search.
      Besides the arrow keys to move around on the page, here is a list of the most useful key strokes:

      @UL {
          (TT "?", " -- display information about all of the possible keystrokes"),
          (TT "q", " -- quit info, return to Macaulay2"),
          (TT "n", " -- go to the next documentation node"),
          (TT "p", " -- go to the revious node"),
          (TT "m", " -- follow the menu link"),
          (TT "r", " -- follow a cross-reference"),
          (TT "l", " -- go to the last node visited"),
          }@

  Caveat
    The @TO help@ command allows other possible arguments, such as @TT "help methods ideal"@,
    but @TT "infoHelp"@ requires that the argument @TT "s"@ refer to only one documentation page.

    @HEADER2 "Viewing Info files in Emacs"@

    Reading the info form of the documentation in Emacs is perhaps better than using @TO "infoHelp"@,
    as the preferred way of running Macaulay2 is in Emacs. If you do so, we recommend configuring
    the value of the Emacs variable @TT "Info-hide-note-references"@ to @TT "hide"@ in order to
    prevent Emacs from inserting a superfluous @TT "See"@ or @TT "see"@ in front of the hyperlinks.
  SeeAlso
    viewHelp
    help
    about
    apropos
///

doc ///
Node
  Key
    (symbol?, Symbol)
    (symbol?, ScriptedFunctor)
    (symbol?, Function)
    (symbol?, Keyword)
    (symbol?, Package)
    (symbol?, Type)
  Headline
    view brief documentation of a symbol
  Description
    Text
      A question mark followed by a symbol representing a function, type, option name, or package
      prints a brief, compact list of various ways to use the given symbol.

      @UL {
          (TT "? Tally",    " -- displays brief information about ways to use objects of class ", TT "Tally", "."),
          (TT "? betti",    " -- displays brief information about ways to use the function ", TT "betti", "."),
          (TT "? Weights",  " -- displays brief information about ways to use the option ", TT "Weights", "."),
          (TT "? Package",  " -- displays brief information about the package ", TT "Package", "."),
          (TT "? symbol>>", " -- displays brief information about ways to use the operator ", TT ">>", "."),
          (TT "? HH",       " -- displays brief information about ways to use the scripted functor ", TT "HH", ".")
          }@

      When using Macaulay2 in Emacs, moving the cursor to each line of the output beginning with
      @TT "*"@ and pressing Enter results in showing the full documentation node corresponding to that line.

  SeeAlso
    "initial help"
    methods
    help
    code
    about
///

doc ///
Node
  Key
     about
    [about, Body]
    (help, ZZ)
    (viewHelp, ZZ)
    (about, Function)
    (about, String)
    (about, Symbol)
    (about, Type)
  Headline
    search the documentation
  Usage
    about s
  Inputs
    s:{String, Function, Symbol, Type}
    Body=>Boolean
      whether also to search the bodies of the documentation nodes. By default, just their keys are searched.
  Outputs
    :NumberedVerticalList
      a list of documentation node keys matching the regular expression in the string @TT "s"@, if @TT "s"@ is a string.
      Otherwise the search matches against the name of @TT "s"@ as a complete word.
  Description
    Text
      The documentation corresponding to the keys in the list returned can be displayed by applying the function
      @TO "help"@ to it. To see the documentation corresponding to just one or some of the keys, give @TO "help"@
      an integer or a list of integers to be used as indices in the list returned by the most recent application
      of @TO "about"@. The function @TO "viewHelp"@ can also be given an integer for viewing the documentation.

      The packages searched are the loaded packages and the packages installed under one of the prefixes listed
      in @TO "prefixPath"@. The first search will take a few seconds while it reads all the documentation keys
      into memory.

    -- this example won't work until after Macaulay2Doc is installed.
    CannedExample
      about resolution
      help 5
  Caveat
    Since @TT "s"@ is taken as a regular expression, parentheses serve
    for grouping subexpressions, rather than matching themselves.
  SeeAlso
    help
    (symbol?, Symbol)
    apropos
///

-- the node displayed by the help command by default
doc ///
Node
  Key
    "initial help"
  Description
    Text
      Welcome to Macaulay2

      Try entering '2+2' at your next input prompt, which begins with @TT "i"@.
      The two output prompts begin with @TT "o"@.  The first one, with the
      equal sign, '=', gives the value computed from your input, and the second one, with
      the colon, ':', tells what type of thing the value is.

      Type one of these commands to get started reading the documentation:

      @UL {
	  SPAN (////copyright////, ////                         -- the copyright////),
	  SPAN (////help "Macaulay2"////, ////                  -- top node of the documentation.////),
	  SPAN (////help "reading the documentation"////, ////  -- ////),
	  SPAN (////help "getting started"////, ////            -- ////),
	  SPAN (////help "a first Macaulay2 session"////, ////  -- ////),
	  SPAN (////help x////, ////                            -- show documentation for ////, TT ////x////),
	  SPAN (////help about x////, ////                      -- show documentation about ////, TT ////x////),
	  SPAN (////help about (x,Body=>true)////, ////         -- show documentation mentioning ////, TT ////x////),
	  SPAN (////? f////, ////                               -- display brief documentation for a function ////, TT ////f////),
	  SPAN (////printWidth = 80////, ////                   -- set print width to 80 characters////),
	  SPAN (////viewHelp////, ////                          -- view documentation in a browser////),
	  SPAN (////viewHelp x////, ////                        -- view documentation on ////, TT ////x////, //// in browser////),
	  }@

      To read the documentation in info form, in case you happen to be running Macaulay2 in a
      terminal window, replace @TO "help"@ by @TO "infoHelp"@ in any of the commands above.
///
