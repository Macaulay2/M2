--- status: Rewritten June 2020
--- author(s): Mahrud Sayrafi
--- notes: functions below are all defined in help.m2
--- FIXME: help "Macaulay2" doesn't do what this page ways

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
    Code
      TABLE {
	  { M2CODE "help",			  { " -- display the ", TO "initial help" }},
	  { M2CODE "help \"Macaulay2\"",	  { " -- display the top of the documentation tree" }},
          { M2CODE "help \"matrices\"",           { " -- display an overview of matrices in Macaulay2" }},
	  { M2CODE "help X",                      { " -- display the documentation node whose key is ", TT "X" }},
	  { M2CODE "help res",                    { " -- documentation for the function ", TO "OldChainComplexes :: res", " and ways to use it" }},
          { M2CODE "help(res, Module)",           { " -- documentation for the method ", TO "OldChainComplexes :: resolution(Module)" }},
	  { M2CODE "help Module",                 { " -- documentation for the type ", TO "Module", " and methods that take one as input" }},
	  { M2CODE "help symbol **",              { " -- documentation for the operator ", TO "**", " and its various uses and meanings" }},
          { M2CODE "help(symbol **, Ring, Ring)", { " -- documentation for the method ", TO (symbol **, Ring, Ring) }},
          { M2CODE "help[gb, DegreeLimit]",       { " -- documentation for the optional argument ", TO [gb, DegreeLimit] }},
	  { M2CODE "help methods(map, Module)",   { " -- documentation for various ways to use the function ", TO "map", " and a module" }},
	  { M2CODE "help methods(symbol **, T)",  { " -- documentation for various ways to use the operator ", TO "**", " and an object of class ", TT "T" }},
	  { M2CODE "help methods(X, Y)",          { " -- documentation for the methods that take an object of class ", TT "X", " and an object of class ", TT "Y" }},
	  { M2CODE "help apropos \"hilbert\"",    { " -- documentation about all functions whose name contains the string ", TT "hilbert" }},
	  { M2CODE "help about X",                { " -- documentation nodes from all installed packages whose keys contain ", TT "X" }},
	  { M2CODE "help about(X, Body => true)", { " -- documentation nodes from all installed packages whose keys or contents contain ", TT "X" }},
      }
    Text
      The @TT "help"@ command is used to display online documentation, as in the following suggestions.
    Code
      TABLE {
	  { M2CODE "help" },
          { M2CODE "help ideal" },
          { M2CODE "help(ideal, List)" },
      }
    Text
      Some other potential help topics:
    Code
      TABLE {
	  { M2CODE "help \"monomial orderings\"" },
          { M2CODE "help \"Gröbner bases\"" },
          { M2CODE "help \"graded and multigraded polynomial rings\"" },
      }
    Text
      Use @TO viewHelp@ to display the corresponding documentation in your web browser.
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
    (viewHelp, DocumentTag)
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
      The given documentation page is displayed using the function @TO show@
      which opens the page in the default web browser.
    Item
      If no argument is given to @TT "viewHelp"@, then @TT "~/.Macaulay2/index.html"@ opens,
      which contains a list of all packages installed locally.
  Description
    Text
      Some example uses:
    Code
      TABLE {
	  { M2CODE "viewHelp",                        { " -- open the list of local packages and their documentation" }},
          { M2CODE "viewHelp \"Macaulay2\"",          { " -- open the top of the online documentation tree" }},
          { M2CODE "viewHelp \"matrices\"",           { " -- open an overview of matrices in Macaulay2" }},
	  { M2CODE "viewHelp X",                      { " -- open the documentation node whose key is ", TT "X" }},
          { M2CODE "viewHelp ideal",                  { " -- documentation for the function ", TO ideal, " and ways to use it" }},
          { M2CODE "viewHelp(ideal, List)",           { " -- documentation for the method ", TO (ideal, List) }},
          { M2CODE "viewHelp[gb, DegreeLimit]",       { " -- documentation for the optional argument ", TO [gb, DegreeLimit] }},
          { M2CODE "viewHelp(symbol **, Ring, Ring)", { " -- documentation for the method ", TO (symbol **, Ring, Ring) }},
      }
  Caveat
    The @TO help@ command allows other possible arguments, such as @TT "help methods ideal"@,
    but for @TT "viewHelp"@ the argument must refer to only one web page.
    Note that the Safari browser may prevent loading of the style files for the online documentation.
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
    (infoHelp, Thing)
    (infoHelp, DocumentTag)
  Headline
    view documentation in Info format
  Usage
    infoHelp
    infoHelp X
  Inputs
    X:Thing
      a descriptor for a documentation node (see below for examples)
  Consequences
    Item
      The given documentation page is displayed within a terminal window using the command @TT "info"@.
    Item
      If you are running Macaulay2 in Emacs, then the page is opened in another window using Info mode.
    Item
      If no argument is given to @TT "infoHelp"@, then the top node of the Macaulay2 documentation is displayed.
  Description
    Text
      Some example uses:
    Code
      TABLE {
	  { M2CODE "infoHelp",                        { " -- open the top of the online documentation tree" }},
	  { M2CODE "infoHelp \"Macaulay2\"",          { " -- open the top of the online documentation tree" }},
	  { M2CODE "infoHelp \"matrices\"",           { " -- open an overview of matrices in Macaulay2" }},
	  { M2CODE "infoHelp X",                      { " -- open the documentation node whose key is ", TT "X" }},
	  { M2CODE "infoHelp ideal",                  { " -- documentation for the function ", TO ideal, " and ways to use it" }},
	  { M2CODE "infoHelp(ideal, List)",           { " -- documentation for the method ", TO (ideal, List) }},
	  { M2CODE "infoHelp[gb, DegreeLimit]",       { " -- documentation for the optional argument ", TO [gb, DegreeLimit] }},
	  { M2CODE "infoHelp(symbol **, Ring, Ring)", { " -- documentation for the method ", TO (symbol **, Ring, Ring) }},
      }
    Text
      While in the @TT "info"@ program, there are many ways to navigate and search.
      Besides the arrow keys to move around on the page, here is a list of the most useful key strokes:
    Code
      TABLE {
	  { TT "?", " -- information about all of the possible keystrokes" },
	  { TT "q", " -- quit info, return to Macaulay2" },
	  { TT "n", " -- go to the next documentation node" },
	  { TT "p", " -- go to the previous node" },
	  { TT "m", " -- follow the menu link" },
	  { TT "r", " -- follow a cross-reference" },
	  { TT "l", " -- go to the last node visited" },
      }
  Caveat
    The @TO help@ command allows other possible arguments, such as @TT "help methods ideal"@,
    but @TT "infoHelp"@ requires that the argument refer to only one documentation page.

    @HEADER2 "Viewing Info files in Emacs"@

    If you read the info form of the documentation in Emacs, we recommend configuring
    the value of the Emacs variable @TT "Info-hide-note-references"@ to @TT "hide"@ in order to
    prevent Emacs from inserting a superfluous @TT "See"@ or @TT "see"@ in front of the hyperlinks.
    This is done automatically for you by running @TO setup@ or @TO setupEmacs@.
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
    --(symbol?, Command)
    (symbol?, Keyword)
    (symbol?, Package)
    --(symbol?, Thing)
    (symbol?, Type)
  Headline
    view brief documentation of a symbol
  Description
    Text
      A question mark followed by a symbol representing a function, type, option name, or package
      prints a brief, compact list of various ways to use the given symbol.
    Code
      TABLE {
	  { M2CODE "? BGG",	  { " -- brief information about the package ", TO "BGG::BGG" }},
	  { M2CODE "? Tally",	  { " -- brief information about ways to use objects of class ", TO "Tally" }},
	  { M2CODE "? betti",	  { " -- brief information about ways to use the function ", TO "betti" }},
	  { M2CODE "? Weights",	  { " -- brief information about ways to use the option ", TO "Weights" }},
	  { M2CODE "? symbol >>", { " -- brief information about ways to use the operator ", TO ">>" }},
	  { M2CODE "? HH",	  { " -- brief information about ways to use the scripted functor ", TT "HH" }},
      }
    Text
      If the object is defined by the user, brief information about the object and its class are printed.

      When using Macaulay2 in Emacs, moving the cursor to each line of the output beginning with
      @TT "'*'"@ and pressing Enter results in showing the full documentation node corresponding to that line.
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
      of @TO "about"@. The functions @TO "viewHelp"@ and @TO "infoHelp"@ can also be given an integer for viewing
      the documentation.

      The packages searched are the loaded packages and the packages installed under one of the prefixes listed
      in @TO "prefixPath"@. The first search will take a few seconds while it reads all the documentation keys
      into memory.
    Example
      about firstFunction
      help 1
    Text
      It is also possible to view a table of headlines corresponding to the results.
    Example
      headlines about firstFunction
  Caveat
    Since @TT "s"@ is taken as a regular expression, parentheses serve
    for grouping subexpressions, rather than matching themselves.
  SeeAlso
    (symbol?, Symbol)
    apropos
    headlines
    findSynonyms
    "regular expressions"
    (examples, ZZ)

Node
  Key
     apropos
    (apropos, String)
  Headline
    symbols matching a pattern
  Usage
    apropos pattern
  Inputs
    pattern:String
      a regular expression pattern to match
  Outputs
    :List
      of global symbols matching the given pattern
  Description
    Text
      In the simplest case, the list of symbols containing the given string is returned.
    Example
      apropos "atrix"
    Text
      @TO2 {"regular expressions", "Regular expressions"}@ allow for more complicated requests.
      For example, to find all functions that start with @TT "mat"@ or @TT "Mat"@:
    Example
      apropos "^[mM]at"
    Text
      It is also possible to view a table of headlines corresponding to the results.
    Example
      headlines apropos "hilbert"
  SeeAlso
    help
    about
    headlines
    findSynonyms
    "regular expressions"

Node
  Key
    headlines
   (headlines, List)
   -- TODO: move to separate node
   (help,     ZZ)
   (viewHelp, ZZ)
   (infoHelp, ZZ)
  Headline
    display a table of documentation headlines
  Usage
    headlines about s
    headlines apropos p
    headlines methods f
  Description
    Text
      This method displays a table of documentation headlines for the input list.

      If the input is a list of documentation nodes generated using the function @TO about@,
      the method @TO (help, ZZ)@ can be used to select and open one of the documentation nodes.
    Example
      headlines about firstFunction
      help 0
    Text
      This is also true for a list of symbols generated using the function @TO apropos@.
    Example
      headlines apropos "hilbert"
--      help 0
    Text
      If the input was a list of method keys generated using @TO methods@, then in addition
      the method @TO (code, ZZ)@ can be used to view the source code for the selected entry.
    Example
      headlines methods syz
--      help 1
      code 1
  SeeAlso
    help
    viewHelp
    about
    apropos
    methods
    --(help, ZZ)
    (code, ZZ)
///

-- the node displayed by the help command by default
doc ///
Node
  Key
    "initial help"
  Headline
    Welcome to Macaulay2
  Description
    Text
      Try entering @KBD "2+1.5"@ at your next input prompt, which begins with @KBD "i"@ (e.g. @KBD "i2 : "@). @BR()@
      The two output prompts begin with @KBD "o"@.
    Code
      UL{ LI{"the first one, for instance ", KBD "o2 = ", ", gives the value computed from your input;"},
	  LI{"the second one, for instance ", KBD "o2 : ", ", tells what type of thing the value is."} }
    Text
      Type one of these commands to get started reading the documentation:
    Code
      TABLE {
	  { M2CODE "copyright", 			 "-- the copyright" },
	  { M2CODE "help \"Macaulay2\"", 		 "-- top node of the documentation." },
	  { M2CODE "help \"reading the documentation\"", "" },
	  { M2CODE "help \"getting started\"", 		 "" },
	  { M2CODE "help \"a first Macaulay2 session\"", "" },
	  { M2CODE "help coker", 			 "-- show documentation for coker" },
	  { M2CODE "help about Ext", 			 "-- show documentation about Ext" },
	  { M2CODE "help about(\"Yoneda\", Body=>true)", "-- show documentation mentioning \"Yoneda\"" },
	  { M2CODE "printWidth = 80", 			 "-- set print width to 80 characters" },
	  { M2CODE "viewHelp", 				 "-- view documentation in a browser" },
	  { M2CODE "viewHelp coker", 			 "-- view documentation for coker in browser" },
	  { M2CODE "? hilbertFunction", 		 "-- display brief documentation about Hilbert functions" },
      }
    Text
      To read the documentation in info form, in case you happen to be running Macaulay2 in a
      terminal window, replace @TO "help"@ by @TO "infoHelp"@ in any of the commands above.

      To get BibTeX code for citing Macaulay2 or one of its packages, type one
      of the following commands.
    Code
      TABLE {
	  { M2CODE "cite",                               "-- how to cite Macaulay2" },
	  { M2CODE "cite \"FirstPackage\"",              "-- how to cite a package" }
      }
    Text
      To get information about the startup of Macaulay2, type one of the
      following commands.
    Code
      TABLE {
	  { M2CODE "loadedPackages",                            "-- a list of the currently loaded packages" },
	  { M2CODE "help \"packages provided with Macaulay2\"", "-- a list of all the available packages" },
	  { M2CODE "help \"initialization file\"",              "-- show documentation about the file init.m2" }
      }
///
