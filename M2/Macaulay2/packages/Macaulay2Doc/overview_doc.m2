document {
     Key => "writing documentation",
     HEADER3 "Introduction",
     "Documentation for user defined ", TO "packages", 
     " and Macaulay2 itself is written using 
     the ", TO document, " function, using a straightforward ",
     TO2("hypertext list format", "hypertext"), 
     " markup language. 
     It is then formatted via ", TO "installPackage", 
     " as the documentation built in to Macaulay2, 
     the online HTML documentation, and the info pages.
     Much of the format and structure of the documentation is 
     automatically generated.  Each documentation entry must be 
     part of a package, and occur 
     after the ", TO beginDocumentation, " section of the package.",     
     HEADER3  "Documentation templates",
     "Each documentation entry is either an overview topic, or 
     documentation of an individual feature, such as a symbol, a function
     name, a function call (that is, a function name, together with
     specific types of its arguments), an optional argument to a function, or
     a package",
     PARA{},
     "The easiest way to
     write documentation for an entry is to start with one of the following examples 
     or templates, and then modify it.",
     UL {
	  { TO("package documentation template")
	  },
	  { TO("function name documentation template")
	  },
	  { TO("function documentation template")
	  },
	  { TO("optional argument name documentation template")
	  },
	  { TO("optional argument documentation template")
	  },
	  { TO("overview documentation template")
	  }
     },
     HEADER3 "The documentation writing cycle",
     "Start with the package that you wish to document, and select
     one, or several of the above examples or templates.
     Cycle through the following steps as you refine your documentation.",
     UL {
	  "edit your doc entries as desired",
	  {"generate the html pages for your package, using e.g., ",
	       PRE///installPackage("yourPackage")///,
     	      "A link to your package documentation is placed in the ", 
	      TT "index.html", " file in the directory ",
              TO2(applicationDirectory, TT "applicationDirectory()")},
	  {"view your html using your favorite web browser, or use ",
	       TO viewHelp, " as in e.g., ",
	       PRE///viewHelp "doc entry name"///,
	       "which displays this page in your browser."}
	  },
     HEADER3 "Documentation style conventions",
	"There are a few stylistic conventions that should be noted:",
	UL {
		{"Lowercase is used for all titles, unless a proper noun is being used."},
		{"The name of any Macaulay2 function, option, or variable, occurring in the documentation 
		should be an active hyperlink. This can be accomplished with the tag ", TO "TO", "."},
		{"If one needs to refer to the ", TT "i", "-th coefficient of some object, then use the format as given here."}
		},
  SeeAlso => {
       document,
       "hypertext list format",
       SYNOPSIS
     }
  }
document {
     Key => "hypertext list format",
     "Documentation text is composed of a list of text 
     and hypertext items. A single text string, even though it is not a list, is 
     generally accepted as a hypertext list. Math support is currently
     rudimentary.  Macaulay2 examples may be included in the list",
     PARA{},
     "Each element of the list may be a text string, or one of the elements
     below.",
     "The following items are used by themselves",
     UL {
	  {TT "BR", " -- break to the next line"},
	  {TT "HR", " -- a horizontal rule (line)"},
	  TO "PARA",
	  },
     "Items that take a text string (or other hypertext list) as argument",
     UL {
	  {TT "TT s", " -- makes the argument ", TT "s", " into a 
	                typewriter like, fixed font"},
	  {TT "EM s", " -- change the font of ", TT "s", " to
	                emphasize it"},
	  {TT "PRE s", " -- considers the string ", TT "s", " to be
	                preformatted."},
	  {TT "SUB s", " -- subscript"},
	  {TT "SUP s", " -- superscript"},
	  },
     "Items that place hyperlinks into the documentation",
     UL {
	  TO TO,
	  TO TOH,
	  TO TO2
	  },
     "Other useful hypertext elements",
     UL {
     	  TOH UL,
	  TOH EXAMPLE},
     SUBSECTION "Example",
       "For example, the hypertext list",

       PRE///{ 
  HR{},
  "When referring to a ", 
  EM "Macaulay2", 
  " identifier such as ",
  TT "matrix",   
  ", use the TT element, or use a cross-reference, as in ",
  TO matrix, 
  ".  Incorporate ", 
  EM "Macaulay2", 
  " examples (during ",
  TO (installPackage,String), 
  ") as illustrated here.",
  EXAMPLE "matrix{{1,2},{3,4}}",
  HR{}
 }///,

  PARA{},
  
  "when used in a ", TO "document", " node, produces",PARA{},

{ 
  HR{},
  "When referring to a ", 
  EM "Macaulay2", 
  " identifier such as ",
  TT "matrix",   
  ", use the TT element, or use a cross-reference, as in ",
  TO matrix, 
  ".  Incorporate ", 
  EM "Macaulay2", 
  " examples (during ",
  TO (installPackage,String), 
  ") as illustrated here.",
  EXAMPLE "matrix{{1,2},{3,4}}",
  HR{}
},

SeeAlso => {
     "conventions for documentation",
     "writing documentation"
  }
}  



  
     
document {
     Key => "conventions for documentation",
     "While not hard and fast rules, keeping these stylistic conventions
     in mind makes for easier reading by users",
     UL {
	  { "Start with a documentation template, see ", TO "writing documentation", "." },
	  { TO Inputs, ", ", TO Outputs, ", and ", TO Consequences, " should not end with periods."}
	  }
     }
document {
     Key => "function name documentation template",
 	PRE ///document {
     Key => functionName,
     Headline => "one line description",
     Usage => "usage",
     Inputs => {
	  -- each input is a hypertext list
	  },
     Outputs => {
	  -- each output is a hypertext list
	  },
     Consequences => {
          -- each effect is a hypertext list
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }
document {
     Key => "function documentation template",
 	PRE ///document {
     Key => (functionName, argumentClass1, argumentClass2, ...),
     Headline => "one line description", -- only if different functionName Headline
     Usage => "usage",
     Inputs => {
	  -- each input is a hypertext list
	  },
     Outputs => {
	  -- each output is a hypertext list
	  },
     Consequences => {
          -- each effect is a hypertext list
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }
document {
     Key => "optional argument name documentation template",
 	PRE ///document {
     Key => optionName,
     Headline => "one line description",
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }
document {
     Key => "optional argument documentation template",
 	PRE ///document {
     Key => [functionName, optionName],
     Headline => "one line description",
     Usage => "usage",
     Inputs => {
	  -- each input is a hypertext list
	  },
     Consequences => {
          -- each effect is a hypertext list
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }
document {
     Key => "overview documentation template",
 	PRE ///document {
     Key => "description of this page",
     Headline => "one line description",
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }
document {
     Key => "package documentation template",
 	PRE ///document {
     Key => PACKAGENAME,
     Headline => "one line description",
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
	  "m2code",
	  "m2code",
	  "m2code"
	  },
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"}
     Subnodes => {
	  TO functionName1,
	  TO functionName2 -- and so on
	  }
     }///,
     SeeAlso => {"writing documentation",
	  "hypertext list format",
	  document}
     }

undocumented {(SYNOPSIS, Sequence),(SYNOPSIS, Thing),(SYNOPSIS, List)}

document {
     Key => [SYNOPSIS, Heading],
     "Specifies a subheading for the synopsis.  The default is simply \"Synopsis\"."
     }

document { Key => {SYNOPSIS,[SYNOPSIS, Usage],[SYNOPSIS, Outputs],[SYNOPSIS, Inputs],[SYNOPSIS, Consequences],
	  [SYNOPSIS, BaseFunction]},
     PARA {
	  "This function prepares a standardized synopsis in hypertext for use in documentation nodes."
	  },
     "Here is an empty template for use with SYNOPSIS.",
     PRE "     SYNOPSIS (
	  Heading => \"\",
	  Usage => \"\"
	  BaseFunction => fn,
	  Inputs => {
	       },
	  Consequences => {
	       }
	  Outputs => {
	       },
	  PARA {
	       },
	  EXAMPLE lines ///
	  ///
	  ),
",
     PARA {
	  "The options are used just as with ", TO "document", "."
	  },
     "Here is an example of its use.",
     PRE "     SYNOPSIS {
	  Heading => \"using binary methods for method functions\",
	  Usage => \"f(x,y)\",
	  Inputs => {
	       \"f\" => { \"a method function\" },
	       \"x\" => { \"an object of type \", TT \"X\" },
	       \"y\" => { \"an object of type \", TT \"Y\" }
	       },
	  Outputs => {
	       { \"the previously installed method for \", TT \"f(X,Y)\", \" is called with arguments \", TT \"(x,y)\", \", and the return value is returned.
  		    If no such method has been installed, then Macaulay2 searches for a method
		    for \", TT \"f(X',Y')\", \", where \", TT \"X'\", \" is an ancestor of \", TT \"X\", \" and \", TT \"Y'\", \" is an ancestor of \", TT \"Y\", \"
		    (see \", TO \"inheritance\", \" for details).\"
		    }
	       },
	  \"The second line of the following example illustrates the syntax above, using \", TO \"source\", \", which happens to be a method function.\",
	  EXAMPLE lines ///
	       source(String,String) := peek;
	       source(\"foo\",\"bar\")
	  ///,
	  PARA \"The same syntax works for 3 or 4 arguments.\"
	  }
",
     }
