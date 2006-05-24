document {
     Key => "writing documentation",
     HEADER3 "Introduction",
     "Documentation for user defined ", TO "packages", 
     " and Macaulay 2 itself is written using 
     the ", TO document, " function, using a straightforward ",
     TO2("hypertext list format", "hypertext"), 
     " markup language. 
     It is then formatted via ", TO "installPackage", 
     " as the documentation built in to Macaulay 2, 
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
	  { "package header ", 
	       TO2("package documentation template", "template"),", ", 
	       TO2("package documentation example", "example") 
	  },
	  { "function name ", 
	       TO2("function name documentation template", "template"),", ", 
	       TO2("function name documentation example", "example") 
	  },
	  { "function call (includes classes of function arguments) ", 
	       TO2("function documentation template", "template"),", ", 
	       TO2("function documentation example", "example") 
	  },
	  { "optional argument name ", 
	       TO2("optional argument name documentation template", "template"),", ", 
	       TO2("optional argument name documentation example", "example") 
	  },
	  { "optional argument ", 
	       TO2("optional argument documentation template", "template"),", ", 
	       TO2("optional argument documentation example", "example") 
	  },
	  { "overview ", TO2("overview documentation template", "template"),", ", 
	       TO2("overview documentation example", "example") 
	  }
     },
     HEADER3 "The documentation writing cycle",
     "Start with the package that you wish to document, and select
     one, or several of the above examples or templates.
     Cycle through the following steps as you refine your documentation.",
     UL {
	  "edit your doc entries as desired",
	  {"generate the html pages for your package, using e.g. ",
	       PRE///installPackage("yourPackage")///,
     	      "A link to your package documentation is placed in the ", 
	      TT "index.html", " file in the directory ",
              TO2(applicationDirectory, TT "applicationDirectory()")},
	  {"view your html using your favorite web browser, or use ",
	       TO viewHelp, " as in e.g. ",
	       PRE///viewHelp "doc entry name"///,
	       "which displays this page in your browser."}
	  },
     HEADER3 "Documentation style conventions",
	"There are a few stylistic conventions which should be noted:",
	UL {
		{"Lowercase is used for all titles, unless a proper noun is being used."},
		{"The name of any Macaulay 2 function, option, or variable, occurring in the documentation 
		should be an active hyperlink. This can be accomplished with the tag ", TO "TO", "."},
		{"If one needs to refer to the ", TT "i", "-th coefficient of some object, then use the format as given here."}
		},
  SeeAlso => {
       document,
       "hypertext list format"
     }
  }
document {
     Key => "hypertext list format",
     "Documentation text is composed of a list of text 
     and hypertext items. A single text string, even though it is not a list, is 
     generally accepted as a hypertext list. Math support is currently
     rudimentary.  Macaulay 2 examples may be included in the list",
     PARA{},
     "Each element of the list may be a text string, or one of the elements
     below.",
     "The following items are used by themselves",
     UL {
	  {TT "BR", " -- break to the next line"},
	  {TT "HR", " -- a horizontal rule (line)"},
	  TO "PARA",
	  },
     "Items which take a text string (or other hypertext list) as argument",
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
     "Items which place hyperlinks into the documentation",
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
  hr,
  "When referring to a ", 
  EM "Macaulay 2", 
  " identifier such as ",
  TT "matrix",   
  ", use the TT element, or use a cross-reference, as in ",
  TO matrix, 
  ".  Incorporate ", 
  EM "Macaulay 2", 
  " examples (during ",
  TO (installPackage,String), 
  ") as illustrated here.",
  EXAMPLE "matrix{{1,2},{3,4}}",
  hr
 }///,

  PARA{},
  
  "when used in a ", TO "document", " node, produces",PARA{},

{ 
  hr,
  "When referring to a ", 
  EM "Macaulay 2", 
  " identifier such as ",
  TT "matrix",   
  ", use the TT element, or use a cross-reference, as in ",
  TO matrix, 
  ".  Incorporate ", 
  EM "Macaulay 2", 
  " examples (during ",
  TO (installPackage,String), 
  ") as illustrated here.",
  EXAMPLE "matrix{{1,2},{3,4}}",
  hr
},

SeeAlso => {
     "conventions for documentation",
     "writing documentation"
  }
}  



  
     
document {
     Key => "conventions for documentation",
     "While not hardfast rules, keeping these stylistic conventions
     in mind makes for easier reading by users",
     UL {
	  { "Start with a ", TO "documentation template" },
	  { TO Inputs, ", ", TO Outputs, ", and ", TO Consequences, 
	       "should not end with periods."}
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
