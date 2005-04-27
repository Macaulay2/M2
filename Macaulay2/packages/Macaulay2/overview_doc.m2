document {
     Key => "writing a documentation entry",
     "A package should provide documentation for each of its 
     exported symbols and functions.  The easiest way to
     write documentation for an entry is to use one of the following examples 
     or templates, and then modify it.",
     UL {
	  { "package header ", TO2("package documentation template", "template"),", ", 
	       TO2("package documentation example", "example") 
	  },
	  { "function name ", TO2("function name documentation template", "template"),", ", 
	       TO2("function name documentation example", "	     example") 
	  },
	  { "function call (includes classes of function arguments) ", TO2("function documentation template", "template"),", ", 
	       TO2("function documentation example", "example") 
	  },
	  { "optional argument name ", TO2("optional argument name documentation template", "template"),", ", 
	       TO2("optional argument name documentation example", "example") 
	  },
	  { "optional argument ", TO2("optional argument documentation template", "template"),", ", 
	       TO2("optional argument documentation example", "example") 
	  },
	  { "overview ", TO2("overview documentation template", "template"),", ", 
	       TO2("overview documentation example", "example") 
	  }
     },
     PARA,
     "To have Macaulay 2 generate the html pages for the documentation for your package, use ", TO installPackage , ":",
     PRE///installPackage "yourPackage"///,
     "A link to your package documentation is placed in the ", TT "index.html", " file in the directory ",
     TO2(applicationDirectory, TT "applicationDirectory()"), "."
   }
document {
     Key => "writing documentation",
	"Documentation for Macaulay 2 is written in a hypertext markup language. It is 
	then formatted as the documentation built in to Macaulay 2, the online 
	", TO "HTML", " documentation, and the info pages. Much of the
	format and structure of the documentation is automatically generated.",
	PARA,
        "There are two main types of documentation in Macaulay 2: basic
	documentation of individual features, and overviews.  Essentially
	they are documented the same way. However, there are some differences,
	see ", TO "document", " for details.",
	PARA,
	"There are a few stylistic conventions which should be noted:",
	UL {
		{"Lowercase is used for all titles, unless a proper noun is being used."},
		{"The name of any Macaulay 2 function, option, or variable, occurring in the documentation 
		should be an active hyperlink. This can be accomplished with the tag", TO "TO", "."},
		{"If one needs to refer to the ", TT "i", "-th coefficient of some object, then use the format as given here."}
		},
  SeeAlso => document
     }
document {
     Key => "hypertext list format",
     "Documentation text is composed of a list of text 
     and hypertext items. A single text string, even though it is not a list, is 
     generally accepted as a hypertext list. Math support is currently
     rudimentary.  Macaulay 2 examples may be included in the list",
     PARA,
     "Each element of the list may be a text string, or one of the elements
     below.",
     "The following items are used by themselves",
     UL {
	  {TT "BR", " -- break to the next line"},
	  {TT "HR", " -- a horizontal rule (line)"},
	  {TT "PARA", " -- start a paragraph"},
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
	  TOH TO,
	  TOH TOH,
	  TOH TO2
	  },
     "Other useful hypertext elements",
     UL {
     	  TOH UL,
	  TOH EXAMPLE},
     SUBSECTION "Example",
       "For example, the hypertext list",

       PRE///{ 
  HR,
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
  HR  
 }///,

  PARA,
  
  "when used in a ", TO "document", " node, produces",PARA,

{ 
  HR,
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
  HR  
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
     Key => "hypertext example",
     "The following items are used by themselves",
     UL {
	  {TT "BR", " -- break to the next line"},
	  {TT "HR", " -- a horizontal rule (line)"},
	  {TT "PARA", " -- start a paragraph"},
	  },
     "Items which take a phrase as argument",
     UL {
	  {TT "TT s", " -- makes the argument ", TT "s", " into a 
	                typewriter like, fixed font"},
	  {TT "EM s", " -- change the font of ", TT "s", " to
	                emphasize it"},
	  {TT "PRE s", " -- considers the string ", TT "s", " to be
	                preformatted."},
	  {TT "SUB s", " -- subscript"},
	  {TT "SUP s", " -- superscript"}
	  },
     "Items which place hyperlinks into the documentation",
     UL {
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
     Caveat => {"warning"},
     SeeAlso => {link}
     }///,
     SeeAlso => {"writing a documentation entry",
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
     Caveat => {"warning"},
     SeeAlso => {link}
     }///,
     SeeAlso => {"writing a documentation entry",
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
     Caveat => {"warning"},
     SeeAlso => {link}
     }///,
     SeeAlso => {"writing a documentation entry",
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
     Caveat => {"warning"},
     SeeAlso => {link}
     }///,
     SeeAlso => {"writing a documentation entry",
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
     Caveat => {"warning"},
     SeeAlso => {link}
     }///,
     SeeAlso => {"writing a documentation entry",
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
     Caveat => {"warning"},
     SeeAlso => {link},
     Subnodes => {
	  TO functionName1,
	  TO functionName2 -- and so on
	  }
     }///,
     SeeAlso => {"writing a documentation entry",
	  "hypertext list format",
	  document}
     }
