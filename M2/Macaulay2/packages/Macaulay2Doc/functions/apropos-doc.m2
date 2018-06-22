--- status: DONE
--- author(s): M. Stillman
--- notes: 

document { 
     Key => {(apropos, String),apropos},
     Headline => "symbols matching a pattern",
     Usage => "apropos pat",
     Inputs => {
	  "pat" => String => "a regular expression pattern to match"
	  },
     Outputs => {
	  List => "of global symbols in Macaulay2 matching the given pattern"
	  },
     "In the simplest case, the list of symbols containing the given string is returned.",
     EXAMPLE {
	  "apropos \"atrix\""
	  },
     TO2("regular expressions", "Regular expressions"), " allow for more complicated requests.  For example, to
     find all functions that start with ", TT "mat", " or ", TT "Mat", ":",
     EXAMPLE {
	  ///apropos "^[mM]at"///
	  },
     SeeAlso => {findSynonyms, help, about, examples, viewHelp, infoHelp, "reading the documentation", "regular expressions"}
     }

