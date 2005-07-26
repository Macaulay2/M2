--		Copyright 1993-1999 by Daniel R. Grayson

-- throughout the documentation of the documentation
-- examples are referred to in the code. 
-- Each time such a reference was made, a comment
-- was added to the referenced code to insure that 
-- if changes are made there, the author will know to
-- change the documentation of the documentation to meet
-- the changes.

document {  
     Key => document,
     Usage => "document { Key => key, ... }",
     Headline => "install documentation",
	Inputs => {List => {"a hyptertext list including special documentation entries"}},
	Consequences => {"formatted documentation is created"},
	    "There are two basic types of documentation. The first type documents 
	    a function or a method, as in ", TO (resolution,Module), ". The second type of 
	    documentation is for overviews or functions with methods, as in ", 
	    TO "chain complexes", 
	    " or ", TO resolution, " respectively. The headings ", TO "Usage", ", ", TO2 {"Function ", "Function"}, 
	    ", ", TO "Inputs", ", ", TO "Outputs", ", and ", TO "Consequences", ", are useful only for documentation of the first type. 
	    Here is a template for a typical documentation node.",
 	PRE ///document {
     Key => key,
     Headline => "one line description", -- not needed for overviews
     Usage => "usage",
     Function => "function",
     Inputs => {
		inputs
	  },
     Outputs => {
		outputs
	  },
     Consequences => {
		effects
	  },
  	"There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE {
		"m2code",
		"m2code",
		"m2code"
		 },
	"There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"},
     SeeAlso => {linkFile},
     Subnodes => {hypertextlist},
     }///,
     PARA "Special documentation entries:",
     UL {
	  TOH Key,
	  TOH Headline,
	  TOH Usage,
	  {TO2 {"Function ", "Function"}, " -- function for a documented feature"},
	  TOH Inputs,
	  TOH Outputs,
	  TOH Consequences,
    	  TOH Caveat,
	  TOH EXAMPLE,
	  TOH SeeAlso,
          TOH Subnodes
	  },
     PARA "Hypertext markup items that you may use",
     UL {
	  TOH "hypertext format"
	  },
     SeeAlso => {"writing documentation", 
	  "conventions for documentation",
	  hypertext}
  }
document {
	Key => Key,
	Headline => "key of a documentation node",
	Usage => "document { Key => key, ... }",
	Function => document, 
	Consequences => {"specifies the name under which the documentation is stored"},
	"The document key is the the name of the node, specifically the one that allows 
	users to find the documentation for the desired command. Some node names, rather than being strings, 
	are sequences of Macaulay 2 objects that refer to specific method functions. If one is documenting 
	a function of Macaulay 2, the key  will just be the function's name, as in the
	documentation for ", TO "resolution", ".",
	PRE ///Key => resolution,///,
	"However, if one is documenting a method for a  function, then the key will have a different form, as in the 
	documentation for ", TO {(resolution,Module)}, ".",
	PRE ///Key => (resolution,Module),///,
	"If one is documenting an optional argument to a function, then the key has a different form still, as in ",
	TO {[resolution,SyzygyLimit]}, ".",
	PRE ///Key => [resolution,SyzygyLimit],///,
	"Finally, if one is documenting an overview of a group of functions or a package, as in ", 
	TO "chain complexes", 
	" then one would set up a document key of the form:",
    	PRE ///Key => "chain complexes",///,
	Caveat => {"No two documentation nodes may have the same key or even a key which is a synonym for a key 
	     which is already used. However, sometimes there is a need for several documentation nodes to have the same  
		key. This happens when a command that needs to be documented is a synonym for another already documented command. In this case the 
		synonym's key should be stated in quotation marks. As an example, look at the documentation for ", TO "SUBSECTION", " and ", TO "HEADER2", "."},
	SeeAlso => {document,TO}
	}


document { -- This node is used as an example in the node: Consequences 
     Key => Headline,
     Headline => "make a headline for a documentation node",
     Usage => "document { Key => key, Headline => \"one line description\", ... }",
     Function => document,
     Consequences => {
	  { "the headline string will be used to annotate itemized 
		  lists of cross references to the documentation ", TO "Key" }
	  },
	"The headline of a documentation node, gives a brief, half line, description of the thing being documented. 
	As an example, the headline for this documentation node was obtained with the code:",
	PRE ///Headline => "make a headline for a documentation node", ///,
     SeeAlso => {document, hypertext, TOH}
     }
document {
     Key => Usage, 
	Headline => "shows the usage of a function",
	Usage => "document { ... , Usage => \"usage\", ... }",
     Function => document,
	"The ", TT "Usage", " entry should give a formal example showing the usage of the function. 
	The variables used in this formal example should be the ones used in the ", TO "Inputs", " and ", TO "Outputs",
	" sections of the documentation. Here is the code for the ", TT "Usage", " entry of the method ", TO (matrix, List), ":",
	PRE ///Usage => "matrix v",///,
	"Here is the code for the ", TT "Usage", " entry of the method ", TO (resolution, Module), ":",
	PRE ///Usage => {TT "resolution M", " or ", TT "res M"},///,
	SeeAlso => {document, Inputs, Outputs} 
	}
document {
	Key => "Function ",
	Headline => "function for a documented feature",
	"The ", TT "Function", " entry gives the function that uses the feature being documented. 
	Using this node as an example we see that the function is ", TO "document", 
	". Indeed, outside of the function ", TO "document", ", ", TT "Function", " has a completely different usage.", 
	}
document {
     Key => Inputs, 
	Headline => "inputs for a function",
	Usage => "document { ... , Inputs => { inputs, ... }, ... }",
	Function => document,
	"The entries should consist of items in one of the following forms.",
	  UL {
	  TT "hypertextlist",
	  TT "class => hypertextlist",
	  TT "string => null",
	  TT "string => hypertextlist",
	  TT "string => class => hypertextlist",
	  TT "symbol => null",
	  TT "symbol => hypertextlist",
	  TT "symbol => class => hypertextlist"
	  },
	"As an example, here is the ", TT "Inputs", " entry of the method ", TO (resolution,Ideal), ":",
	PRE ///Inputs => { "I" => { "an ideal in a ring ", TT "R", ", say" } },///,
	"Here is an example of the ", TT "Inputs", " entry of the function ", TO sin, ":",
	PRE ///Inputs => { "x" => RR => null },///,
	SeeAlso => {document, Outputs, Usage}
	}
document {
     Key => Outputs,
	Headline => "outputs for a function",
	Usage => "document { ... , Outputs => { outputs, ... }, ... }",
	Function => document,
	"The entries should consist of items in one of the following forms.",
	  UL {
	  TT "hypertextlist",
	  TT "class => hypertextlist",
	  TT "string => null",
	  TT "string => hypertextlist",
	  TT "string => class => hypertextlist",
	  TT "symbol => null",
	  TT "symbol => hypertextlist",
	  TT "symbol => class => hypertextlist"
	  },
     "As an example, here is the ", TT "Outputs", " entry of the method ", TO (resolution,Ideal), ":",
	 PRE /// Outputs => { {"a resolution of ", TT "R/I", " by projective ", TT "R", "-modules" } },///,
	"Note that the hypertext list needs to be bounded by ", TT "{", " and ", TT "}", " as there is only one output for ", 
	TO (resolution, Ideal), ". Without the braces, multiple outputs are defined. Note also that
	the ", TO Type, " of the output is automatically added in this case.",
	"Here is an example of the ", TT "Outputs", " entry of the function ", TO sin, ":",
	PRE ///Outputs => { { "the sine of ", TT "x", "" } },///,
    	SeeAlso => {document, Inputs, Usage}
	}
document {
     Key => Consequences, 
	Headline => "side-effects of a function",
	Usage => "document { ... , Consequences => {\"effects\" }, ... }",
	Function => document,
    	"Here is where one documents effects of a function which are not return values. As an 
	example here is the ", TT "Consequences", " entry for the documentation node ", TO Headline, ":", 
	PRE ///Consequences => {
	  { "the headline string will be used to annotate itemized 
		  lists of cross references to the documentation ", TO "Key" }
	  },///,
	SeeAlso => {document}
	}
document {
     Key => Caveat, 
	Headline => "warnings",
	Usage => "document { ... , Caveat => {\"warning\"}, ... }",
	Function => document,
    	"This part of the documentation serves to highlight pitfalls for the user.",
	SeeAlso => {document}
	}
document {
     Key => SeeAlso,
     Headline => "crossreferences in documentation",
     Usage =>  "document { ... , SeeAlso => { ... }, ... }",
	Function => document,
    	"This option inserts into a documentation page a sentence
     instructing the reader to see some other topics.",
     PARA,
     "The entries may have the special forms used with ", TO "TO", ". As an example, here is the code for the ", TT "SeeAlso", 
	" part of this documentation node.",
	PRE ///SeeAlso => {document, TO},///,
     SeeAlso => {document, TO}
     }
document {
     Key => Subnodes,
     Headline => "a menu of documentation nodes",
     Usage =>  "document { ... , Subnodes => { hypertextlist }, ... }",
	Function => document,
   	"This option inserts into a documentation page a menu of subnodes. Here is a generic example:",
	PRE ///Subnodes => {
		"An example of subnodes",
		TO "a link name",
		TO "another link name"
		},///,
	"The ", TT "Subnodes", " option defines how the tree structure of the documentation is constructed. This is relevant when printing
	the documentation.",   
    	SeeAlso => {document, TO},
     }

document {
     Key => PARA,
     Headline => "hypertext paragraph separator",
     Usage => {"PARA", EM ", or ", "PARA x"},
     Inputs => {
	  "x" => String => {", a ", TO2("hypertext list format", "hypertext list")}
	  },
     Consequences => {
       {" makes a ", TO "hypertext", " double-spaced paragraph break. If no 
	    string is given, then a new paragraph is started" }
     },
     "For an example, see ", TO "hypertext list format", "."
     }
