--		Copyright 1993-1999 by Daniel R. Grayson

-- throughout the documentation of the documentation
-- examples are referred to in the code. 
-- Each time such a reference was made, a comment
-- was added to the referenced code to ensure that 
-- if changes are made there, the author will know to
-- change the documentation of the documentation to meet
-- the changes.

document {  
     Key => {document,(document, List),ExampleFiles,[document,ExampleFiles],[document,SourceCode]},
     Usage => "document { Key => key, ... }",
     Headline => "package item: documentation node",
     Inputs => {
	  List => {"a hypertext list including special documentation entries; any optional arguments are to be placed in this list"},
	  SourceCode => List => "a list of functions whose source code should be displayed in the documentation"
	  },
     Consequences => {"formatted documentation is created and stored"},
     PARA {
     	  "There are two basic types of documentation.  The first type documents a function or a method, as in ", TO (resolution,Module), ". The second type of 
	  documentation is for overviews or functions with methods, as in ", TO "chain complexes", " or ", TO resolution, " respectively.
	  The headings ", TO "Usage", ", ", TO "Function", ", ", TO "Inputs", ", ", TO "Outputs", ", and ", TO "Consequences", ", are 
	  useful only for documentation of the first type."
	  },
     PARA {
	  "Here is a template for a typical documentation node."
	  },
     PRE ///document {
     Key => key,
     Headline => "one line description", -- not needed for overviews
     Usage => "usage",
     BaseFunction => "function", -- usually not needed
     SourceCode => {METHOD1,...}, -- usually not needed
     Inputs => {
          inputs
	  },
     Consequences => {
	  effects
	  },
     Outputs => {
          outputs
	  },
     "A list of strings containings names of files in the auxiliary 
     directory of the package can go here; the files will be visible
     in the current directory while example code is run.",
     ExampleFiles => {"datafile1", "datafile2"},
     "There can be explanatory prose here in the form of a hypertext list.",
     EXAMPLE lines \/\/\/
		m2code
		m2code
		m2code
	  \/\/\/,
     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"warning"},
     SeeAlso => {"other things"},
     Subnodes => {
	  "subheading a",
	  TO "node 1", 
	  TO "node 2",
	  "subheading b",
	  TO "node 3", 
     	  ...
	  },
     }///,
     PARA "Hypertext markup items that you may use:",
     UL {
	  -- Mike wanted this: TOH "hypertext format",
	  TOH EXAMPLE,
	  },
     SeeAlso => {"writing documentation", "conventions for documentation", hypertext}
     }

document {
	Key => {[document,Key],Key},
	Headline => "key of a documentation node",
	Usage => "document { Key => key, ... }",
	Consequences => {"specifies the name under which the documentation is stored"},
	"The document key is the the name of the node, specifically the one that allows 
	users to find the documentation for the desired command. Some node names, rather than being strings, 
	are sequences of Macaulay2 objects that refer to specific method functions. If one is documenting 
	a function of Macaulay2, the key  will just be the function's name, as in the
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
	Caveat => {"No two documentation nodes may have the same key or even a key that is a synonym for a key 
	     which is already used. However, sometimes there is a need for several documentation nodes to have the same  
		key. This happens when a command that needs to be documented is a synonym for another already documented command. In this case the 
		synonym's key should be stated in quotation marks. As an example, look at the documentation for ", TO "SUBSECTION", " and ", TO "HEADER2", "."},
	SeeAlso => {document,TO}
	}


document { -- This node is used as an example in the node: Consequences 
     Key => {Headline,[document, Headline]},
     Headline => "make a headline for a documentation node",
     Usage => "document { Key => key,
     Headline => \"one line description\", ... }",
     BaseFunction => document,
     Consequences => {
	  { "the headline string will be used to annotate itemized 
	      lists of cross-references to the documentation ", TO "Key" }
	  },
	"The headline of a documentation node, gives a brief, half line, description of the thing being documented. 
	As an example, the headline for this documentation node was obtained with the code:",
	PRE ///Headline => "make a headline for a documentation node", ///,
     SeeAlso => {document, hypertext, TOH}
     }
document {
     Key => {Usage,[document, Usage]}, 
	Headline => "shows the usage of a function",
	Usage => "document { ... , Usage => \"usage\", ... }",
     BaseFunction => document,
	"The ", TT "Usage", " entry should give a formal example showing the usage of the function. 
	The variables used in this formal example should be the ones used in the ", TO "Inputs", " and ", TO "Outputs",
	" sections of the documentation.  Here is the code for the ", TT "Usage", " entry of the method ", TO (matrix, List), ":",
	PRE ///Usage => "matrix v"///,
	"Here is the code for the ", TT "Usage", " entry of the method ", TO (resolution, Module), ":",
	PRE ///Usage => "resolution M\nres M"///,
	PARA {
	     "This option also can be used within a ", TO "SYNOPSIS", " section.",
	     },
	SeeAlso => {document, Inputs, Outputs} 
	}

document {
	Key => {[document,BaseFunction],BaseFunction},
	Headline => "function for a documented feature",
	"The ", TT "BaseFunction", " entry gives the function that uses the feature being documented."
	}

document {
     Key => {[document,Inputs],Inputs}, 
     Headline => "inputs for a function",
     Usage => "document { ... , Inputs => { inputs, ... }, ... }",
     "The entries should consist of items in one of the following forms.",
	  UL {
	      TT "hypertext",
	      TT "class",
	      TT "symbolname",
	      TT "class => hypertext",
	      TT "symbolname => class",
	      TT "symbolname => hypertext",
	      TT "symbolname => class => hypertext",
	  },
	"As an example, here is the ", TT "Inputs", " entry of the method ", TO (resolution,Ideal), ":",
	PRE ///Inputs => { "I" => { "an ideal in a ring ", TT "R", ", say" } },///,
	"Here is an example of the ", TT "Inputs", " entry of the function ", TO sin, ":",
	PRE ///Inputs => { "x" => RR },///,
	SeeAlso => {document, Outputs, Usage}
	}
document {
     Key => {[document,Outputs],Outputs},
	Headline => "outputs for a function",
	Usage => "document { ... , Outputs => { outputs, ... }, ... }",
	BaseFunction => document,
	"The entries should consist of items in one of the following forms.",
	  UL {
	      TT "hypertext",
	      TT "class => hypertext (or null)",
	      TT "symbolname => hypertext (or null)",
	      TT "symbolname => class => hypertext (or null)",
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
     Key => {[document,Consequences],Consequences}, 
	Headline => "side-effects of a function",
	Usage => "document { ... , Consequences => {\"effects\" }, ... }",
	BaseFunction => document,
    	"Here is where one documents effects of a function that are not return values. As an 
	example here is the ", TT "Consequences", " entry for the documentation node ", TO Headline, ":", 
	PRE ///Consequences => {
	  { "the headline string will be used to annotate itemized 
	      lists of crossreferences to the documentation ", TO "Key" }
	  },///,
	SeeAlso => {document}
	}
document {
     Key => {[document,Acknowledgement],Acknowledgement},
	Headline => "acknowledgements",
	Usage => "document { ... , Acknowledgements => {\"NSF\"}, ... }",
	BaseFunction => document,
	"This part of the documentation can be used to acknowledge funding sources
	and collaborators.",
	SeeAlso => {document}
	}
document {
     Key => {[document,Contributors],Contributors},
	Headline => "non-author contributors",
	Usage => "document { ... , Contributors => {\"previous authors\"}, ... }",
	BaseFunction => document,
	"This part of the documentation can be used to list contributors and
	previous authors to the package who are no longer maintainers for it.",
	SeeAlso => {document}
	}
document {
     Key => {[document,References],References},
	Headline => "references",
	Usage => "document { ... , References => {\"bibliography\"}, ... }",
	BaseFunction => document,
	"This part of the documentation can be used to list references for the package.",
	SeeAlso => {document}
	}
document {
     Key => {[document,Caveat],Caveat}, 
	Headline => "warnings",
	Usage => "document { ... , Caveat => {\"warning\"}, ... }",
	BaseFunction => document,
    	"This part of the documentation serves to highlight pitfalls for the user.",
	SeeAlso => {document}
	}
document {
     Key => {[document,SeeAlso],SeeAlso},
     Headline => "cross-references in documentation",
     Usage =>  "document { ... , SeeAlso => { ... }, ... }",
     BaseFunction => document,
     "This option inserts into a documentation page a sentence instructing the reader to see some other topics.",
     PARA{},
     "The entries may have the special forms used with ", TO "TO", ". As an example, here is the code for the ", TT "SeeAlso", " part of this documentation node.",
     PRE ///SeeAlso => {document, TO},///,
     SeeAlso => {document, TO}
     }
document {
     Key => {[document,Subnodes],Subnodes},
     Headline => "a menu of documentation nodes",
     Usage =>  "document { ... , Subnodes => { ... }, ... }",
     BaseFunction => document,
     "This option inserts into a documentation page a menu of subnodes. Here is a generic example:",
     PRE ///Subnodes => {
	  "An example of subnodes",
	  TO "a link name",
	  TO "another link name"
	  },///,
     "The ", TT "Subnodes", " option defines how the tree structure of the documentation is constructed. This is relevant when printing the documentation.",   
     SeeAlso => {document, TO, "SimpleDoc::doc"}
     }

