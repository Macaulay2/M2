--- status: TODO
--- author(s): 
--- notes: 

undocumented {(subquotient,Nothing,Matrix),(subquotient,Matrix,Nothing)}
document {
     Key => {subquotient,(subquotient,Matrix,Matrix)},
     Headline => "make a subquotient module",
     Usage => "subquotient(g,r)",
     Inputs => {
	  "g" => "the matrix of generators",
	  "r" => {"the matrix of relations, with the same target as ", TT "g", ""}
	  },
     Outputs => {
	  {"the image of ", TT "g", " in the cokernel of ", TT "r"}
	  },
     PARA {
	  "If ", TT "g", " is omitted, then the identity map on the target of ", TT "r", " is used as ", TT "g", ", and the cokernel of ", TT "r", " is returned.
	  If ", TT "r", " is omitted (but not the comma), then a zero map to the target of ", TT "g", " is used as ", TT "r", ", and the image of ", TT "g", " is returned."
	  },
     PARA {
     	  "The general form in which modules are represented in Macaulay 2 is as subquotients,
	  and subquotient modules are often returned as values of computations, as in the example below."
	  },
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "M = kernel vars R ++ cokernel vars R",
      	  "generators M",
      	  "relations M",
	  "M === subquotient(generators M, relations M)",
      	  "prune M",
	  },
     SeeAlso => {"generators", "relations", "prune"}
     }

 -- doc7.m2:1317:     Key => (subquotient,Matrix,Matrix),
 -- overviewC.m2:1238:     Key => "subquotient modules",
 -- overviewC.m2:1328:     Key => "what is a subquotient module?",
 -- overviewC.m2:1341:     Key => "extracting parts of a subquotient module",
