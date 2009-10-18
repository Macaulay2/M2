--- status: Draft
--- author(s): 
--- notes: 

document {
     Key => {subquotient,(subquotient,Matrix,Matrix),
	  (subquotient,Nothing,Matrix),(subquotient,Matrix,Nothing),
	  (subquotient,Module,Matrix,Matrix),
	  (subquotient,Module,Matrix,Nothing),
	  (subquotient,Module,Nothing,Matrix),
	  (subquotient,Module,Nothing,Nothing)},
     Headline => "make a subquotient module",
     Usage => "subquotient(g,r)\nsubquotient(M,g,r)",
     Inputs => {
	  "g" => "the matrix of generators",
	  "r" => {"the matrix of relations, with the same target as ", TT "g", ""},
	  "M" => Module => {"if given, must be the common target of g and r"}
	  },
     Outputs => {
	  {"the image of ", TT "g", " in the cokernel of ", TT "r"}
	  },
     PARA {
	  "If ", TT "g", " is omitted, then the identity map on the target of ", 
	  TT "r", " is used as ", TT "g", ", and the cokernel of ", TT "r", " is returned.
	  If ", TT "r", " is omitted (but not the comma), then a zero map to the target of ", 
	  TT "g", " is used as ", TT "r", ", and the image of ", TT "g", " is returned.
	  If both are omitted, then ", TT "M", " is returned."
	  },
     PARA{
	  "See ", TO "subquotient modules", " for an overview of subquotient modules in Macaulay2."
	  },
     PARA {
     	  "The general form in which modules are represented in Macaulay2 is as subquotients,
	  and subquotient modules are often returned as values of computations, as in the example below."
	  },
     EXAMPLE lines ///
	  R = ZZ/101[a..d]
      	  M = kernel vars R ++ cokernel vars R
      	  generators M
      	  relations M
	  M === subquotient(generators M, relations M)
      	  prune M,
	  ///,
     SeeAlso => {"subquotient modules", isSubquotient, ambient, "generators", "relations", "prune"}
     }

document {
     Key => {(isSubquotient,Module,Module), isSubquotient},
     Headline => "check whether a module is a subquotient of another",
     Usage => "isSubquotient(M,N)",
     Inputs => {
	  "M",
	  "N" 
	  },
     Outputs => {
	  Boolean => "returns true if M is a subquotient module of N"
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  N = coker matrix{{a,b},{c,d}}
	  N1 = N/(a^4*N)
     	  M = a*N/(R*a*N_0+a*b*N)
	  isSubquotient(M,N)
     	  isSubquotient(M,N1)	  
         ///
     }
