--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {genericSkewMatrix, (genericSkewMatrix,Ring,RingElement,ZZ), (genericSkewMatrix,Ring,ZZ)},
     Headline => "make a generic skew symmetric matrix of variables",
     Usage => "genericSkewMatrix(R,r,n)",
     Inputs => {
	  "R" => Ring,
	  "r" => RingElement => {"which is a variable in the ring ", TT "R", " (this input is optional)"},
	  "n" => ZZ
	  },
     Outputs => {
	  {"a skew symmetric ", TO2("Matrix","matrix"), " with ", TT "n", 
	       " rows whose entries above the diagonal are the variables of ", TT "R",
	       " starting with ", TT "r"}
	  },
     "A square ", TO2("Matrix","matrix"), " ", TT "M", " is ", EM "skew symmetric", 
     " if ", TT "transpose(M) + M == 0", ".",
     EXAMPLE {
	  "R = ZZ[a..z];",
	  "M = genericSkewMatrix(R,a,3)",
	  "transpose(M) + M == 0",
	  "genericSkewMatrix(R,d,5)"
	  },
     PARA{},
     "Omitting the input ", TT "r", " is the same as having ", TT "r", 
     " be the first variable in ", TT "R", ".",
     EXAMPLE{
     	  "genericSkewMatrix(R,3)",
     	  "genericSkewMatrix(R,5)"
	  },
     SeeAlso => {(vars, Ring), genericMatrix, genericSymmetricMatrix}
     }

