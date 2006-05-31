--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {genericSymmetricMatrix, (genericSymmetricMatrix,Ring,ZZ), (genericSymmetricMatrix,Ring,RingElement,ZZ)},
     Headline => "make a generic symmetric matrix",
     Usage => "genericSymmetricMatrix(R,r,n)",
     Inputs => {
	  "R" => Ring,
	  "r" => RingElement => {"which is a variable in the ring ", TT "R", 
	       " (this input is optional)"},
	  "n" => ZZ
	  },
     Outputs => {
	  {"a symmetric ", TO2("Matrix", "matrix"), " with ", TT "n",
	       " rows whose entries on and above the diagonal are the variables of ", TT "R",
	       " starting with ", TT "r"}
	  },
     "A square ", TO2("Matrix","matrix"), " ", TT "M", " is ", EM "symmetric", 
     " if ", TT "transpose(M) - M == 0", ".",
     EXAMPLE {
	  "R = ZZ[a..z];",
	  "M = genericSymmetricMatrix(R,a,3)",
	  "transpose(M) - M == 0",
	  "genericSymmetricMatrix(R,d,5)"
	  },
     PARA{},
     "Omitting the input ", TT "r", " is the same as having ", TT "r", 
     " be the first variable in ", TT "R", ".",
     EXAMPLE{
     	  "genericSymmetricMatrix(R,3)",
     	  "genericSymmetricMatrix(R,5)"
	  },
     SeeAlso => {(vars, Ring), genericMatrix, genericSkewMatrix}
     }

