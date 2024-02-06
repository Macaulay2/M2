--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => indices,
     Headline => "indices of a polynomial; also components for a direct sum",
     TT "indices", " has two uses in Macaulay2: indices of a direct sum, or indices of a
     polynomial.  Additionally, the symbol is used as a key in a direct sum
     under which to store a list of the preferred  keys used
     to index the components of the direct sum.",
     SeeAlso => {"directSum", "components", "indexComponents",index,support}
     }

document { 
     Key => {(indices,RingElement),(indices,Matrix)},
     Headline => "indices of variables occurring in a polynomial",
     Usage => "indices f",
     Inputs => {
	  "f" => {"or ",ofClass Matrix, "over a polynomial ring"}
	  },
     Outputs => {
	  List => "of integers, the indices of the variables 
	    occurring in the polynomial or matrix"
	  },
     "The first variable in a polynomial ring has index 0, the second has index 1, etc.
     This function returns a list (in ascending order) of the indices of all
     of the variables that occur in ", TT "f", ".",
     EXAMPLE lines ///
	  R = QQ[a..g]
	  F = a^3+b^2*c+3*f^10*d-1+e-e
	  indices F
	  index a
	  support F
	  ///,
     "The same works for matrices.",
     EXAMPLE lines ///
     	  M = matrix"a+b,c+e;2a-e,3b-c4"
	  indices M
	  support M
     	  ///,
     "This use of ", TO indices, " has no relationship with the use for
     specifying parts of a direct sum",
     SeeAlso => {index, support, indices, (symbol_,Ring,ZZ)}
     }
