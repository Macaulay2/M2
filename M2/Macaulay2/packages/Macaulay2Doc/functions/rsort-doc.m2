--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {rsort,(rsort,Matrix),(rsort,List),(rsort, List, Function)},
     Headline => "sort a list or matrix in reverse order",
     "Same as ", TO "sort", " except that the elements or columns
     are placed in reverse order.",
     EXAMPLE {
	  "rsort {4,2,3,1}"
	  },
     "The optional parameters are the same as for ", TO sort, ".",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "m = matrix{{a*b, c*d, a*d^3, b^3*c, 1_R}}",
	  "rsort(m, DegreeOrder=>Ascending)"
	  },
     SeeAlso => {sort},
     Subnodes => {
	 TO [rsort, MonomialOrder],
	 TO [rsort, DegreeOrder],
         },
     }
document { 
     Key => [rsort, MonomialOrder],
     Headline => "specify Ascending or Descending monomial order",
     "The meaning is the same as from ", TO [sort,MonomialOrder], "."
     }
document { 
     Key => [rsort, DegreeOrder],
     Headline => "specify Ascending, Descending, or null",
     "The meaning is the same as from ", TO [sort,MonomialOrder], "."
     }

