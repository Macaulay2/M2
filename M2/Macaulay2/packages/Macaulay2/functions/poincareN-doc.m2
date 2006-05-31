--- status: DRAFT
--- author(s): L. Gold
--- notes: needs better example

document { 
     Key => {poincareN,(poincareN,ChainComplex)},
     Headline => "assemble degrees into polynomial",
     Usage => "poincareN C",
     Inputs => {
     	  "C" => ChainComplex
	  },
     Outputs => {
	  RingElement => "in the Laurent polynomial ring whose
	  variables correspond to the degrees of the ambient ring"
	  },
     "This function encodes information about the degrees of basis
     elements of a free chain complex in a polynomial. The polynomial
     has a term ", TT "S^i T_0^(d_0) ... T_(n-1)^(d_(n-1))", 
     " in it for each basis element of ", TT "C_i", 
     " with multi-degree",  TT "{d_0,...,d_(n-1)}.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[a,b,c, Degrees=>{1,1,2}];",
	  "C = res cokernel vars R", 
	  "betti C",
      	  "p = poincareN C"
	  },
     "Setting the ", TT "S", 
     " variable to -1 gives the Poincare polynomial calculated by ", 
     TO "poincare", ".",
     EXAMPLE {
     	  "use ring p",
      	  "substitute(p, {S=>-1})"
	  },
     SeeAlso => {"poincare", "degreesRing", "hilbertFunction",
	  "hilbertSeries", "hilbertPolynomial", "reduceHilbert" }
     }
