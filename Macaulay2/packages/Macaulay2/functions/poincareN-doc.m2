--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {poincareN,(poincareN,ChainComplex)},
     Headline => "assemble degrees into polynomial",
     Usage => "poincareN C",
     Inputs => {
     	  "C" => ChainComplex => ""
	  },
     Outputs => {
	  RingElement => ""
	  },
     "This function encodes information about the degrees of basis
     elements of a free chain complex in a polynomial. The polynomial
     has a term ", TT "S^i T_0^(d_0) ... T_(n-1)^(d_(n-1))", " in it
     for each basis element of ", TT "C_i", " with multi-degree",
     TT "{d_0,...,d_(n-1)}.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "p = poincareN resolution cokernel vars R"
	  },
     "Setting the S variable to 1 would give the Poincare polynomial
     calculated by ",
     TO "poincare", ".",
     EXAMPLE {
      	  "poincare resolution cokernel vars R"
	  },
     SeeAlso => {"poincare", "degreesRing", "hilbertFunction",
	  "hilbertSeries", "hilbertPolynomial", "reduceHilbert" }
     }
