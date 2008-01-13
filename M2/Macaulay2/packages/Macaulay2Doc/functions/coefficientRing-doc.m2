--- status: DRAFT
--- author(s): taken from Dan's, MES
--- notes: 

undocumented {
	  (coefficientRing,FractionField),
	  (coefficientRing,QuotientRing),
	  -- (coefficientRing,GaloisField),
	  (coefficientRing,PolynomialRing)
	  }

document { 
     Key => {coefficientRing,(coefficientRing,Ring)},
     Headline => "get the coefficient ring",
     Usage => "coefficientRing R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Ring => {"the coefficient ring of ", TT "R"}
	  },
     "If ", TT "R", " is a polynomial ring, then the coefficient ring is
     the base ring from which the coefficients are drawn.  If ", TT "R", " is
     constructed from a polynomial ring as a quotient ring or a fraction ring
     or a sequence of such operations, then the original coefficient ring
     is returned.",
     EXAMPLE {
	  "coefficientRing(ZZ/101[a][b])",
      	  "ultimate(coefficientRing,ZZ/101[a][b])"
	  },
     SeeAlso => {ultimate, baseRings}
     }

