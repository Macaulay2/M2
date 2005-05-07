--- status: DRAFT
--- author(s): L. Gold
--- notes:  

document { 
     Key => degreesRing,
     Headline => "the ring of degrees",
     "Elements of this ring are used as variables for Poincare
     polynomials and Hilbert series.", 
     PARA,
     "The degrees ring is a Laurent polynomial ring as can be seen by
     the option ", TT "inverses => true", ". The monomial ordering
     used in the degrees ring is ", TT "RevLex", " so the polynomials
     in it will be displayed with the smallest exponents first,
     because such polynomials are often used as Hilbert series.",
     SeeAlso => {"poincare", "poincareN", "hilbertFunction",
	  "hilbertSeries", "hilbertPolynomial", "reduceHilbert" }
     }

document { 
     Key => (degreesRing,ZZ),
     Headline => "the ring of degrees",
     Usage => "degreesRing n",
     Inputs => {
	  "n" => ZZ => ""
	  },
     Outputs => {
	  PolynomialRing => "actually a Laurent polynomial ring"
	  },
     "This function produces a Laurent polynomial ring in n variables
     whose monomials are to be used to represent degrees in another
     ring with multi-degrees of length n.",
     EXAMPLE {
	  "degreesRing 3"
	  }
     }

document { 
     Key => (degreesRing,Ring),
     Undocumented => {(degreesRing, QuotientRing), (degreesRing,PolynomialRing)},
     Headline => "the ring of degrees",
     Usage => "degreesRing R",
     Inputs => {
	  "R" => ""
	  },
     Outputs => {
	  PolynomialRing => "actually Laurent polynomial ring"
	  },
     "This function produces a Laurent polynomial ring in n variables
     whose monomials are the degrees of elements of the given ring.",
     EXAMPLE {
	  "R =  ZZ [x, y];",
	  "degreesRing R",
	  "S = ZZ[x,y, Degrees=>{{1,1},{1,1}}];",
	  "degreesRing S"
     	  }
     }


-- document { 
--      Key => (degreesRing,Module),
--      Headline => "the ring of degrees",
--      Usage => "degreesRing M",
--      Inputs => {
-- 	  "M" => Module => ""
-- 	  },
--      Outputs => {
-- 	  PolynomialRing => ""
-- 	  },
--      "For an R-module this function produces the ring in n variables
--      whose monomials are the degrees of elements of the ring R.",
--      EXAMPLE {
-- 	  "R =  ZZ [x, y];",
--      	  "M = R^3;",
-- 	  "degreesRing M",
-- 	  "S = ZZ[x,y, Degrees=>{{1,1},{1,1}}]",
-- 	  "M = S^3;",	  
-- 	  "degreesRing M"
--      	  }
--      }
-- 
-- document { 
--      Key => (degreesRing,CoherentSheaf),
--      Headline => "the ring of degrees",
--      Usage => "degreesRing S",
--      Inputs => {
-- 	  "S" => CoherentSheaf => ""
-- 	  },
--      Outputs => {
-- 	  PolynomialRing => ""
-- 	  },
--      EXAMPLE {
-- 	  "V = Proj(ZZ/101[x_0..x_2]);",
-- 	  "M = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
--       	  "degreesRing M"
-- 	  }
--      }

