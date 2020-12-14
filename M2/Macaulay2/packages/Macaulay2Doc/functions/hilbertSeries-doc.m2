--- status: DRAFT 
--- author(s): L.Gold
--- note:

undocumented {(hilbertSeries,CoherentSheaf)}

document { 
     Key => hilbertSeries,
     Headline => "compute the Hilbert series",
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.", 
     "Note that the series is provided as a type of expression called
     a ", TO "Divide", ".",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare",
	  "poincareN", "hilbertPolynomial", "hilbertFunction"}
     }

document { 
     Key => {(hilbertSeries, PolynomialRing), (hilbertSeries,QuotientRing)},
     Headline => "compute the Hilbert series of a ring",
     Usage => "hilbertSeries R",
     Inputs => {
	  "R"
	  },
     Outputs => {
	  Divide => "the Hilbert series" },
     "We compute the ", TO2(hilbertSeries, "Hilbert series"), " of a ring.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}]/ideal(x^2);",
      	  "s = hilbertSeries R",
	  "numerator s",
     	  "poincare R"
	  },
     "Recall that the variables of the power series are the variables of
     the ", TO2 (degreesRing,"degrees ring"), ".",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}]/ideal(x^2);",
      	  "s = hilbertSeries R",
	  "numerator s",
     	  "poincare R"
	  }
     }

document { 
     Key => (hilbertSeries,Module),
     Headline => "compute the Hilbert series of the module",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  Divide => "the Hilbert series" 
	  },
     "We compute the ", TO2 (hilbertSeries, "Hilbert series"), " of a
     module.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "M = module ideal x^2",
      	  "s = hilbertSeries M",
      	  "numerator s",
      	  "poincare M"
	  },
     "Recall that the variables of the power series are the variables of
     the ", TO2 (degreesRing,"degrees ring"), ".",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "M = module ideal x^2;",
	  "s = hilbertSeries M",
     	  "numerator s",
	  "poincare M"
	  }
     }

-- NOTE: listed as undocumented above.
-- document { 
--      Key => (hilbertSeries,CoherentSheaf),
--      Headline => "compute the Hilbert series of a coherent sheaf",
--      Usage => "hilbertSeries M",
--      Inputs => {
-- 	  "M"
-- 	  },
--      Outputs => {
-- 	  Divide => "the Hilbert series" },
--      "We compute the ", TO2 (hilbertSeries, "Hilbert series"), " of a
--      coherent sheaf.",
--      EXAMPLE {
-- 	  "V = Proj(ZZ/101[x_0..x_2]);",
-- 	  "M = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
--       	  "s = hilbertSeries M",
--       	  "numerator s"
-- 	  }
--      }

document { 
     Key => {
	 (hilbertSeries, Ideal),
	 (hilbertSeries, MonomialIdeal)},
     Headline => "compute the Hilbert series of the quotient of the ambient ring by the ideal",
     Usage => "hilbertSeries I",
     Inputs => {
	  "I" => Ideal
	  },
     Outputs => {
	  Divide =>  "the Hilbert series" },
     "We compute the ", TO2  (hilbertSeries, "Hilbert series"), " of ", 
     TT "R/I", ", the quotient of the ambient ring by the
     ideal. Caution: For an ideal ", TT "I", " running ", 
     TT "hilbertSeries I ", "calculates the Hilbert series of ",
     TT "R/I", ".",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2",
      	  "s = hilbertSeries I",
      	  "numerator s",
      	  "poincare I",
	  "reduceHilbert s"	  
	  },
     "Recall that the variables of the power series are the variables of
     the ", TO2 (degreesRing,"degrees ring"), ".",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "I = ideal x^2;",
	  "s = hilbertSeries I",
	  "numerator s",
	  "poincare I",
	  "reduceHilbert s"
	  },
     Caveat => {
	  "As is often the case, calling this function on an ideal ", 
	  TT "I", " actually computes it for ", TT "R/I", " where ", 
	  TT "R", " is the ring of ", TT "I", ".",
	  }
     }
document { 
     Key => (hilbertSeries,ProjectiveHilbertPolynomial),
     Headline => "compute the Hilbert series of a projective Hilbert polynomial",
     Usage => "hilbertSeries P",
     Inputs => {
	  "P" => ProjectiveHilbertPolynomial
	  },
     Outputs => {
	  Divide =>  "the Hilbert series" },
     "We compute the ", TO2 (hilbertSeries, "Hilbert series"), " of a
     projective Hilbert polynomial.",
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 3",
      	  "s = hilbertSeries P",
     	  "numerator s"
	  },
     "Computing the ", TO2 (hilbertSeries, "Hilbert series"), " of a
     projective variety can be useful for finding the h-vector of a
     simplicial complex from its f-vector. For example, consider the
     octahedron. The ideal below is its Stanley-Reisner ideal. We can
     see its f-vector (1, 6, 12, 8) in the Hilbert polynomial, and
     then we get the h-vector (1,3,3,1) from the coefficients of the
     Hilbert series projective Hilbert polynomial.",
     EXAMPLE {
     	  "R = QQ[a..h];",
	  "I = ideal (a*b, c*d, e*f);",
	  "P=hilbertPolynomial(I)",
	  "s = hilbertSeries P",
	  "numerator s"
     }
}

document { 
     Key => (hilbertSeries,ProjectiveVariety),
     Headline => "compute the Hilbert series of a projective variety",
     Usage => "hilbertSeries V",
     Inputs => {
	  "V" =>  ProjectiveVariety
	  },
     Outputs => {
	  Divide =>   "the Hilbert series" },
     "We compute the ", TO2 (hilbertSeries, "Hilbert series"), " of a
     projective variety, that is, the Hilbert series of the
     homogeneous coordinate ring of ", TT "V", ". The saturation of
     the ideal may need to be computed.",
     PARA {
	  "This method is not implemented yet."
	  }
-* temporarily disabled
     EXAMPLE {
	  "V = Proj(QQ[x,y])",
	  "s = hilbertSeries V",
	  "numerator s"
	  }
*-
     }
document { 
     Key => [hilbertSeries, Order],
     Headline => "display the truncated power series expansion",
     Usage => "hilbertSeries(..., Order => n)",
     Inputs => {
	  "n" => ZZ},
     Consequences => {
	  {"The output is no longer of type ", TO "Divide", ". It is a
	  polynomial in the ", TO2 (degreesRing,"degrees ring"), "."}
	  },
     "We compute the Hilbert series both without and with the optional
     argument. In the second case notice the terms of power series
     expansion up to, but not including, degree 5 are displayed rather
     than expressing the series as a rational function. The polynomial
     expression is an element of a Laurent polynomial ring that is
     the ", TO2 (degreesRing,"degrees ring"), " of the ambient ring.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "hilbertSeries(R/x^3)",
	  "hilbertSeries(R/x^3, Order =>5)"
	  },
     "If the ambient ring is multigraded, then the ",
     TO2 (degreesRing,"degrees ring"), " has multiple variables.",
     EXAMPLE {
	  "R = ZZ/101[x,y, Degrees=>{{1,2},{2,3}}];",
	  "hilbertSeries(R/x^3, Order =>5)"
	  },     
     "The heft vector provides a suitable monomial ordering and degrees in the ring of the Hilbert series.",
     EXAMPLE lines ///
     R = QQ[a..d,Degrees=>{{-2,-1},{-1,0},{0,1},{1,2}}]
     hilbertSeries(R, Order =>3)
     degrees ring oo
     heft R
     ///,
     }

TEST ///
R = ZZ/101[x,y]
M = R^1/x
T = degreesRing R
t = T_0
assert( hilbertSeries (M, Order => 5) == t^4+t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 4) == t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 7) == t^6+t^5+t^4+t^3+t^2+t+1 )
///

document {
     Key => [hilbertSeries,Reduce],
     Headline => "reduce the Hilbert series",
     Usage => "hilbertSeries(..., Reduce => true)",
     Consequences => {{"the resulting rational function is reduced by cancelling
	       factors of the numerator that occur explicitly as factors of the denominator.  
	       See also ", TO "reduceHilbert", "."
	       }},
     EXAMPLE lines ///
     R = QQ[x,y,z];
     hilbertSeries ideal (x,y)
     hilbertSeries(ideal (x,y), Reduce => true)
     ///
     }
	  
