--- status: DRAFT 
--- author(s): L.Gold
--- note:

 
document { 
     Key => hilbertSeries,
     Headline => "compute the Hilbert series",
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.", 
     PARA, 
     "Note that the series is provided as an ", TO "Expression", ".",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare"}
     }
document { 
     Key => {(hilbertSeries,PolynomialRing), (hilbertSeries,QuotientRing)},
     Headline => "compute the Hilbert series of the ring",
     Usage => "hilbertSeries R",
     Inputs => {
	  "R" => {" or a ", TO QuotientRing}
	  },
     Outputs => {
	  Expression => "" },
     "We compute the ", TO2(hilbertSeries, "Hilbert series"), " of a ring.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries R",
	  },
     "Recall that the variables of the power series are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries R",
	  },
     }
document { 
     Key => {(hilbertSeries,Module), (hilbertSeries,CoherentSheaf)},
     Headline => "compute the Hilbert series of the module",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M" => {" or a ", TO CoherentSheaf} 
	  },
     Outputs => {
	  Expression => "" },
     "We compute the ", TO2(hilbertSeries, "Hilbert series"), " of a
     module.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "M = module ideal x^2",
      	  "hilbertSeries M",
      	  "numerator oo",
      	  "poincare M",
	  },
     "Recall that the variables of the power series are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "M = module ideal x^2;",
	  "hilbertSeries M",
	  }
     }
document { 
     Key => (hilbertSeries,Ideal),
     Headline => "compute the Hilbert series of the quotient of the ambient ring by the ideal",
     Usage => "hilbertSeries I",
     Inputs => {
	  "I" => Ideal => ""
	  },
     Outputs => {
	  Expression => "" },
     "We compute the ", TO2 (hilbertSeries, "Hilbert series"), " of R/I, the
     quotient of the ambient ring by the ideal. Caution: For an ideal
     I, ", TT "hilbertSeries I ", "calculates the Hilbert series of R/I.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2",
      	  "hilbertSeries I",
      	  "numerator oo",
      	  "poincare I",
	  "reduceHilbert o3"	  
	  },
     "Recall that the variables of the power series are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "I = module ideal x^2;",
	  "hilbertSeries I"
	  },
     Caveat => {"For an ideal I, ", TT "hilbertSeries I", " calculates
	  the Hilbert series of R/I."
	  }
     }
document { 
     Key => (hilbertSeries,ProjectiveHilbertPolynomial),
     Headline => "compute the Hilbert series of a projective Hilbert polynomial",
     Usage => "hilbertSeries P",
     Inputs => {
	  "P" => ProjectiveHilbertPolynomial => ""
	  },
     Outputs => {
	  Expression => "" },
     "We compute the ", TO2(hilbertSeries, "Hilbert series"), " of a
     projective Hilbert polynomial.",
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 3",
      	  "hilbertSeries P"
	  },
     "Computing the ", TO2(hilbertSeries, "Hilbert series"), " of a
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
	  "hilbertSeries P"
     }
}
document { 
     Key => (hilbertSeries,ProjectiveVariety),
     Headline => "compute the Hilbert series of a projective variety",
     Usage => "hilbertSeries V",
     Inputs => {
	  "V" =>  ProjectiveVariety => ""
	  },
     Outputs => {
	  Expression => "" },
     "We compute the ", TO2(hilbertSeries, "Hilbert series"), " of a
     projective variety.",
     EXAMPLE {
	  "V = Proj(QQ[x,y])",
	  "hilbertSeries V"
	  }
     }
document { 
     Key => [hilbertSeries, Order],
     Headline => "display the truncated power series expansion",
     Usage => "hilbertSeries(..., Order => n)",
     Inputs => {
	  "n" => ZZ => ""},
     "We compute the Hilbert series both without and with the optional
     argument. In the second case notice the first 5 terms of the
     power series expansion are displayed rather than expressing the
     series as a rational function.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "hilbertSeries(R/x^3)",
	  "hilbertSeries(R/x^3, Order =>5)"
	  },
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

