--- status: DRAFT
--- author(s): L.Gold with some help from J.Caviglia
--- notes: still working on subnodes for each type of usage
document { 
     Key => hilbertSeries,
     Headline => "compute the Hilbert series",
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  Note that the series is provided as an ",
     TO "Expression", ".",
     SeeAlso => {"degreesRing", "reduceHilbert"}
     }
document { 
     Key => {(hilbertSeries,PolynomialRing), (hilbertSeries,QuotientRing)},
     Headline => "compute the Hilbert series of a ring",
     Usage => "hilbertSeries R",
     Inputs => {
	  "R" => {" or a ", TO QuotientRing}
	  },
     Outputs => {
	  Expression => "" },
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  Notice that the series is provided as an ", 
     TO "Expression", " representing a rational function with that
     series and it is not reduced.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"hilbertSeries"}
     }
document { 
     Key => (hilbertSeries,ProjectiveVariety),
     Headline => "compute the Hilbert series of a projective variety",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M" =>  ProjectiveVariety => ""
	  },
     Outputs => {
	  Expression => "" },
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component. Note that the series is provided as an ",
     TO "Expression", " representing a rational function with that
     series and it is not reduced.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"hilbertSeries"}
     }
document { 
     Key => (hilbertSeries,ProjectiveHilbertPolynomial),
     Headline => "compute the Hilbert series projective Hilbert polynomial",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M" => ProjectiveHilbertPolynomial => ""
	  },
     Outputs => {
	  Expression => "" },
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  Notice that the series is provided as an ", 
     TO "Expression", " representing a rational function with that
     series and it is not reduced.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"hilbertSeries"}
     }
document { 
     Key => {(hilbertSeries,Module), (hilbertSeries,CoherentSheaf)},
     Headline => "compute the Hilbert series of a module",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M" => {" or a ", TO CoherentSheaf} 
	  },
     Outputs => {
	  Expression => "" },
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  Notice that the series is provided as an ", 
     TO "Expression", " representing a rational function with that
     series and it is not reduced.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"hilbertSeries"}
     }
document { 
     Key => (hilbertSeries,Ideal),
     Headline => "compute the Hilbert series of an ideal",
     Usage => "hilbertSeries M",
     Inputs => {
	  "M" => Ideal => ""
	  },
     Outputs => {
	  Expression => "" },
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  Notice that the series is provided as an ", 
     TO "Expression", " representing a rational function with that
     series and it is not reduced.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"hilbertSeries"}
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
	  "hilbertSeries(R/x^3, Order =>5)",
	  },
     SeeAlso => {"hilbertSeries"}
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

