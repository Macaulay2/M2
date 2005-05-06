--- status: DRAFT
--- author(s): L. Gold
--- notes: Do something about 3rd para. of "poincare" node

document { 
     Key => poincare,
     Headline => "assemble degrees into polynomial",
     "The Poincare polynomial encodes information about the degrees of
     basis elements of given object in a polynomial. The polynomial
     has a term (-1)^i T_0^(d_0) ... T_(n-1)^(d_(n-1)) in it
     for each basis element of C_i with multi-degree {d_0,...,d_(n-1)}.
     When the multi-degree has a single component, the term is
     (-1)^i T^(d_0).",
     PARA,
     "Note: the monomial ordering used in the degrees ring is ", TT "RevLex", 
    ", so the polynomials in it will be displayed with the
     smallest exponents first.",
     PARA,
     --- DO SOMETHING ABOUT FOLLOWING PARAGRAPH
     TT "(cokernel f).cache.poincare = p", " -- inform the system that
     the Poincare polynomial of the cokernel of ", TT "f", " is ", TT "p",
     ".  This can speed the computation of a Groebner basis of ",
     TT "f", ".  For details, see ", TO "computing Groebner bases",
     ".",
     SeeAlso => {"poincareN", "degreesRing", "reduceHilbert",
	  "hilbertFunction", "hilbertSeries", "hilbertPolynomial"}
     }

document {
     Key => (poincare,Ring),
     Headline => "assemble degrees of an ring into a polynomial",
     Usage => "poincare R",
     Inputs => {
	  "R" => Ring => ""
	  },
     Outputs => {
	  RingElement => "an element of the polynomial ring in variables corresponding to degrees ring "
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of a ring.",
     EXAMPLE {
	  "R=ZZ/101[x]/ideal(x^2);",
     	  "poincare R",
      	  "numerator hilbertSeries R"
	  },
     "Recall that the variables of the polynomial are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}]/ideal(x^2);",
     	  "poincare R",
      	  "numerator hilbertSeries R"
	  }
     }

document { 
     Key => (poincare,Module),
     Headline => "assemble degrees of an module into a polynomial",
     Usage => "poincare M",
     Inputs => {
     	  "M" => Module => ""     
	  },
     Outputs => {
	  RingElement => "an element of the polynomial ring in variables corresponding to degrees ring "
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of a module.",
          EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "M = module ideal x^2",
      	  "poincare M",
      	  "numerator hilbertSeries M"
	  },
     "Recall that the variables of the polynomial are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "M = module ideal x^2;",
	  "poincare M",
	  "numerator hilbertSeries M"
	  }
     }

document { 
     Key => {(poincare,Ideal),(poincare,MonomialIdeal)},
     Headline => "assemble degrees of the quotient of the ambient ring by an ideal into a polynomial",
     Usage => "poincare I",
     Inputs => {
	  "I" => Ideal => {" or a ", TO "MonomialIdeal"}
	       }, 
    Outputs => {
	  RingElement => "an element of the polynomial ring in variables corresponding to degrees ring "
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of an ideal.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2",
      	  "poincare I",
      	  "numerator hilbertSeries I"
	  },
     "Recall that the variables of the polynomial are the variables
     of the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "I = ideal x^2;",
	  "poincare I",
	  "numerator hilbertSeries I"
	  },
     Caveat => {"For an ideal I, ", TT "poincare I", 
	  " calculates the Poincare polynomial of R/I."
	  }
     }

document { 
     Key => (poincare,ChainComplex),
     Headline => "assemble degrees of a chain complex into a polynomial",
     Usage => "poincare C",
     Inputs => {
     	  "C" => ChainComplex => ""
	  },
     Outputs => {
	  RingElement => "an element of the polynomial ring in variables corresponding to degrees ring "
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of a chain complex.",
     EXAMPLE {
     	     "R = ZZ/32003[a..h];",
	     "C = res ideal(a*b, c*d, e*f)",
     	     "C.dd",
	     "poincare C"
	  }
     }

 -- doc7.m2:2194:     Key => poincareComputation, -- WHAT ABOUT THIS?



