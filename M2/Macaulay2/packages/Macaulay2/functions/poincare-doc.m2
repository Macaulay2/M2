--- status: DRAFT
--- author(s): L. Gold
--- notes: Do something about 3rd para. of "poincare" node

document { 
     Key => poincare,
     Headline => "assemble degrees into polynomial",
     "The Poincare polynomial is the numerator of the ", 
     TO2 (hilbertSeries, "Hilbert series"), ". It encodes information
     about the degrees of basis elements of given object. The
     polynomial has terms ",
     TT "(-1)^i T_0^(d_0) ... T_(n-1)^(d_(n-1))", 
     " in it for each basis element of ", TT "C_i", " with multi-degree",
     TT "{d_0,...,d_(n-1)}", ". When the multi-degree has a single
     component, the term is", TT "(-1)^i T^(d_0).",
     PARA{},
     "This polynomial is an element of the ", 
     TO2(degreesRing, "degrees ring"), ". Notice that the monomial
     ordering used in the degrees ring is ", TT "RevLex", ", so the
     polynomials in it will be displayed with the smallest exponents first.",
--      PARA{},
--      TT "(cokernel f).cache.poincare = p", " -- inform the system that
--      the Poincare polynomial of the cokernel of ", TT "f", " is ", TT "p",
--      ".  This can speed the computation of a Groebner basis of ",
--      TT "f", ".  For details, see ", TO "Groebner bases",
--      ".",
     SeeAlso => {"poincareN", "degreesRing", "hilbertFunction",
	  "hilbertSeries", "hilbertPolynomial", "reduceHilbert"}
     }

document {
     Key => (poincare,Ring),
     Headline => "assemble degrees of an ring into a polynomial",
     Usage => "poincare R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  RingElement => "in the Laurent polynomial ring whose variables correspond to the degrees of the ring"
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
     	  "M" => Module     
	  },
     Outputs => {
	  RingElement => "in the Laurent polynomial ring whose variables correspond to the degrees of the ambient ring"
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of a module.",
          EXAMPLE {
	  "R = ZZ/101[w..z];",
	  "M = module monomialCurveIdeal(R,{1,3,4});",
      	  "poincare M",
      	  "numerator reduceHilbert hilbertSeries M"
	  },
     "Recall that the variables of the polynomial are the variables of
     the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "M = module ideal x^2;",
	  "poincare M",
	  "numerator reduceHilbert hilbertSeries M"
	  }
     }

undocumented {(poincare,MonomialIdeal)}

document { 
     Key => (poincare,Ideal),
     Headline => "assemble degrees of the quotient of the ambient ring by an ideal into a polynomial",
     Usage => "poincare I",
     Inputs => {
	  "I" => Ideal
	       }, 
    Outputs => {
	  RingElement => "in the Laurent polynomial ring whose variables correspond to the degrees of the ambient ring"
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of the
     quotient of the ambient ring by an ideal.",
     EXAMPLE {
	  "R = ZZ/101[w..z];",
	  "I = monomialCurveIdeal(R,{1,3,4});",
      	  "poincare I",
      	  "numerator reduceHilbert hilbertSeries I"
	  },
     "Recall that the variables of the polynomial are the variables
     of the degrees ring.",
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
	  "I = ideal x^2;",
	  "poincare I",
	  "numerator reduceHilbert hilbertSeries I"
	  },
     Caveat => {
	  "As is often the case, calling this function on an ideal ",
	  TT "I", " actually computes it for ", TT "R/I", " where ", 
	  TT "R", " is the ring of ", TT "I", ".",
	  }
     }

document { 
     Key => (poincare,ChainComplex),
     Headline => "assemble degrees of a chain complex into a polynomial",
     Usage => "poincare C",
     Inputs => {
     	  "C" => ChainComplex
	  },
     Outputs => {
	  RingElement => "in the Laurent polynomial ring whose
	  variables correspond to the degrees of the ambient ring"
	  },
     "We compute the ", TO2(poincare, "Poincare polynomial"), " of a
     chain complex.",
     EXAMPLE {
     	     "R = ZZ/32003[a..h];",
	     "C = res ideal(a*b, c*d, e*f)",
     	     "C.dd",
	     "poincare C"
	  }
     }


