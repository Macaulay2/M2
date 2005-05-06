--- status: DRAFT
--- author(s): L. Gold
--- notes: working on this
---    to do: edit main node description, edit subnodes

document { 
     Key => poincare,
     Headline => "assemble degrees into polynomial",
     SeeAlso => {"poincareN", "degreesRing", "reduceHilbert",
	  "hilbertFunction", "hilbertSeries", "hilbertPolynomial"}
     }

document { 
     Key => {(poincare,Ideal),(poincare,MonomialIdeal)},
     Headline => "",
     Usage => "poincare I",
     Inputs => {
	  "I" => Ideal => {" or a ", TO "MonomialIdeal"}
	       },
     Outputs => {
	  },
     EXAMPLE {
	  }
     }

document {
     Key => (poincare,Ring),
     Headline => "",
     Usage => "poincare R",
     Inputs => {
	  "R" => Ring => ""
	  },
     Outputs => {
	  },
     EXAMPLE {
	  }
     }

document { 
     Key => (poincare,Module),
     Headline => "",
     Usage => "poincare M",
     Inputs => {
     	  "M" => Module => ""     
	  },
     Outputs => {
	  },
     EXAMPLE {
	  "R = ZZ/101[x_0 .. x_3,y_0 .. y_3]",
      	  "m = matrix table (2, 2, (i,j) -> x_(i+2*j))",
      	  "n = matrix table (2, 2, (i,j) -> y_(i+2*j))",
      	  "M = cokernel flatten (m*n - n*m)",
      	  "poincare M"
	  },
     }

document { 
     Key => (poincare,ChainComplex),
     Headline => "encode information about the degrees of basis elements
     of a free chain complex in a polynomial.",
     Usage => "poincare C",
     Inputs => {
     	  "C" => ChainComplex => ""
	  },
     Outputs => {
	  },
     EXAMPLE {
	  },
     }


 -- doc7.m2:2194:     Key => poincareComputation, -- WHAT ABOUT THIS?



--     "The polynomial has a term (-1)^i T_0^(d_0) ... T_(n-1)^(d_(n-1)) in it
--     for each basis element of C_i with multi-degree {d_0,...,d_(n-1)}.
--     When the multi-degree has a single component, the term is
--     (-1)^i T^(d_0).",
--     PARA,
--     "The variable ", TT "T", " is defined in a hidden local scope, so
--     will print out as ", TT "$T", " and not be directly accessible.",
--     PARA,
--     "Note: the monomial ordering used in the degrees ring is ", TT "RevLex", 
--    ", so the polynomials in it will be displayed with the
--     smallest exponents first.",
--     PARA,
--     TT "(cokernel f).cache.poincare = p", " -- inform the system that
--     the Poincare polynomial of the cokernel of ", TT "f", " is ", TT "p",
--     ".  This can speed the computation of a Groebner basis of ",
--     TT "f", ".  For details, see ", TO "computing Groebner bases",
--     ".",
