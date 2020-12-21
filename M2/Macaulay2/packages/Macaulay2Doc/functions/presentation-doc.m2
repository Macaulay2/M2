--- status: DRAFT
--- author(s): MES, taken from before
--- notes: 

undocumented {
     	  (presentation,QuotientRing,QuotientRing),	  
	  (presentation,PolynomialRing),
	  (presentation,PolynomialRing,PolynomialRing),
	  (presentation,QuotientRing,PolynomialRing)
     }
document { 
     Key => presentation,
     Headline => "presentation of a module or ring",
     SeeAlso => {minimalPresentation}
     }
document { 
     Key => {
	  (presentation,PolynomialRing,QuotientRing),
     	  (presentation,QuotientRing)
	  },
     Headline => "presentation of a quotient ring",
     Usage => "presentation B\npresentation(A,B)",
     Inputs => {
	  "A",
	  "B" => "a quotient ring of A"
	  },
     Outputs => {
	  Matrix => {"whose image is the ideal of ", TT "A", " defining ", TT "B"}
	  },
     "If A is not present, then it is understood to be the ultimate ambient polynomial ring of B.
     In general, A may be any ring of which B is a quotient.",
     PARA{},
     "In the examples below, A is the ultimate ambient polynomial ring of A, B and C.",
     EXAMPLE lines ///
     	  A = QQ[a..d];
	  B = A/(a^2,b^3);
	  C = B/(a*b*c,b*c*d, b^2);
	  presentation A
	  presentation B
	  presentation C
	  presentation(B,C)
	  presentation(A,C)
	  minimalPresentation C
	  ///,
     PARA{},
     Caveat => "The given presentation is often not minimal",
     SeeAlso => {minimalPresentation}
     }
document { 
     Key => (presentation,Module),
     Headline => "presentation of a module",
     Usage => "presentation M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  Matrix => {"a presentation matrix of ", TT "M"}
	  },
     "A presentation of ", TT "M", " is a map ", TT "p", " so that ", TT "coker p", " is 
     isomorphic to ", TT "M", ".  The presentation obtained is expressed in 
     terms of the given generators, i.e., the modules ", TT "cover M", " and 
     ", TT "target p", " are identical.
     The isomorphism can be obtained as ", TT "map(M,coker p,1)", ".",
     PARA{},
     "Since a module M may be described as a submodule or a subquotient 
     module of a free module, some computation may be required to produce 
     a presentation.  See also ",
     TO "trim", ", or ", 
     TO "minimalPresentation", ",
      which do a bit more work to try to
     eliminate redundant generators.",
     EXAMPLE lines ///
     	  R = QQ[a,b,c];
	  I = ideal"a2-b2,abc"
	  M = I/(I^2+a*I)
	  presentation M
	  ///,
     SeeAlso => {minimalPresentation, trim, generators, relations, cover}
     }
