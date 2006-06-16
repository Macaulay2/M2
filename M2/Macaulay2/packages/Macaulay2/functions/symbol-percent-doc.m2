--- status: DRAFT
--- author(s): kummini 
--- notes: 

document { 
     Key => (symbol %, RingElement, Ideal),
     Headline => "calculate the normal form.",
     Usage => "f % I",
     Inputs => { "f" , "I"},
     Outputs => {
	  RingElement => {"the normal form of ", TT "f", " w.r.t. a Groebner basis
	  of I"}
	  },
     "To reduce f with respect to I, a Groebner basis of I is computed, unless
     it has already been done. Then the element is reduced to normal form using the division
     algorithm.",
     EXAMPLE lines ///
	  R = ZZ/1277[x,y];
    	  I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
	  (x^3 - 2*x) % I
	  (x^3) % I
     ///,
     SeeAlso => {(symbol "/", RingElement, Matrix), "Groebner bases"},
     }

document { 
     Key => (symbol %, Matrix, Ideal),
     Headline => "calculate the normal form.",
     Usage => "f % I",
     Inputs => { "f" , "I"},
     Outputs => {
	  Matrix => {"the matrix whose columns are the normal forms of the columns
	       of f w.r.t I"}
	       },
     "To reduce f with respect to I, a Groebner basis of I is computed, unless
     it has already been done. Then the element is reduced using the division
     algorithm.",
     EXAMPLE lines ///
	  R = QQ[a..d]
	  I = ideal(a^2-b*c,b^3-a*c^2,c^3-b*d^2)
    	  I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
	  (x^3 - 2*x) % I
	  (x^3) % I
     ///,
     SeeAlso => {(symbol "/", RingElement, Matrix), "Groebner bases"},
     }


