--- status: DRAFT
--- author(s): kummini 
--- notes: 

document { 
     Key => {(symbol %, RingElement, Ideal),
	  (symbol %, RingElement, MonomialIdeal),
	  (symbol %, RingElement, Matrix),
	  (symbol %, RingElement, RingElement),
	  (symbol %, Matrix, Ideal),
	  (symbol %, Matrix, MonomialIdeal),
	  (symbol %, Matrix, RingElement)
	  },
     Headline => "calculate the normal form of ring elements.",
     Usage => "f % I",
     Inputs => { "f" => Nothing => {ofClass RingElement, ", or ",
	       ofClass Matrix},
	     "I" => Nothing => {ofClass Ideal, ", ",
		  ofClass Matrix, ", or ",
		  ofClass RingElement}
	     },
     Outputs => {
	  Nothing => {"the normal form of ", TT "f", " w.r.t. a Groebner basis
	  of the ideal I"}
	  },
     "The result has the same type as ", TT "f", ".  If ", TT "f", " is a matrix, then the matrix of the
     normal forms of each column is returned.",
     PARA{},
     "To reduce ", TT "f", " with respect to ", TT "I", 
     ", a Groebner basis of ", TT "I", " is computed, unless
     it has already been done. Then the element is reduced 
     to normal form using the division algorithm.",
     EXAMPLE lines ///
	  R = ZZ/1277[x,y];
    	  I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
	  (x^3 - 2*x) % I
	  (x^3) % I
	  S = ZZ[x,y]
	  144*x^2*y^2 % (7*x*y-2)
     ///,
     "Of course, normal forms work over quotient rings too.",
     EXAMPLE lines ///
     	  A = QQ[x,y,z]/(x^3-y^2-3)
	  I = ideal(x^4, y^4)
	  J = ideal(x^3*y^2, x^2*y^3)
	  (gens J) % I
     ///,
     "Here is an example involving rational functions",
     EXAMPLE lines ///
     	  kk = toField(frac(ZZ[a,b]))
	  B = kk[x,y,z]
	  I = ideal(a*x^2-b*x-y-1, 1/b*y^2-z-1)
	  gens gb I
	  x^2*y^2 % I
     ///,
     SeeAlso => {"Groebner bases", generators, toField},
     }

document {
     Key => (symbol %, Matrix, Matrix),
     Headline => "find the normal form modulo the image of a map",
     TT "f % g", " -- yields the reduction of the columns of the matrix
     ", TT "f", " modulo a Groebner basis of the matrix ", TT "g", "."
     }



