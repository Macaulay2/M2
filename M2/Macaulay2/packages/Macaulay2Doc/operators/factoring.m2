document {
     Key => {
       (symbol //, Matrix, Matrix),
       (symbol //, RingElement, MonomialIdeal),
       (symbol //, RingElement, GroebnerBasis),
       (symbol //, RingElement, RingElement),
       (symbol //, Matrix, MonomialIdeal),
       (symbol //, Matrix, GroebnerBasis),
       (symbol //, Matrix, Number),
       (symbol //, Number, Matrix),
       (symbol //, Matrix, RingElement),
       (symbol //, RingElement, Matrix)
       },
     Headline => "factor a map through another",
     Usage => "f//g",
     Inputs => {
	  "f" => {"between modules F --> H, or ",
	     ofClass RingElement},
	  "g" => {"between modules G --> H, ",
	       ofClass RingElement, ", ",
	       ofClass MonomialIdeal, ", or ",
	       ofClass GroebnerBasis}
	  },
     Outputs => {
	  Matrix => "a matrix h : F --> G"
	  },
     "If ", TT "f", " is a matrix, and ", TT "g", " is a matrix or Gröbner basis, then ", TT "quotient(f,g)", " is an alternate
     notation for ", TT "f//g", ".",
     PARA{},
     "If either ", TT "f", " or ", TT "g", " is a ring element, then it is taken to be a scalar matrix acting on ", TT "H", ".  If both are ring elements,
     then the result is also a ring element.  If ", TT "g", " is a
     ", TO "MonomialIdeal", ", then it is taken to be the matrix of generators of ", TT "g", ".  Finally, if ", TT "g", " is a ", TO "GroebnerBasis", "
     object, then the Gröbner basis as so far computed is used.  In these latter two cases, no Gröbner bases
     will be computed.",
     PARA{},
     "The resulting matrix ", TT "h", " is such that ", TT "f - g*h", " is the reduction of ", TT "f", " modulo a Gröbner basis
     for the image of ", TT "g", ".",
     PARA{},
     "If the remainder ", TT "f - g*h", " is zero,
     then the quotient ", TT "f//g", " satisfies the equation ", TT "f === g * (f//g)", ".",
     PARA{},
     "One common use is the following.  If an ideal contains 1, then we may write 1 in terms
     of the generators of the ideal.  First we make an ideal.",
     EXAMPLE lines ///
     A = ZZ/101[x,y,z]
     F = x^4 - y*z*(1-x)^2 - z - y^3
     I = ideal(F,diff(x,F),diff(y,F),diff(z,F))
     ///,
     "Transposing the (row) matrix of generators of the ideal puts the generators on separate lines and shows the degrees.",
     EXAMPLE lines ///
     transpose gens I
     ///,
     "Next we test whether 1 is in the ideal.",
     EXAMPLE lines ///
     1 % I
     ///,
     "We see that 1 is in the ideal.  Now we represent 1 in terms of the generators of ", TT "I", ".",
     EXAMPLE lines ///
     h = 1 // gens I
     gens I * h
     ///,
     SeeAlso => {(symbol %, Matrix, Matrix), generators, diff, substitute, quotient, remainder, quotientRemainder }
     }
