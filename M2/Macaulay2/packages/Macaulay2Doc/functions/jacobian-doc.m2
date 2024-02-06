--- status: moved December 2020

-- in IntegralClosure: (jacobian, RingElement)

document {
     Key => jacobian,
     Headline => "the Jacobian matrix of partial derivatives",
     SeeAlso => {
	 "diff",
	 "contract"
	  }
     }
document {
     Key => (jacobian,Matrix),
     Headline => "the matrix of partial derivatives of polynomials in a matrix",
     Usage => "jacobian f",
     Inputs => {"f" => " with one row"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the polynomial entries of ", TT "f"}},
     "If ", TT "f", " is a 1 by ", TT "m", " matrix over a polynomial ring ",
     TT "R", " with ", TT "n"," indeterminates,
     then the resulting matrix of partial derivatives has dimensions ",TT "n"," by ",TT "m",", 
     and the ", TT "(i,j)", " entry is the partial derivative of the ", TT "j", "-th entry of
     ", TT "f", " by the ", TT "i", "-th indeterminate of the ring.",
     PARA{},
     "If the ring of ", TT "f", " is a quotient polynomial ring ", TT "S/J", ",
     	  then only the derivatives of the given entries of ", TT "f", " are
     	  computed and NOT the derivatives of elements of ", TT "J", ".",
     	  EXAMPLE {
	       "R = QQ[x,y,z];",
      	       "f = matrix{{y^2-x*(x-1)*(x-13)}}",
      	       "jacobian f",
	       },
	  "If the ring of ", TT "f", " is a polynomial ring over a polynomial ring,
	  then indeterminates in the coefficient ring are treated as constants.",
     	  EXAMPLE {
	       "R = ZZ[a,b,c][x,y,z]",
	       "jacobian matrix{{a*x+b*y^2+c*z^3, a*x*y+b*x*z}}"
	       }
     }
document {
     Key => {(jacobian,Ideal),(jacobian, MonomialIdeal)},
     Headline => "the Jacobian matrix of the generators of an ideal",
     Usage => "jacobian I",
     Inputs => {"I" => " in a polynomial ring"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the generators of ", TT "I"}},
     "This is identical to ", TT "jacobian generators I", ".  See ", TO (jacobian,Matrix), 
     " for more information.",
     	  EXAMPLE {
	       "R = QQ[x,y,z];",
      	       "I = ideal(y^2-x*(x-1)*(x-13))",
      	       "jacobian I",
	       },
	  "If the ring of ", TT "I", " is a polynomial ring over a polynomial ring,
	  then indeterminates in the coefficient ring are treated as constants.",
     	  EXAMPLE {
	       "R = ZZ[a,b,c][x,y,z]",
	       "jacobian ideal(a*y*z+b*x*z+c*x*y)"
	       }
     }
document {
     Key => (jacobian,Ring),
     Headline => "the Jacobian matrix of the polynomials defining a quotient ring",
     Usage => "jacobian R",
     Inputs => {"R" => " a quotient of a polynomial ring"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the presentation matrix of ", TT "R"}},
     "This is identical to ", TT "jacobian presentation R", ", except
     that the resulting matrix is over the ring ", TT "R", ".  See ", TO (jacobian,Matrix), 
     " for more information.",
     	  EXAMPLE {
	       "R = QQ[x,y,z]/(y^2-x^3-x^7);",
      	       "jacobian R",
	       },
	  "If the ring ", TT "R", " is a (quotient of a) polynomial ring over a polynomial ring,
	  then the top set of indeterminates is used, on the top set of quotients:",
     	  EXAMPLE {
	       "A = ZZ[a,b,c]/(a^2+b^2+c^2);",
	       "R = A[x,y,z]/(a*x+b*y+c*z-1)",
	       "jacobian R"
	       }
     }
