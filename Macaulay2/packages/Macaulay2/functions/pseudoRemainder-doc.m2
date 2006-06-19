--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {pseudoRemainder,
	  (pseudoRemainder,RingElement,RingElement)
	  },
     Headline => "compute the pseudo-remainder",
     Usage => "pseudoRemainder(f,g)",
     Inputs => {RingElement => "f", 
	  RingElement => "g" => {"in the same polynomial ring ", TT "R", " as ", TT "f"}},
     Outputs => {
	  RingElement => {"the pseudo remainder of the polynomial ", TT "f", " by the polynomial ", TT "g"}
	  },
     "Let ", TT "x", " be the first variable of ", TT "R", " appearing in ", 
     TT "g", ".  Suppose that ", TT "g", " has degree ", TT "d", "
     in ", TT "x", ", and that the coefficient of ", TT "x^d", " in ", 
     TT "g", " (as an element of ", TT "R", ", but not involving the variable ", 
     TT "x", ") is ", TT "c", ".
     The pseudo remainder of ", TT "f", " by ", TT "g", " is the polynomial ", 
     TT "h", " of degree less than ", TT "d", " in ", TT "x", " such that ",
     TT "c^(e-d+1) * f = q*g + h",
     ", where ", TT "f", " has degree ", TT "e", " in ", TT "x", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y];
	  f = x^4
	  g = x^2*y + 13*x^2*y^4 +x*y^2-3*x - 1
	  (lg, cg) = topCoefficients g
	  h = pseudoRemainder(f,g)
	  (cg^3 * f - h) % g
	  q = (cg^3 * f - h) // g
	  cg^3*f == h + q*g
	  ///,
     Caveat => {"There is no pseudo-division implemented, and the only way to change the notion
	  of what the top variable is, is to change to a ring where the variables are in a different order"},
     SeeAlso => {topCoefficients}
     }
