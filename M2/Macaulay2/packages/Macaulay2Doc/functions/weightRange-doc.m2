document { 
     Key => {weightRange,(weightRange,RingElement),(weightRange,List,RingElement)},
     Headline => "the pair of lowest and highest weights of the monomials",
     SYNOPSIS (
	 Usage => "(lo,hi) = weightRange(w,f)",
	 Inputs => {
	      "w" => List => "of integers, the weight of each variable",
	      "f" => RingElement => "in a polynomial ring"
	      },
	 Outputs => {
	      "lo" => ZZ => {"the least weight of the monomials of ", TT "f"},
	      "hi" => ZZ => {"the greatest weight of the monomials of ", TT "f"}
	      },
	 "The weight of a monomial is the dot product of w and the exponent vector.
	 If the weight vector has length smaller than the number of variables,
	 the other variables are assumed to have weight zero.  If there are too many weights
	 given, the extras are silently ignored.",
	 EXAMPLE {
	      "R = QQ[a..g]",
	      "f = a^3+b^2*c+3*f^10*d-1+e-e",
	      "weightRange({1,1,0,0,0,0,0},f)",
	      },
	 "Use  ", TO terms, " and ", TO weightRange, " together to select the terms
	 which have a given weight.",
	 EXAMPLE {
	      "f = a^2*b+3*a^2*c+b*c+1",
	      "sum select(terms f, t -> (weightRange({1,0},t))#0 == 2)"
	      },
	 "If the coefficient ring is a polynomial ring, one can use the weights
	 of these variables too.  The order of weights is the same as the order of variables (see ", TO index, ")",
	 EXAMPLE {
	      "S = R[x,y];",
	      "weightRange({0,0,3,7},a*x^2+b*x*y)"
	      },
	 ),
     SYNOPSIS (
     	  Usage => "(lo,hi) = weightRange f",
     	  Inputs => {
	       "f" => RingElement
	       },
	  Outputs => {
	       "lo" => ZZ => {"the minimum value for (the first component of) the degrees
	       	    of the monomials occurring in ", TT "f" },
	       "hi" => ZZ => {"the corresponding maximum value" }
	       },
	  EXAMPLE lines ///
	       R = QQ[x,y];
	       weightRange (x^3+y^2)^5
	  ///
	  ),
     SeeAlso => {degree, homogenize, part, index}
     }
