--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {terms,(terms,RingElement)},
     Headline => "provide a list of terms of a polynomial",
     Usage => "terms f",
     Inputs => {
	  "f" => "in a polynomial ring"
	  },
     Outputs => {
	  {"the list of terms of ", TT "f"}
	  },
     "The polynomial ", TT "f", " is split into its terms",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "terms(a+d^2-1+a*b*c)"
	  },
     "In the situtation where the ring is a polynomial ring over another 
     polynomial ring, the polynomial is split using the monomials
     of the outer ring.",
     EXAMPLE {
	  "S = R[x,y];",
	  "terms(a*x+b*y+c*x*y+c*x^3+1+a)"
	  },
     SeeAlso => {coefficients, monomials, someTerms}
     }

