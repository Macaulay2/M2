--- status: DRAFT
--- author(s): ?
--- notes: 

document { 
     Key => {terms,(terms,RingElement)},
     Headline => "provide a list of terms of a polynomial",
     Usage => "terms f",
     Inputs => {
	  "f" => RingElement => {"in a polynomial ring ", TT "R", " with coefficient ring ", TT "A"},
	  },
     Outputs => {
	  {"the list of terms of ", TT "f"}
	  },
     "Each term is an element of the coefficient ring ", TT "A", ", multiplied with a  monomial in the variables of ", TT "R", ".",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "terms(a+d^2-1+a*b*c)"
	  },
     "In the situtation where the ring is a polynomial ring over another 
     polynomial ring, the polynomial is split using the monomials
     of the outer ring.",
     EXAMPLE {
	  "S = R[x,y];",
	  "terms(a*x+b*x+c*x*y+c*x^3+1+a)"
	  },
     SeeAlso => {coefficients, monomials, someTerms}
     }

