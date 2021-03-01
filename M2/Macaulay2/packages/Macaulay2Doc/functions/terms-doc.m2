--- status: DRAFT
--- author(s): ?
--- notes: 

document { 
     Key => {terms,(terms,RingElement),(terms,Ring,RingElement)},
     Headline => "provide a list of terms of a polynomial",
     SYNOPSIS (
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
	  "In the situation where the ring is a polynomial ring over another 
	  polynomial ring, the polynomial is split using the monomials
	  of the outer ring.",
	  EXAMPLE {
	       "S = R[x,y];",
	       "terms(a*x+b*x+c*x*y+c*x^3+1+a)"
	       }
	  ),
     SYNOPSIS (
	  Usage => "terms(k,f)",
	  Inputs => {
	       "k" => Ring,
	       "f" => RingElement => {"in a polynomial ring ", TT "R"},
	       },
	  Outputs => {
	       {"the list of terms of ", TT "f", " with ", TT "k", " regarded as the coefficient ring" }
	       },
	  PARA {
	       "Each term is an element of the coefficient ring ", TT "k", ", multiplied with a monomial in the variables of ", TT "R", ".
	       This is useful in the situation where the polynomial ", TT "R", " is built from ", TT "k", " by a sequence of extensions."
	       },
	  EXAMPLE lines ///
	       R = QQ[a][d];
	       f = (1+a+d)^3
	       terms f
	       terms(QQ,f)
     	       ///
	  ),
     SeeAlso => {coefficients, monomials, someTerms}
     }

