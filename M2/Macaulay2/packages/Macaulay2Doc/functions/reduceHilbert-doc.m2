--- status: DRAFT
--- author(s): L.Gold
--- notes: needs good example, maybe: 
---        dim of scheme is degree of reduced denominator

document { 
     Key => {reduceHilbert, (reduceHilbert,Divide)},
     Headline => "reduce a Hilbert series expression",
     Usage => "reduceHilbert H",
     Inputs => {
	  "H" => Divide
	  },
     Outputs => {
	  Divide => {"the Hilbert series reduced by removing common factors"}
	  },
     PARA {
     	  "This function is used to reduce the rational expression
	  given by the command ", TO "hilbertSeries", 
	  ". It is not automatically reduced, but sometimes it is
	  useful to write it in reduced form. For instance, one might
	  not notice that the series is a polynomial until it is
	  reduced."
	  },
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2;",
      	  "s = hilbertSeries I",
	  "reduceHilbert s"	  
     	  },
     PARA {
	  "The reduction is partial, in the sense that the explicit factors of
	  the denominator are cancelled entirely or not at all."
	  },
     EXAMPLE lines ///
     M = R^{0,-1}
     hilbertSeries M
     f = reduceHilbert oo
     gcd( value numerator f, value denominator f )
     ///
     }
