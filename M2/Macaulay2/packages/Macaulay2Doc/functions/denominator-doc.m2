--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented { (denominator,QQ) }

document {
     Key => {denominator, (denominator,Divide)},
     Headline => "denominator of a fraction",
     Usage => "denominator x",
     Inputs => {
	  "x" => "a fraction"
	  },
     Outputs => {
	  {"the denominator of ", TT "x"}
	  },
     EXAMPLE "denominator (4/6)",
     PARA{},
     EXAMPLE {
	  "R = frac(ZZ[x,y]);",
	  "denominator((x+2*y-3)/(x-y))"
	  },
     PARA{},
     TT "denominator", " also works with Hilbert series.",
     EXAMPLE {
	  "R = QQ[a..d]/(a^2,b^2,c^3);",
	  "hf = hilbertSeries R",
	  "denominator hf"
	  },
     PARA {
	  "For a Laurent polynomial in a ring with inverses of variables, it gives the monomial needed to clear
	  all the denominators in each of the terms."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z,Inverses => true, MonomialOrder => Lex]
     denominator (x*y^-1+y*z^-2+1+y^-1*z^-1)
     ///,
     SeeAlso => {
	  numerator,
	  "fraction fields",
	  hilbertSeries
	  }
     }

