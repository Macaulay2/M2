--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented { (numerator,QQ) }

document {
     Key => {numerator, (numerator,Divide)},
     Headline => "numerator of a fraction",
     Usage => "numerator x",
     Inputs => {
	  "x" => "a fraction"
	  },
     Outputs => {
	  {"the numerator of ", TT "x"}
	  },
     EXAMPLE "numerator (4/6)",
     PARA{},
     EXAMPLE {
	  "R = frac(ZZ[x,y]);",
	  "numerator((x+2*y-3)/(x-y))"
	  },
     PARA{},
     TT "numerator", " also works with Hilbert series.",
     EXAMPLE {
	  "R = QQ[a..d]/(a^2,b^2,c^3);",
	  "hf = hilbertSeries R",
	  "numerator hf"
	  },
     PARA {
	  "For a Laurent polynomial in a ring with inverses of variables, it gives the result after clearing
	  all the denominators in each of the terms by multiplying by a suitable monomial."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z,Inverses => true, MonomialOrder => Lex]
     numerator (x*y^-1+y*z^-2+1+y^-1*z^-1)
     ///,
     SeeAlso => {
	  denominator,
	  "fraction fields",
	  hilbertSeries
	  }
     }
