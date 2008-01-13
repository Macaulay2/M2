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
     SeeAlso => {
	  numerator,
	  "fraction fields",
	  hilbertSeries
	  }
     }

