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
     SeeAlso => {
	  denominator,
	  "fraction fields",
	  hilbertSeries
	  }
     }
