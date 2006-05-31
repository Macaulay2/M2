--- status: DRAFT
--- author(s): M2Fest2005 DE
--- notes: Mike will make this work for rings

document { 
     Key =>{binomial, (binomial,ZZ,ZZ)},
	Headline => "binomial coefficient",
	Usage => "binomial(n,k)",
	Inputs => {
		"n" => ZZ,
		"k" => ZZ => ", must be non-negative"
		},
	Outputs => {{"the binomial coefficient, the coefficient of ", TT "x^k", " in ", TT "(1+x)^n"}
		},
     EXAMPLE {"binomial(13,6)",
	      "binomial(-1,3)"},
     Caveat => {"Doesn't do polynomials, things like ", TT "binomial(x,3)", 
	             " where ", TT "x", " is an element of a ring other than", TT " ZZ"},
     SeeAlso => {}
     }
