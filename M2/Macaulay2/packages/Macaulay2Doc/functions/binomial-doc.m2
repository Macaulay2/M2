--- status: DRAFT
--- author(s): M2Fest2005 DE
--- notes: Mike will make this work for rings

document { 
     Key =>{binomial, (binomial,ZZ,ZZ), (binomial,RingElement,ZZ)},
	Headline => "binomial coefficient",
	Usage => "binomial(n,k)",
	Inputs => {
		"n" => {ofClass{ZZ,RingElement}},
		"k" => ZZ => ", must be non-negative"
		},
	Outputs => {{"the binomial coefficient, the coefficient of ", TEX "$x^k$", " in ", TEX "$(1+x)^n$",
		" or ", TEX "$n(n-1)...(n-k+1)/k!$" }
		},
     EXAMPLE {"binomial(13,6)",
	      "binomial(-1,3)"},
     SeeAlso => {}
     }
