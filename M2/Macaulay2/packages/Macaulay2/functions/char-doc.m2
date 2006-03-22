--- status: DRAFT
--- author(s): kummini
--- notes: 

document { 
	Key => {char, (char,Ring)},
	Undocumented => {
	     (char,FractionField), 
	     (char,QuotientRing),
	     (char,PolynomialRing)},
     Headline => "computes the characteristic of a field or ring",
     Usage => "char F",
     Inputs => {
	   	"F" => Ring => ""
	  },
     Outputs => {
		"the characteristic of the ring"
	  },
     EXAMPLE {
		"R = ZZ/10007[x,y];",
		"char R",
	  },
     }
