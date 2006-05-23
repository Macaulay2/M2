--- status: DRAFT
--- author(s): kummini 
--- notes: 

document { 
     Key => (symbol %, RingElement, Ideal),
	Headline => "calculate the normal form.",
     Usage => "f % I",
     Inputs => { "f" => "" , "I" => ""},
	Outputs => {  },
	"To reduce f with respect to I, a Groebner basis of I is computed, unless
	it has already been done. Then the element is reduced using the division
	algorithm.",
	EXAMPLE {
	  "R = ZZ/1277[x,y];",
    	  "I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);",
	  "(x^3 - 2*x) % I",
	  "(x^3) % I"
     },
	SeeAlso => "Groebner bases",
	}
