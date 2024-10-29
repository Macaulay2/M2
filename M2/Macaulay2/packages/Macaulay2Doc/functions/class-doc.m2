--- status: DRAFT
--- author(s): MES, taken from Dan's.
--- notes: 

document { 
     Key => {class, (class, Thing)},
     Headline => "class of an object",
     Usage => "class x",
     Inputs => {
	  "x" => Thing => "any Macaulay2 object"
	  },
     Outputs => {
	  Type => {"the class of ", TT "x"}
	  },
     "Every object in Macaulay2 has a class, see ", TO "what a class is", " for more details.",
     EXAMPLE {
	  "class {1,2,3}",
	  "R = QQ[a..d]",
     	  "class R"
	  },
     "Use strict equality ", TT "===", " to test the exact class
     of an object.",
     EXAMPLE {
	  "class R === PolynomialRing",
	  "class R === Ring"
	  },
     "However, each PolynomialRing is a Ring, by inheritance, so a more
     useful test uses ", TO instance, ".",
     EXAMPLE {
	  "instance(R,Ring)"
	  },
     SeeAlso => {instance}
     }
