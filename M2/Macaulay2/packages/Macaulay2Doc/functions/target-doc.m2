-- -*- coding: utf-8 -*-
--- status: DRAFT.
--- author(s):  kummini
--- notes: 

document { 
     Key => target,
     Headline => "target of a map",
     "This command returns the target of a morphism or Gröbner basis.",
     SeeAlso => {source}
     }
document { 
     Key => (target,GroebnerBasis),
     Headline => "find target of a Gröbner basis",
     Usage => "target g",
     Inputs => {
		"g" => GroebnerBasis
	  },
     Outputs => {
		"the target of the matrix given by the generators of the Gröbner
		basis"},
     EXAMPLE {
		   "R = ZZ/10007[x,y,z];",
		   "g = gb ideal(x^2, x*y-z^2, z^5);",
		   "target g"
	  },
     SeeAlso => {}
     }
document { 
     Key => {(target,Matrix), (target,MutableMatrix)},
     Headline => "find the target module of matrix",
     Usage => "target f",
     Inputs => {
		"f" => Matrix
	  },
     Outputs => {
		{"the target module of ", TT "f"}
	  },
     EXAMPLE {
	  "R = ZZ[x,y,z];",
	  "M = R^1/(x,y,z);",
	  "N = R^1/(x^2,y^2,x*y*z,z^2);",
	  "g = map(N,M,x*y);",
	  "target g",
	  },
     }
document { 
     Key => (target,RingMap),
     Headline => "find the target ring for a map of rings",
     Usage => "target f",
     Inputs => {
		"f"
	  },
     Outputs => {
		"the target ring of the map"
	  },
     EXAMPLE {
		"S = ZZ/10007[x, y, z];",
		"R = ZZ/10007[t];",
		"f = map(R,S,{t^3,t^4,t^5})",
		"target f"
	  },
     }
