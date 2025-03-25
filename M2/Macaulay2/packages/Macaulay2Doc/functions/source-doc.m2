--- status: DRAFT
--- author(s):  kummini
--- notes: 

document { 
     Key => source,
     Headline => "source of a map",
     "Gives the source of a map of rings, chain complexes or modules",
     SeeAlso => {target}
     }
document { 
     Key => (source,RingMap),
     Headline => "find the source ring for a map of rings",
     Usage => "source f",
     Inputs => {
		"f"
	  },
     Outputs => {
		"the source ring of the map"
	  },
     EXAMPLE {
		"S = ZZ/10007[x, y, z];",
		"R = ZZ/10007[t];",
		"f = map(R,S,{t^3,t^4,t^5})",
		"source f"
	  },
     }
document { 
     Key => (source,ChainComplexMap),
     Headline => "find the source of a map of chain complexes",
     Usage => "source f",
     Inputs => {
		"f" => ChainComplexMap
	  },
     Outputs => {
		{"the source chain complex of ", TT "f"}
	  },
	"In the example below, we have a map between two modules and extend it to
	a map between projective resolutions of the two modules. Then ", 
	TT "source",  " gives the source of the map of chain complexes.",
     EXAMPLE {
	  "R = ZZ[x,y,z];",
	  "M = R^1/(x,y,z);",
	  "N = R^1/(x^2,y^2,x*y*z,z^2);",
	  "g = map(N,M,x*y);",
	  "f = res g;",
	  "source f"
	  },
	"(That was an expensive way of resolving ", TT "M", ".)",
     }

document { 
     Key => (source,Matrix),
     Headline => "find the source module of matrix",
     Usage => "source f",
     Inputs => {
		"f" => Matrix
	  },
     Outputs => {
		{"the source module of ", TT "f"}
	  },
     EXAMPLE {
	  "R = ZZ[x,y,z];",
	  "M = R^1/(x,y,z);",
	  "N = R^1/(x^2,y^2,x*y*z,z^2);",
	  "g = map(N,M,x*y);",
	  "source g",
	  },
     }

document { 
     Key => (source,GradedModuleMap),
     Headline => "find the source of a map of graded modules",
     Usage => "source f",
     Inputs => {
		"f"
	  },
     }
