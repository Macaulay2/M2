--- status: DRAFT
--- author(s): kummini
--- notes: 

document { 
     Key => transpose,
     Headline => "transpose a table or a matrix", 
--      Usage => "",
--      Inputs => {
-- 	  },
--      Outputs => {
-- 	  },
--      Consequences => {
-- 	  },     
	 "The function ", TT "transpose", " transposes a matrix, a map
	 or a table.",
--      EXAMPLE {
-- 	  },
--      Caveat => {},
--      SeeAlso => {}
     }
document { 
     Key => (transpose,List),
     Headline => " transposes a table",
     Usage => "transpose T",
     Inputs => {
		"T" => List => " or a table"
	  },
     Outputs => { 
		{"the tranpose of the table ", TT "T", "."}
	  },
     "Example:",
     EXAMPLE {,
		"transpose{{a, b, c},{d, e, f}}"
	}
     }

document { 
     Key => (transpose,Matrix),
     Headline => "transpose a matrix",
     Usage => "transpose f",
     Inputs => {
		"f" => Matrix => ""
	  },
     Outputs => {
	  },
     "Example:",
     EXAMPLE {
		"S = ZZ/10007[x,y,z];",
		"f = matrix{{x^3,x*y^2},{y*x^2,y^3}}",
		"g = transpose f",
		},
	"The output of ", TT "transpose", " is a map between the duals of the
	original source and target free modules. See:",
	EXAMPLE {
		   "degrees f",
		   "degrees g",
	  },
     }
document { 
     Key => (transpose,ChainComplexMap),
     Headline => "transpose a map of chain complexes",
     Usage => "transpose f",
     Inputs => {
		"f" => ChainComplexMap => ""
	  },
     Outputs => {
	  },
	"The output of ", TT "transpose", " is a map from the duals of the
	original source and target free modules. See the degree of the target
	module in the following example",
	EXAMPLE {
		   "S = ZZ/10007[x,y,z];",
		   "F = res ideal vars S;",
		   "F.dd",
		   "transpose F.dd"
	  },
	"Note that ", TT "M2", " treats the differentials of a chain complex map
	as map of degree -1."
	}
