--- status: Reviewed
--- author(s): kummini
--- notes: 

document { 
     Key => {
	 transpose,
	(transpose, Matrix),
	(transpose, MutableMatrix),
	(transpose, List),
    },
     Headline => "transpose a matrix or table",
     Usage => "transpose f",
     Inputs => {
		"f" => { ofClass {Matrix, List}, " which is a table" }
	  },
     Outputs => {
	  Matrix => { "the transpose of ", TT "f" }	       
	  },
     "Here is an example.",
     EXAMPLE {
		"S = ZZ/10007[x,y,z];",
		"f = matrix{{x^3,x*y^2,x},{y*x^2,y^3,y}}",
		"entries f",
		"g = transpose f",
		"transpose entries f"
		},
	"The output of ", TT "transpose", " is a map between the duals of the
	original source and target free modules. See:",
	EXAMPLE {
		   "degrees f",
		   "degrees g",
	  },
      Caveat => {
		    TT "transpose", " works only for maps between free modules. 
		    Use ", TT "dual", " for more general maps."
	 },
      SeeAlso => {dual}
     }
document { 
     Key => (transpose,ChainComplexMap),
     Headline => "transpose a map of chain complexes",
     Usage => "transpose f",
     Inputs => {
		"f" => ChainComplexMap
	  },
     Outputs => {
	  ChainComplexMap => { "the transpose of ", TT "f" }	       
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
