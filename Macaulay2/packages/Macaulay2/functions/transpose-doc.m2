--- status: DRAFT
--- author(s): kummini
--- notes: 

document { 
     Key => transpose,
     Headline => "transpose a table or a matrix ", 
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (transpose,Matrix),
     Headline => " transposes a matrix",
     Usage => "transpose f",
     Inputs => {
		"f" => Matrix => ""
	  },
     Outputs => { 
		{"the tranpose of the matrix ", TT "f", "."}
	  },
     "description",
     EXAMPLE {
		"S = ZZ/10007[x,y];",
		"M = matrix{{x^3,x*y^2},{y*x^2,y^3}}",
		"transpose M",
	  },
     }
document { 
     Key => (transpose,List),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (transpose,ChainComplexMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
