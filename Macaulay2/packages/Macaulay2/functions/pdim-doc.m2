--- status: DRAFT
--- author(s): 
--- notes: 

document { 
     Key => {(pdim),(pdim,Module), (pdim,CoherentSheaf)},
     Headline => "calculate the projective dimension",
     Usage => "pdim M",
     Inputs => {"M" => Module => " or a ", TO CoherentSheaf
	  },
     Outputs => {
	ZZ => ""  },
     "As an example, we compute the projective dimension of the module R/I.",    
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "I = ideal(x^2, x*y, y*z);",
	  "res(R^1/I)",
	  "pdim(R^1/I)"},
     "Notice this is one more than the projective dimension of I as an R-module.",
     EXAMPLE{
	  "res(module I)",
	  "pdim(module I)"
	  },
     Caveat => {   "For now, the method is to measure the length of a projective resolution."},
     SeeAlso => {}
     }

--   Consequences => {
--	  },
 -- doc8.m2:4:     Key => pdim,
