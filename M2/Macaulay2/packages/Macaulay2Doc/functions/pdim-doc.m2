--- status: DRAFT
--- author(s): L.Gold, Josephine, Jonah
--- notes: 

document { 
     Key => pdim,
     Headline => "calculate the projective dimension",
     Usage => "pdim M",
     Caveat => { "For now, the method is to measure the length of a projective resolution."},
     SeeAlso => {}
     }

document { 
     Key => (pdim, Module),
     Headline => "calculate the projective dimension of a module",
     Usage => "pdim M",
     Inputs => {
	  "M" => Module
	  },
     Outputs => {
	ZZ => "the projective dimension" 
	},
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "I = ideal(x^2, x*y, y*z);",
	  "M = R^1/I",
	  "res M",
	  "pdim M"},
     "Notice this is one more than the projective dimension of I as an R-module.",
     EXAMPLE{
	  "res(module I)",
	  "pdim(module I)"
	  }
     }

document { 
     Key => (pdim,CoherentSheaf),
     Headline => "calculate the projective dimension",
     Usage => "pdim S",
     Inputs => {
	  "S" => CoherentSheaf
	  },
     Outputs => {
	ZZ => "the projective dimension" 
	},
     EXAMPLE {
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	  "pdim S"}
     }
