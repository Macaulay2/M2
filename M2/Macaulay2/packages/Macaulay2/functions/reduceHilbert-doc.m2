--- status: DRAFT
--- author(s): L.Gold
--- notes: may be something wrong with this...see bugs-lgold

document { 
     Key => {reduceHilbert, (reduceHilbert,Divide)},
     Headline => "remove common factors from a Hilbert series expression",
     Usage => "reduceHilbert H",
     Inputs => {
	  "H" => Divide => ""
	  },
     Outputs => {
	  Divide => {"the Hilbert series as a rational function with common factors removed"}
	  },
          EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2;",
      	  "hilbertSeries I",
	  "reduceHilbert o3"	  
	  }
     }
