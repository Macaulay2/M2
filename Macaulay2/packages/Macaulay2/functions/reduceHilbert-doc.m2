--- status: DRAFT
--- author(s): L.Gold
--- notes:

document { 
     Key => {reduceHilbert, (reduceHilbert,Divide)},
     Headline => "reduce a Hilbert series expression",
     Usage => "reduceHilbert H",
     Inputs => {
	  "H" => Divide => ""
	  },
     Outputs => {
	  Divide => {"the Hilbert series reduced by removing common factors"}
	  },
          EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2;",
      	  "hilbertSeries I",
	  "reduceHilbert o3"	  
     	  }
     }
