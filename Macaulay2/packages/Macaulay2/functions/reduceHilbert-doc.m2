--- status: DRAFT
--- author(s): L.Gold
--- notes:

document { 
     Key => {reduceHilbert, (reduceHilbert,Divide)},
     Headline => "remove common factors from a Hilbert series expression",
     Usage => "reduceHilbert H",
     Inputs => {
	  "H" => Divide => ""
	  },
     Outputs => {
	  Divide => {"the Hilbert series"}
	  },
     	  "This function reduces the rational function expression of a
	  Hilbert series produced by the function ", TO "hilbertSeries", 
	  "  by removing all common factors.",
          EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2;",
      	  "hilbertSeries I",
	  "reduceHilbert o3"	  
     	  }
     }
