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
     	  "This function is used to reduce the rational expression
	  given by the command", TO "hilbertSeries"," because it is
	  not automatically reduced.",
          EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
	  "I = ideal x^2;",
      	  "s = hilbertSeries I",
	  "reduceHilbert s"	  
     	  }
     }
