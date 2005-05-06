--- status: TODO
--- author(s): Amelia Taylor
--- notes: 

document { 
     Key => minPresMap,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (minPresMap,Ring),
     Headline => "the map used to return a minimal presentation 
     of a ring",
     Usage => "minPresMap R",
     Inputs => {
	  "R" => ""
	  },
     Outputs => {
	  RingMap => {"the isomorphism from R to its minimal presentation."}
	  },
     Consequences => {
	  },     
     EXAMPLE {
	  "R = ZZ/101[x,y,z,u]/ideal(x - y^2 + z - z^2, u + x*z, x^2 - u^2);"
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (minPresMap,Ideal),
     Headline => "the map used to return a minimal presentation of a 
     the quotient ring from the ideal given",
     Usage => "minPresMap I",
     Inputs => {
	  "I" => ""
	  },
     Outputs => {
	  RingMap => {"the isomorphism from R to its minimal presentation."}
	  },
     Consequences => {
	  },     
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }


