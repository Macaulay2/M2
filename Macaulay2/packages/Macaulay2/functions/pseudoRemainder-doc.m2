--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => pseudoRemainder,

     TT "pseudoRemainder(f,g)", " -- computes the pseudo-remainder for
     f divided by g.",
     PARA{},
     "This is an internal experimental routine."
     }

document { 
     Key => {pseudoRemainder,
	  (pseudoRemainder,RingElement,RingElement)
	  },
     Headline => "compute the pseudo-remainder",
     Usage => "pseudoRemainder(f,g)",
     Inputs => {"f", "g" },
     Outputs => {
	  RingElement => {"the pseudo remainder of the polynomial ", TT "f", " by the polynomial ", TT "g"}
	  },
     
     "description",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = x*y
	  g = x*y^2-3*x
	  topCoefficients matrix{{g}}
	  pseudoRemainder(f,g)
	  ///,
     Caveat => {},
     SeeAlso => {}
     }
