-- This file written by Amelia Taylor <ataylor@math.rutgers.edu>

--Documentation and tests for the functions normalization and isNormal.
document { (isNormal,Ring),
     Headline => "determine if a reduced ring is normal",
     Usage => {
	  TT "isNormal R", "-- determine if a ring ", TT "R", " is normal, 
	  i.e. if both of the Serre conditions R1 and S2 hold."
	  },
     Synopsis => {
	  "isNormal R",
	  "R" => {"reduced ring"},
	  {"returns true if ", TT "R", " is R1 and S2 and false 
	       otherwise."},
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "isNormal R",
	  "S = ZZ/101[a_7,a_6,x,y,z]/ideal(x^2-a_6*z,a_6*x-a_7*z,a_6^2-a_7*x,a_7^2-y^2-z^2)",
	  "isNormal S",
	  },
     PARA "In the example above ", TT "S", " is the Integral Closure 
     of ", TT "R", " given in the form ", TT "T/J", " where T is a polynomial 
     ring and J is an ideal."
     }
document { integralClosure,
     Headline => "compute the integral closure of a ring",
     SEEALSO {"ICMap", "ICFractions", "conductor"}
     }

---------  Take out the w and do options as Variable - where??
document { (integralClosure,Ring),
     Headline => "compute the integral closure of a ring",
     Usage => {
	  TT "integralClosure(R)", "-- compute the integral 
	  closure of a reduced ring ", TT "R", "in its total ring of 
	  fractions using additional variables indexed by ", TT "w", "."
	  },
     Synopsis => {
	  "S = integralClosure (R)",	
	  "R" => {"a reduced ring"},
	  "w" => {"an unassigned symbol"},
	  "S" => {"The integral closure of ", 
	       TT "R", "in its total ring of fractions given as a quotient ring 
	       using new variables are indexed variables using ", TT "w", "."}
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "S = integralClosure (R)"
	  },
     PARA, "The function ", TO "integralClosure", " was written 
     so that certain information can be retrieved if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in this description of 
     the integral closure.  Unfortunately, all of the added features 
     currently only work on affine domains.
     The map and the corresponding fractions are obtained as 
     a matrix using the function", TO "ICfractions R", " where R is 
     an affine domain.  This function can be run without first 
     using ", TT "integralClosure", ".  The natrual map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "ICmap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (ICmap R)", ".  Note that 
     both ", TT "ICfractions", " and ", TT "ICmap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TT "integralClosure", " in this way you can use these 
     functions without running ", TT "integralClosure", ".",
     SEEALSO {"conductor"},
     "The function ", TT "integralClosure", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", TT "ataylor@math.rutgers.edu", "."
     }
    
document{ integralClosure => Variable,
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the integral closure of a reduced ring."
     }

document { (ICmap,Ring),
     Headline => "natural map from an affine domain into its integral closure.",
     Usage => {
	  TT "ICmap R", "-- compute the natural map from an affine 
	  domain", TT "R", " into it's integral closure."
	  },
     Synopsis => {
	  "F = ICmap R",
	  "R" => {"affine domain"},
	  "S" => {"returns a map from ", TT "R", " to its integral closure"},
	  },
     "Note that if an integrally closed ring is given as input a map from 
     the ring to itself is returned.",
     	  EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "ICmap R"
	  }
     }

document { (ICfractions,Ring),
     Headline => "Compute the fractions integral over a domain.",
     Usage => {
	  TT "ICfractions R", "-- compute the fractions in the fraction
	  field of ", TT "R", " that generate the integral closure of ", TT "R", 
	  " over R"
	  },
     Synopsis => {
	  "M = ICfractions R",
	  "R" => {"affine domain"},
	  "M" => {"returns a matrix of fractions that generate the integral
	       closure of ", TT "R", " over R."},
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "S = ICfractions R",
	  "integralClosure(R,Variable => a)"
	  },
     "Thus the new variables a_7 and a_6 correspond to the 
     fractions respectively.  The program currently also returns the original 
     variables as part of the matrix.  In this way the user can see if any are 
     simplified out of the ring during the process of computing the integral
     closure."
     }
------------------------------------------------------------------------------

document{(conductor,RingMap),
     Headline => "compute the conductor of a finite ring map",
     Usage => {
	  TT "conductor F", "-- find the conductor of ", TT "S", 
	  " into ", TT "R", " where ", TT "S", " is a finitely 
	  generated ", TT "R", " module via the ring map ", TT "F", "."
    	  },
     Synopsis => {
	  "C = conductor F",
	  "F" => {"A finite ring map from a ring ", TT "R", 
	       " to a ring ", TT "S", "."},
	  "C" => {"the conductor ideal of ", TT "S", " into ", TT "R", "."},
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "conductor(ICmap R)"
	  },
     PARA, "The command ", TT "conductor", " calls the 
     command ", TT "pushForward", ".  Currently, the 
     command ", TT "pushForward", 
     " does not work if the source of the map ", TT "F", " is multgraded 
     or inhomogeneous.  If the source of the map ", TT "F", " is multigraded 
     or in homogeneous ", TT "conductor", " returns the message -- No conductor
     for ", TT "F", ".",
     SEEALSO{"pushForward", "integralClosure"} 
     }

------------------------------------------------------------------------------

	  

