--Documentation and tests for the functions normalization and isNormal.
document { (isNormal,Ring),
     Headline => "determine if a reduced ring is normal",
     Usage => {
	  TT "isNormal R", "-- determine if a ring ", TT "R", " is normal, 
	  i.e. if both of the Serre conditions R1 and S2 hold."
	  },
     Synopsis => {
	  "S = isNormal R",
	  "R" => {"reduced ring"},
	  "S" => {"returns true if ", TT "R", " is R1 and S2 and false 
	           otherwise."},
	  }
     }
document { integralClosure,
     Headline => "compute the integral closure of a ring",
     SEEALSO {"ICMap", "ICFractions", "conductor"}
     }

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
	  "S" => {"a minimal presentation of the integral closure of ", 
	       TT "R", "in its total ring of fractions where the new 
	       variables are 
	       indexed variables using ", TT "w", "."}
	  },
     PARA, "The function ", TO "integralClosure", " was written 
     so that certain information can be retrieved if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in the presentation of 
     the integral closure that is the output of the function.  
     The map and the corresponding fractions can be obtained by
     typing ", TT "R.IC.fractions", " where R is the the affine 
     domain given as input into ", TT "integralClosure", ".",  
     SEEALSO {"conductor"},
     "The function ", TT "integralClosure", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", TT "ataylor@math.rutgers.edu", "."
     }
    
document{ integralClosure => VarName,
     Headline=> "choose the name of the new variables introduced in computing 
     the integral closure of an affine domain."
     }

document { (ICmap,Ring),
     Headline => "compute the map from a reduced ring to its integral closure.",
     Usage => {
	  TT "ICmap R", "-- compute the map from a reduced ring", TT "R", " to 
	  it's integral closure."
	  },
     Synopsis => {
	  "F = ICmap R",
	  "R" => {"reduce ring"},
	  "S" => {"returns a map from ", TT "R", " to its integral closure"},
	  },
     }

document { (ICfractions,Ring),
     Headline => "Compute the fractions integral over a reduced ring.",
     Usage => {
	  TT "ICfractions R", "-- compute the fractions in the fraction
	  field of ", TT "R", " that generate the integral closure of ", TT "R", 
	  " over R.  The set may not be minimal."
	  },
     Synopsis => {
	  "M = ICfractions R",
	  "R" => {"reduced ring"},
	  "M" => {"returns a matrix of fractions that generate the integral
	       closure of ", TT "R", " over R."},
	  },
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

	  

