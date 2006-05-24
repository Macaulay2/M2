-- This file written by Amelia Taylor <ataylor@math.rutgers.edu>

--Documentation and tests for the functions normalization and isNormal.
document {
     Key => isNormal, Headline => "determine whether a reduced ring is normal" }
document {
     Key => (isNormal,Ring),
     Usage => "isNormal R",
     Inputs => {
	  "R" => {"a reduced ring"}
	  },
     Outputs => {
	  {"whether ", TT "R", " is normal, i.e., whether the Serre conditions R1 and S2 hold"}
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "isNormal R",
	  "S = ZZ/101[a_7,a_6,x,y,z]/ideal(x^2-a_6*z,a_6*x-a_7*z,a_6^2-a_7*x,a_7^2-y^2-z^2)",
	  "isNormal S",
	  },
     PARA {"In the example above ", TT "S", " is the integral closure 
     	  of ", TT "R", " given in the form ", TT "T/J", " where T is a polynomial 
     	  ring and J is an ideal."
	  }
     }
document {
     Key => integralClosure,
     Headline => "compute the integral closure of a ring",
     SeeAlso => {"ICmap", "ICfractions", "conductor"}
     }

document {
     Key => (integralClosure,Ring),
     Headline => "compute the integral closure of a ring",
     Usage => "S = integralClosure R",
     Inputs => {
	  "R" => {"a reduced ring"},
	  Variable => {"an unassigned symbol"},
	  },
     Outputs => {
	  "S" => {
	       "The integral closure of ", TT "R", " in its total ring of fractions, presented as a quotient ring 
	       using new indexed variables based on the symbol ", TT "w"}
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "S = integralClosure (R)"
	  },
     PARA{},
     "The code for this function was written 
     so that certain information can be retrieved if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in this description of 
     the integral closure.  Unfortunately, all of the added features 
     currently only work on affine domains.
     The map and the corresponding fractions are obtained as 
     a matrix using the function ", TO (ICfractions,Ring), " where R is 
     an affine domain.  This function can be run without first 
     using ", TT "integralClosure", ".  The natrual map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "ICmap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (ICmap R)", ".  Note that 
     both ", TT "ICfractions", " and ", TT "ICmap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TT "integralClosure", ".  In this way you can use these 
     functions without running ", TT "integralClosure", ".",
     SeeAlso => {"conductor"},
     "The function ", TT "integralClosure", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", HREF {"mailto:ataylor@math.rutgers.edu", "<ataylor@math.rutgers.edu>"}, "."
     }
    
document {
     Key => [integralClosure,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the integral closure of a reduced ring."
     }

document {
     Key => {ICmap, (ICmap,Ring)},
     Headline => "natural map from an affine domain into its integral closure.",
     Usage => "ICmap R",
     Inputs => {
	  "R" => {"an affine domain"}
	  },
     Outputs => {
	  {"a map from ", TT "R", " to its integral closure"}
	  },
     "Note that if an integrally closed ring is given as input, the identity map from 
     the ring to itself is returned.",
     	  EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "ICmap R"
	  }
     }

document {
     Key => {ICfractions, (ICfractions,Ring)},
     Headline => "Compute the fractions integral over a domain.",
     Usage => "ICfractions R",
     Inputs => {
	  "R" => {"an affine domain"},
	  },
     Outputs => {
	  {"returns a matrix whose entries are fractions that generate the integral closure of ", TT "R", " over R."}
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

document {
     Key => {conductor,(conductor,RingMap)},
     Headline => "compute the conductor of a finite ring map",
     Usage => "conductor F",
     Inputs => {
	  "F" => {"a finite map from a ring ", TT "R", " to a ring ", TT "S"},
	  },
     Outputs => {
	  {"the conductor ideal of ", TT "S", " into ", TT "R", "."}
	  },
     "Suppose that the ring map F : R --> S is finite: i.e. S is a finitely 
     generated R-module.  The conductor of F is defined to be {",
     TEX "g \\in R \\mid g S \\subset f(R)", "}.  One way to think
     about this is that the conductor is the set of universal denominators
     of ", TT "S", " over ", TT "R", ", or as the largest ideal of ", TT "R", " 
     which is also an ideal in ", TT "S", ".",
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "F = ICmap R",
	  "conductor F"
	  },
     PARA{},
     "The command ", TT "conductor", " calls the 
     command ", TO pushForward, ".  Currently, the 
     command ", TT "pushForward", 
     " does not work if the source of the map ", TT "F", " is multgraded 
     or inhomogeneous.  If the source of the map ", TT "F", " is multigraded 
     or in homogeneous ", TT "conductor", " returns the message -- No conductor
     for ", TT "F", ".",
     SeeAlso =>{"pushForward", "integralClosure"} 
     }


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
