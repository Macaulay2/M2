-- This file written by Amelia Taylor <ataylor@math.rutgers.edu>

document{(minPres,Ring),
     Headline => "compute a minimal presentation of a quotient ring",
     Synopsis => {
	  Usage => "Q = minPres R",
	  Inputs => {
	       "R" => {"a quotient ring of the form ", TT "R = S/I"}
	       },
	  Outputs => {
	       "Q" = {"an minimally presented isomorphic quotient ring ", TT "S'/J"}
	       }
	  },
     PARA "The function ", TT "minPres", " is written so that it stores 
     the map from ", TT "R", " to ", TT "Q", ".  The map is stored 
     as ", TT "R.minPresMap", ".  The inverse of this map is stored 
     as ", TT "R.minPresMapInv", ".",
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres(C)",
	  "C.minPresMap",
	  "C.minPresMapInv"
	  }
     }


-- I commented this out because minPres doesn't actually accept this option (drg)

-- document { (minPres,VarName),
--      Headline=> "Rename the variables in the ring given so that the minimal 
--      presentation variables are named differently than those in the original 
--      ring."
--      }

document{(minPresIdeal,Ideal),
     Headline => "compute a minimal presentation of an ideal",
--      Usage => {
-- 	  TT "minPresIdeal I", "-- If ", TT "R", " is the ring of ", TT "I", 
-- 	  " then computes a minimal presentation of ", TT "R/I", " and 
-- 	  returns and ideal ", TT "J", " such that ", TT "R/J", " is 
-- 	  isomorphic to ", TT "R/I", "."
--     	  },
     Synopsis => {
	  Usage => "J = minPresIdeal I",
	  Inputs => {
	       "I" => {"Any ideal in a polynomial ring."}
	       },
	  Outputs => {
	       "J" => {"An ideal such that ", TT "R/J", " is isomorphic 
	       	    to ", TT "R/I", " and ", TT "J", " has fewer generators 
	       	    than ", TT "I", "."}
	       },
	  },
     -- this documentation is wrong, because the ambient ring changes, too!
     PARA {
	  "The function ", TT "minPresIdeal", " is written so that it stores 
	  the map from ", TT "R/I", " to ", TT "R/J", ".  The map is stored 
	  as ", TT "I.cache.minPresMap", " , where ", TT "I", " is the input
	  ideal.  Similarly the inverse of this map is stored 
	  as ", TT "I.cache.minPresMapInv", ".  There are times when it is 
	  advantageous to have the input be an ideal as opposed to the 
	  quotient ring."},
     SEEALSO {"minPres"},
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w];",
	  "I = ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPresIdeal I",
	  "I.cache.minPresMap",
	  "I.cache.minPresMapInv"
	  }
     }

-- I commented this out because minPres doesn't actually accept this option (drg)

-- document { (minPresIdeal,VarName),
--      Headline=> "Rename the variables in the ring of the ideal given so 
--      that the variables for the minimal presentation are named differently 
--      than the variables in the original ring."
--      }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
