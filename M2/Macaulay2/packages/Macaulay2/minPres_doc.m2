document{
     Key => (minPres,Ring),
     Headline => "compute a minimal presentation of a quotient ring",
     Usage => {
	  TT "Q = minPres R", "-- If ", TT "S", " is a polynomial ring 
	  and ", TT "I", " an ideal of ", TT "S", " then we compute a minimal presentation of  ", TT "R = S/I", " and 
	  return a quotient ring ", TT "S'/J", " such that ", TT "S'/J", " is 
	  isomorphic to ", TT "S/I", "."
    	  },
     Inputs => {
	  "R" => {"Any quotient ring S/I."}
	  },
     Outputs => {
	  {"A quotient ring ", TT "Q = S'/J", " that is isomorphic 
	       to ", TT "S/I", " and ", TT "J", " has fewer generators 
	       than ", TT "I", "."}
	  },
     PARA "The function ", TT "minPres", " is written so that it stores 
     the map from ", TT "R = S/I", " to ", TT "Q = S'/J", ".  The map is stored 
     as ", TT "R.minPresMap", " , where ", TT "I", " is the input
     ideal.  Similarly the inverse of this map is stored 
     as ", TT "R.minPresMapInv", ".",
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres(C)",
	  "C.minPresMap",
	  "C.minPresMapInv",
	  }
     }


document{ 
     Key => [minPres,Variable],
     Headline=> "Rename the variables in the ring given so that the minimal 
     presentation variables are named differently than those in the original 
     ring."
     }

document{
     Key => (minPres, Ideal),
     Headline => "compute a minimal presentation of an ideal",
     Usage => {
	  TT "J = minPres I", "-- If ", TT "R", " is the ring of ", TT "I", 
	  " a minimal presentation of ", TT "R/I", " is computed and 
	  and an ideal ", TT "J", " in a polynomial ring S is returned such that ", TT "S/J", " is 
	  isomorphic to ", TT "R/I", "."
    	  },
     Inputs => {
	  "I" => {"Any ideal in a polynomial ring."}
	  },
     Outputs => 
	  {"An ideal such that ", TT "R/J", " is isomorphic 
	       to ", TT "R/I", " and ", TT "J", " has fewer generators 
	       than ", TT "I", "."},
     PARA "The function ", TT "minPres", " is written so that it stores 
     the map from ", TT "R/I", " to ", TT "R/J", ".  The map is stored 
     as ", TT "I.cache.minPresMap", " , where ", TT "I", " is the input
     ideal.  Similarly the inverse of this map is stored 
     as ", TT "I.cache.minPresMapInv", ".  There are times when it is 
     advantageous to have the input be an ideal as opposed to the 
     quotient ring.",
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w];",
	  "I = ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres I",
	  "I.cache.minPresMap",
	  "I.cache.minPresMapInv",
	  }
     }
