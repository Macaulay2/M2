--- status: Draft
--- author(s): Giulio
--- notes: 

document { 
     Key => {removeLowestDimension,(removeLowestDimension,Ideal),(removeLowestDimension, Module)},
     Headline => "remove components of lowest dimension",
     Usage => "removeLowestDimension M",
     Inputs => {
	  "M" =>{"an ", TO Ideal, " or a ", TO Module}
	  },
     Outputs => {	  
	  "N" => {"an ", TO Ideal, ", respectively a ", TO Module, "."} 
	       },
       "This function yields the intersection of the primary components of ", TT "M", ", 
       except those of lowest dimension (and thus returns the
       ambient free module of ", TT "M", " (or unit ideal), if ", TT "M", "
       is pure dimensional).", PARA{},
       "For a very brief description of the method used, see ", TO "topComponents", ".",PARA{},
       "As an example we remove the lowest dimensional component of an ideal I",
     
     EXAMPLE {
	  "R=ZZ/32003[a..d];",
	  "I=intersect(ideal(a*b+a^2,b^2),ideal(a^2,b^2,c^2),ideal(b^3,c^3,d^3))",
	  "removeLowestDimension I"
	  },
     SeeAlso => {"topComponents", "Colon :: saturate", "Colon :: Ideal : Ideal", "radical", "MinimalPrimes :: minimalPrimes"}
     }





--document {
--     Key => removeLowestDimension,
--     Headline => "remove components of lower dimension",
--     TT "removeLowestDimension I", " -- removes the components of ", TT "I", " of lower dimension",
--     PARA{},
--     "Yields the intersection of the primary components of ", TT "I", ",
--     excepting those of lowest dimension (and thus returns the
--     ambient free module of ", TT "I", " (or unit ideal), if ", TT "I", "
--     is pure dimensional).",
--     PARA{},
--     "For an example, see also ", TO "component example", ".",
--     PARA{},
--     "Computes one free resolution, and some homology groups, but no
--     projections or determinants are used.  For a very brief description
--     of the method used, see ", TO "topComponents", ".",
--     SeeAlso => {"topComponents", "saturate", "quotient", "radical", "minimalPrimes"}
--     }
