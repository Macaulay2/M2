--- status: Draft
--- author(s): Amelia Taylor
--- notes: 

document { 
     Key => minPres,
     Headline => "minimal presentation of a ring",
     SeeAlso => {prune}
     }
document { 
     Key => (minPres,Ring),
     Headline => "compute a minimal presentation of a quotient ring",
     Usage => "Q/J = minPres R",
     Inputs => {
	  "R" => {TT "S/I"}
	  },
    Outputs => {
	  "Q/J" => QuotientRing => {"isomorphic to ", TT "R"}
	  },
     Consequences => {
	  {TT "minPres", " stores the isomorphism from ", TT "R = S/I", 
	       " to ", TT "Q/J", " as ", TT "R.minPresMap", " and the inverse 
	       of this map as ", TT "R.minPresMapInv"}
	  },
     "If the ring ", TT "S/I", " is graded, then the quotient ring ", 
     TT "Q/J", " is a minimal presentation 
     of ", TT "R = S/I", ". This is accomplished through considering the 
     generators of ", TT "I", ". If 
     a variable occurs as a term of a generator of ", TT "I", " and in no 
     other terms of the same polynomial, then  the variable is replaced 
     by the remaining terms and removed from the ring. A minimal generating 
     set for the resulting defining ideal is then computed; this is ", 
     TT "J", " and ", TT "Q/J", "the new quotient ring is returned. If ", 
     TT "S/I", " is not graded, then an attempt is made to improve the
      presentation of ", TT "S/I", ".",
    EXAMPLE {
	  "R = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres(R)",
	  "R.minPresMap",
	  "R.minPresMapInv"
	  },
     SeeAlso => {(minPres,Ideal)}     
     }
document { 
     Key => (minPres,Ideal),
     Headline => "compute a minimal presentation of the quotient ring 
     defined by an ideal",
     Usage => "J = minPres I",
     Inputs => {
	  "I" => {"in a ring ", TT "S"}
	  },
     Outputs => {
	  "J" => Ideal => {" in a new ring ", TT "Q", " with ", TT "S/I", " 
	       isomorphic to ", TT "Q/J"}
	  },
     Consequences => {
     	  {TT "minPres", " stores the isomorphism  from ", TT "S/I", " to ", 
	       TT "Q/J", " as ", TT "I.cache.minPresMap", " , where ", 
	       TT "I", " is the input ideal and the inverse of this map 
	       as ", TT "I.cache.minPresMapInv"}
	  },  
      "If the ideal ", TT "I", " is homogeneous, then the ideal ", TT "J", ", 
      in a new ring ", TT "Q", " is the defining ideal 
      for a minimal presentation of the ring ", TT "S/I", " where ", TT "S", 
      " is the ring of ", TT "I", ". This is accomplished as follows. If 
      a variable occurs as a term of a generator 
      of ", TT "I", " and in no other terms of the same polynomial, then the 
      variable is replaced 
      by the remaining terms and removed from the ring. A minimal generating 
      set for the resulting ideal is then computed. If ", TT "I", " is 
      not homogeneous, then an attempt is made to improve the presentation 
      of ", TT "S/I", ".",  
      EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w];",
	  "I = ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres I",
	  "I.cache.minPresMap",
	  "I.cache.minPresMapInv"
	  },
     SeeAlso => {(minPres,Ring)}
     },
document { 
     Key => [minPres, Variable],
     Headline => "specify the variable for the new isomorphic ring",
     Usage => "minPres(..., Variable => u)",
     Inputs => {
	  "u" => Symbol => ""
	  },
     Consequences => {
	  {"variables in the resulting ring are ", TT "u_0, u_1, ..."}
	  },     
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres(C,Variable => a)",
	  },
     Caveat => {"If the symbol, ", TT "u", " is used as a variable in 
	  the original ring an error is returned."}
     }
 -- minPres_doc.m2:2:     Key => (minPres,Ring),
 -- minPres_doc.m2:33:     Key => [minPres,Variable],
 -- minPres_doc.m2:40:     Key => (minPres, Ideal),
 
 
