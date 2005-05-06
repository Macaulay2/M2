--- status: Draft
--- author(s): Amelia Taylor
--- notes: 

document { 
     Key => minPres,
     Headline => "minimal presentation of a ring",
     }
document { 
     Key => (minPres,Ring),
     Headline => "compute a minimal presentation of a quotient ring",
    Usage => "minPres R",
     Inputs => {
	  "R" => "S/I"
	  },
    Outputs => {
	  QuotientRing => {TT "S'/J", " that is isomorphic 
	       to ", TT "S/I", " and ", TT "S'/J", " is a minimal 
	       presentation of ", TT "S/I",}
	  },
     Consequences => {
	  {TT "minPres", " stores the isomorphism from ", TT "R = S/I", 
	       " to ", TT "S'/J", " as ", TT "R.minPresMap", " and the inverse 
	       of this map as ", TT "R.minPresMapInv"}
	  },
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
     Usage => "minPres I",
     Inputs => {
	  "I" => ""
	  },
     Outputs => {
	  Ideal => {
	       TT "J", " such that ", TT "R/I", " is 
	       isomorphic to ", TT "S/J", " where ", TT "R", 
	       " and ", TT "S", " are the rings for ", TT "I", "and ", TT "J", 
	       " respectively and ", TT "S/J", "is a minimal presentation of ", 
	       TT "R/I",}
	  },
     Consequences => {
     	  {TT "minPres", " stores the isomorphism  from ", TT "R/I", " to ", 
	       TT "S/J", " as ", TT "I.cache.minPresMap", " , where ", 
	       TT "I", " is the input ideal and the inverse of this map 
	       as ", TT "I.cache.minPresMapInv"}
	  },   
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
	  "uses u in the construction of a new ring isomorphic to the one 
	  given"
	  },     
     EXAMPLE {
	  "C = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
	  "minPres(C,Variable => a)",
	  },
     SeeAlso => {}
     }
 -- minPres_doc.m2:2:     Key => (minPres,Ring),
 -- minPres_doc.m2:33:     Key => [minPres,Variable],
 -- minPres_doc.m2:40:     Key => (minPres, Ideal),
 
 
