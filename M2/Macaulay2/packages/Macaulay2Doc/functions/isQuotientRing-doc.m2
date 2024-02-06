--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isQuotientRing, (isQuotientRing,QuotientRing), (isQuotientRing,Ring)},
     Headline => "whether something is a quotient ring",
     Usage => "isQuotientRing R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "R", " is a ", 
	       TO2("QuotientRing", "quotient ring"), " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "S = ZZ/3[x,y,z];",
	  "isQuotientRing S",
	  "R = S/(x^2-y*z);",
	  "isQuotientRing R",
	  "ambient R",
	  "symAlg = symmetricAlgebra R^2;",
	  "isQuotientRing symAlg",
	  "sing = singularLocus R;",
	  "isQuotientRing sing"
	  },
     SeeAlso => {ambient}
     }
