--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isField, (isField, FractionField), (isField, QuotientRing), (isField, GaloisField), 
	  (isField, Ring)},
     Headline => "whether something is a field",
     Usage => "isField R",
     Inputs => {
	  "R" => Ring => ""
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "R", 
	       " was explicitly constructed as a field (no computation is done) and ", 
	       TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "isField QQ",
	  "isField GF(2,3)",
	  "isField(frac(QQ[x,y]))",
	  "R = QQ[x]/(x^2+1)",
	  "isUnit x",
	  "isField R",
	  "F = toField R",
	  "isField F"
	  },
     SeeAlso => {toField, FractionField, GaloisField, QuotientRing, isUnit}
     }

