--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

undocumented{(isField, InexactField)}

document { 
     Key => {isField, (isField, EngineRing), (isField, Ring)},
     Headline => "whether something is a field",
     Usage => "isField R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "R", 
	       " was explicitly constructed as a field (no computation is done) and ", 
	       TO "false", " otherwise"}
	  },
     "This function recognizes basic fields, ", TO2("GaloisField","Galois fields"), 
     " and ", TO2("FractionField","fraction fields"), ".",
     EXAMPLE {
	  "isField QQ",
	  "isField CC",
	  "isField GF(2,3)",
	  "isField(frac(QQ[x,y]))"
	  },
     "This function will not recognize other rings as fields.",
     EXAMPLE {
	  "R = QQ[x]/(x^2+1)",
	  "isUnit x",
	  "isField R",
	  "F = toField R",
	  "isField F"
	  },
     SeeAlso => {toField, FractionField, GaloisField, QuotientRing, isUnit}
     }

