--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isCommutative,(isCommutative,Ring)},
     Headline => "whether a ring is commutative",
     Usage => "isCommutative R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => {TO "true", " if the ", TO2("Ring", "ring"), " ", TT "R", " is commutative and ", 
	  TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "isCommutative(QQ[x,y])",
	  "isCommutative(QQ[x,y, SkewCommutative => true])",
	  "isCommutative(QQ[x,dx, WeylAlgebra => {x => dx}])"
	  },
     SeeAlso => {WeylAlgebra, SkewCommutative}
     }
