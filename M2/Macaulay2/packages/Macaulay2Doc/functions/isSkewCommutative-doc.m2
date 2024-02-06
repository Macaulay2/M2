--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {isSkewCommutative, (isSkewCommutative, PolynomialRing), (isSkewCommutative, Ring), (isSkewCommutative, QuotientRing)},
     Headline => "whether a ring has skew commuting variables",
     Usage => "isSkewCommutative R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "R", " has skew commuting variables
	        and ", TO "false", " otherwise"}
	  },
     EXAMPLE lines ///
     	  A = ZZ/3[a,b,c];
	  isSkewCommutative A
	  B = QQ[a..d,SkewCommutative=>{a,b}]
	  isSkewCommutative B
	  C = B[x,y]
	  isSkewCommutative C
	  b_C * a_C
	  D = B/(a*d-b*c)
	  isSkewCommutative D
	  ///,
     SeeAlso => {"exterior algebras"}
     }
