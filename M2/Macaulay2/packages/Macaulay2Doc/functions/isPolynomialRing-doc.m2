--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isPolynomialRing, (isPolynomialRing,Thing), (isPolynomialRing,PolynomialRing)},
     Headline => "whether something is a polynomial ring",
     Usage => "isPolynomialRing R",
     Inputs => {
	  "R" => Thing
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "R", " is a ", TO2("PolynomialRing","polynomial ring"), 
	       " and ", TO "false", " otherwise"}
	  },
     EXAMPLE {
	  "isPolynomialRing CC",
	  "isPolynomialRing(ZZ/7[x,y])",
	  "isPolynomialRing(QQ[x,dx, WeylAlgebra => {x => dx}])",
	  "isPolynomialRing(GF(2)[x,y, SkewCommutative => true])",
	  "isPolynomialRing(ZZ/101[x,y,z]/(x^2-y*z))"
	  },
     }
