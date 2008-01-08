--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isAffineRing, (isAffineRing,QuotientRing), (isAffineRing,PolynomialRing), (isAffineRing,Ring)},
     Headline => "whether something is an affine ring",
     Usage => "isAffineRing R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  Boolean => 
	  {TO "true", " if ", TT "R", " is an affine ring and ", TO "false", " otherwise"}
	  },
     "For our purposes, an affine ring is a quotient of a (not necessarily commutative) ", TO2("PolynomialRing","polynomial ring"), 
     " over a field.",
     EXAMPLE {
	  "isAffineRing (ZZ[a,b,c,d])",
	  "isAffineRing (ZZ/101[a,b,c,d])",	  
	  "isAffineRing (ZZ/2[x,y,z]/(x^2-y*z))",
	  "isAffineRing (QQ[x,dx, WeylAlgebra => {x => dx}])"
	  },
     SeeAlso => {coefficientRing, isField, ambient}
     }

