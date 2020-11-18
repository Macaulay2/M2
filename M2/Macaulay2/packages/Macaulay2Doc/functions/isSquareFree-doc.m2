--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isSquareFree, (isSquareFree,MonomialIdeal)},
     Headline => "whether something is square free monomial ideal",
     Usage => "isSquareFree I",
     Inputs => {
	  "I" => MonomialIdeal
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "I", " is a square free ", 
	       TO2("MonomialIdeal", "monomial ideal"), 
	       " and ", TO "false", " otherwise"}	  
	  },
     "A square free ", TO2("MonomialIdeal", "monomial ideal"), " is an ideal generated 
     by products of variables; in other words, a radical
     monomial ideal.",
     EXAMPLE {
	  "QQ[x,y,z];",
	  "J = monomialIdeal(x^3*y^5*z, y^5*z^4, y^3*z^5, 
	       x*y*z^5, x^2*z^5, x^4*z^3, x^4*y^2*z^2, 
	       x^4*y^4*z)",
	  "isSquareFree J",
	  "radical J",
	  "isSquareFree radical J"
	  },    
     "Square free monomial ideals correspond both to 
     simplicial complexes and to unions of coordinate 
     subspaces.",     
     EXAMPLE {
	  "needsPackage \"SimplicialComplexes\"",
	  "R = QQ[a..d]",
	  "D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}",
	  "I = monomialIdeal D",
	  "isSquareFree I"
	  },
     PARA{},
     "Implemented by Greg Smith.",	       
     SeeAlso => {"MinimalPrimes::radical", "PrimaryDecomposition::associatedPrimes", "SimplicialComplexes::SimplicialComplexes"}
     }
