-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => isSubset,
     Headline => "whether one object is a subset of another"
     }
document { 
     Key => { (isSubset, Set, Set), (isSubset, Set, VisibleList), (isSubset, VisibleList, Set), (isSubset, VisibleList, VisibleList)},
     Usage => "isSubset(x,y)",
     Inputs => {
	  "x" => {ofClass {Set, List}},
	  "y" => {ofClass {Set, List}},
	  },
     Outputs => {
	  Boolean => {"whether every element of ", TT "x", " is in ", TT "y"},
	  },
     EXAMPLE {
	  "isSubset(set{a},set{a,b,c})",
	  "isSubset({a},set{a,b,c})",
	  "isSubset({a,a},{a,b,c})"
	  },
     SeeAlso => {Set}
     }

document { 
     Key => {(isSubset,Module,Module),(isSubset,Ideal,Module),(isSubset,Module,Ideal)},
     Usage => "isSubset(M,N)",
     Inputs => { "M", "N" },
     Outputs => { Boolean => {"whether ", TT "M", " is contained in ", TT "N"} },
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  M = image matrix {{x,0},{0,y}}
	  N = image matrix {{x^2,0},{-y,y}}
	  isSubset(N,M)
	  isSubset(M,N)
     ///
     }

document {
     Key => {(isSubset,Ideal,Ideal)},
     Usage => "isSubset(I,J)",
     Inputs => { "I", "J" },
     Outputs => { Boolean => {"whether ", TT "I", " is contained in ", TT "J"} },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2-b*c-1,a*c-1,b^3-1);",
	  "isSubset(I^2,I)",
	  "isSubset(I,I^2)"
	  },
     "In polynomial rings, this is accomplished by computing a Gröbner basis of ", TT "J", " and testing 
     whether every element of ", TT "I", " reduces to 0 modulo ", TT "I", "."
     }

