-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): Smith
--- notes: 

document { 
     Key => {standardPairs, (standardPairs,MonomialIdeal), (standardPairs, MonomialIdeal, List)},
     Headline => "find the standard pairs of a monomial ideal",
     Usage => "standardPairs I",
     Inputs => {
	  "I" => MonomialIdeal
	  },
     Outputs => {
	  {"a ", TO List, " of standard pairs for ", TT "I"}
	  },
     "The standard monomials of a monomial ideal ", TT "I", " (those monomials that are not 
     in ", TT "I", ") can be enumerated as follows.  Given a monomial ", 
     TT "m", " and a subset ", TT "F", " of the variables, the pair ", TT "(m,F)", " indexes
     the set of monomials of the form ", TT {"m", "m'"}, 
     " where the monomial ", TT "m'", " is supported on ", TT "F", ".  A ", TO List, " of pairs ", 
     TT "(m, F)", " form ", EM "standard pairs", " for the monomial ideal ", TT "I", 
     " if it satisfies the following three conditions:",
     UL {
	  {"for each pair, the monomial ", TT {"m"}, " is supported on the complement of ", 
	       TT "F", ";"},
	  {"all of the monomials represented by a pair are standard;"},
	  {"the pairs index disjoint sets of monomials."}
	  },
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "I = monomialIdeal(x*y^3*z, x*y^2*z^2, y^3*z^2, y^2*z^3)",
	  "standardPairs I"
	  },     
     PARA{},
     "The standard pairs are computed with Algorithm 3.2.5 in
     Gröbner Deformations of Hypergeometric Differential 
     Equations, by Mutsumi Saito, Bernd Sturmfels and Nobuki Takayama;
     Algorithms and Computation in Mathematics 6, Springer-Verlag, 2000.  
     Implemented by Gregory G. Smith.",
     PARA{},
     "For more information, see the ", EM "Monomial ideals", " chapter in Computations 
     in algebraic geometry with Macaulay2, edited by David Eisenbud, Daniel R. Grayson, 
     Michael E. Stillman, and Bernd Sturmfels, Algorithms and Computations in Mathematics 8, 
     Springer-Verlag, 2001.",
     SeeAlso => {}
     }

