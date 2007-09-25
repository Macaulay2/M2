--- status: Draft
--- author(s): Giulio 
--- notes: 

document { 
     Key => {regularity,(regularity,ChainComplex),(regularity,Ideal),(regularity,Module)},
     Headline => "compute the Castelnuovo-Mumford regularity",
     Usage => "regularity C",
     Inputs => {
	  "C" => {"a ", TO ChainComplex, ", an  ",TO Ideal, ", or a ", TO Module}
	  },
     Outputs => {
	  ZZ} ,
     "For a free chain complex C, the regularity r is the smallest number so that 
      each basis element of C_i has degree at most i+r.  For a module M, the
      regularity is the regularity of a free minimal resolution of M.",
     EXAMPLE {
	  "R=ZZ/32003[a..d];",
	  "I=ideal(a^20,b^20,a*c^19-b*d^19);",
	  "regularity I"
	  },
     PARA{},
     "The regularity is the label of the last row in the betti diagram of a chain complex.",
     EXAMPLE {
	  "J=ideal(a^3,a^2*b,a*b^6,a^2*c);",
	  "C=resolution J",
	  "betti C",
	  "regularity C"
	  },
     SeeAlso => {"resolution","betti"}
     }




--document {
--     Key => regularity,
--     Headline => "compute the regularity",
--     TT "regularity M", " -- computes the regularity of a module or chain complex C.",
--     PARA{},
--     "For a free chain complex C, the regularity r is the smallest number so that 
--     each basis element of C_i has degree at most i+r.  For a module M, the
--     regularity is the regularity of a free minimal resolution of M."
--     }
