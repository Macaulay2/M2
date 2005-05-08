--- status: Draft
--- author(s): Decker 
--- notes: 

document { 
     Key => {radical, (radical,Ideal),  (radical,MonomialIdeal)},
     Headline => "Compute the radical of an ideal",
     Usage => "radical I",
     Inputs => {"I" => (Ideal) => ""},
     Outputs => {Ideal => {"the radical of ", TT "I", " ."}},
     "Computes the radical of ", TT "I", " using the Eisenbud-Huneke-Vasconcelos algorithm.
     If ", TT "I", " is  ", OFCLASS MonomialIdeal, ", a faster \"combinatorial\" algorithm is used.",
     EXAMPLE {
	  "R=QQ[x,y]",
	  "I=ideal((x^2+1)^2*y, y+1)",
	  "radical I"
	  },
     Caveat => {"The current implementation requires that the
	  characteristic of the ground field is either zero
	  or a \"large\" prime (unless ", TT "I", " is  ", OFCLASS MonomialIdeal, ")."},
     SeeAlso => {decompose, top, removeLowestDimension, saturate, quotient}
     }


 -- doc10.m2:317:     Key => radical, 
 -- overviewB.m2:368:     Key => "radical of an ideal",



