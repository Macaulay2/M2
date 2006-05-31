--- status: DRAFT
--- author(s): MES, taken from Dan's
--- notes: 

document { 
     Key => {coimage,
	  (coimage, ChainComplexMap),
	  (coimage, Matrix),
	  (coimage, GradedModuleMap),
	  (coimage, RingMap)},
     Headline => "coimage of a map",
     Usage => "coimage f",
     Inputs => {
	  "f" => {
	       "a ", TO2(RingMap,"ring map"), 
	       ", a ", TO2(Matrix, "matrix"),
	       ", a ", TO2(ChainComplexMap, "chain complex map"),
	       ", or a ", TO2(GradedModuleMap, "graded module map, from ", TT "A", " to ", TT "B")
	       }
	  },
     Outputs => {
	  {"The object ", TT "A/(kernel f)"}
	  },
     "In each of these cases, the coimage is isomorphic to the image, but the coimage is presented as
     a quotient object of the source of the map, whereas the image is presented as a subobject of the
     target of the map.  For rings, we can represent quotient rings, but not subrings, in Macaulay 2.
     Hence, for ring maps, we can compute the coimage, but not the image.",
     PARA "The isomorphism between coimage and image is not always obvious, as the following example shows.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "M = matrix{{a^3,b^3-c^3,a*b*c,a*(b^2-c^2)}}",
	  "image M",
	  "coimage M",
	  "kernel M"
	  },
     SeeAlso => {image, cokernel, kernel, comodule}
     }
