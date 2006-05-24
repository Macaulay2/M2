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
	  "f : A --> B" => {"a ", TO2(RingMap,"ring map"), ", a ",
	       TO2(Matrix, "matrix"),
	       ", a ",
	       TO2(ChainComplexMap, "chain complex map"),
	       ", or a ",
	       TO2(GradedModuleMap, "graded module map")}
	  },
     Outputs => {
	  {"The object ", TT "A/(kernel f)"}
	  },
     "In each of these cases, the coimage is isomorphic to the image.",
     PARA{},
     "This isomorphism is not always obvious.  For example,",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "M = matrix{{a^3,b^3-c^3,a*b*c,a*(b^2-c^2)}}",
	  "image M",
	  "coimage M",
	  "kernel M"
	  },
     SeeAlso => {image, cokernel, kernel, comodule}
     }
