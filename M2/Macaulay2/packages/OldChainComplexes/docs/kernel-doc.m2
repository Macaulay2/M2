document { 
     Key => {(kernel,ChainComplexMap),
	  (kernel, GradedModuleMap)},
     Headline => "kernel of a chain complex map",
     Usage => "kernel f",
     Inputs => {
	  "f" => {"a map of chain complexes ", TT "C --> D"}
	  },
     Outputs => {
	  ChainComplex => {"the kernel of f"}
	  },
     "If f is ", ofClass GradedModuleMap, ", then the result will be ", ofClass GradedModule, ".",
     PARA{},
     EXAMPLE lines ///
     	  R = QQ[a..d]
	  I = ideal(a^3,b^3,c^3)
	  C = res coker gens I
	  D = res coker gens (I + ideal(a*b*c))
	  F = extend(D,C,map(D_0,C_0,1))
	  ///,
     SeeAlso => {syz, 
	  -- Mike wanted this: "kernel, cokernel and image of a map of modules",
	  genericSkewMatrix}
     }
