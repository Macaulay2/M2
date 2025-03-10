-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

doc ///
Node
  Key
    kernel
  Headline
    kernel of a map
  SeeAlso
    source
///

document { 
     Key => (kernel,RingMap),
     Headline => "kernel of a ringmap",
     Usage => "kernel f",
     Inputs => {
	  "f" => {TT "R", " --> ", TT "S"},
	  SubringLimit => ZZ => "stop the computation after this many elements of the kernel have been found"
	  },
     Outputs => {
	  Ideal => {"an ideal of ", TT "R"}
	  },
     EXAMPLE lines ///
	  R = QQ[a..d];
	  S = QQ[s,t];
	  F = map(S,R,{s^3, s^2*t, s*t^2, t^3})
	  ker F
	  G = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
	  ker(G, SubringLimit=>1)
	  ///,
     "In the case when everything is homogeneous, Hilbert functions are
     used to speed up the computations.",
     Caveat => {"It should be possible to interrupt the computation and restart it, but this has
	  not yet been implemented."},
     SeeAlso => {"substitution and maps between rings", "elimination of variables", monomialCurveIdeal},
     Subnodes => { TO [kernel, SubringLimit] },
     }
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

document { 
     Key => {(kernel,Matrix),
	  (kernel, RingElement)},
     Headline => "kernel of a matrix",
     Usage => "kernel f",
     Inputs => {
	  "f" => {"a map of modules ", TT "M --> N"}
	  },
     Outputs => {
	  Module => {"the kernel of f, a submodule of M"}
	  },
     "If f is ", ofClass RingElement, ", then it will be interpreted as a one by one matrix.",
     PARA{},
     "The kernel is the submodule of M of all elements mapping to zero under ", TT "f", ".
     Over polynomial rings, this is computed using a Gröbner basis computation.",
     EXAMPLE lines ///
     	  R = ZZ/32003[vars(0..10)]
	  M = genericSkewMatrix(R,a,5)
	  ker M
	  ///,
     SeeAlso => {syz, 
	  -- Mike wanted this: "kernel, cokernel and image of a map of modules",
	  genericSkewMatrix}
     }

document {
    Key => [kernel, SubringLimit],
    Headline => "stop after finding enough elements of a subring",
    TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
    causes the computation of the kernel of a ring map to stop after ", TT "n", "
    elements have been discovered."
}
