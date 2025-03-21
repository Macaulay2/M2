document {
     Key => {(map,ChainComplex,ChainComplex,Function),
	  (map,GradedModule,GradedModule,Function)},
     Headline => "make a map of chain complexes",
     Usage => "map(C,D,f) or map(C,D,f,Degree=>d)",
     Inputs => { "C", "D", "f" => {"a function such that ", TT "f(i)", " is a matrix ", TT "D_i --> C_(i+d)"}
	  },
     Outputs => {
	  ChainComplexMap => {"a map of chain complexes ", TT "D --> C"}
	  },
     "If the degree d is not given, then d=0 is assumed.",
     PARA{},
     "The function ", TT "f", " is called only for those indices that represent spots
     occupied in both the source and target chain complexes.",
     Caveat => {"This function does not check that the maps ", TT "f(i)",
	  " commute with the differential of the chain complexes."},
     SeeAlso => {
	  extend,
	  "ChainComplex"
	  }
     }
