--- status: draft
--- author(s): Stillman
--- notes: 

document { 
     Headline => "cokernel of a map of modules, graded modules, or chaincomplexes",
     Key => cokernel,
     "Given a map ", TT "f : M --> N", " of modules, graded modules or chain complexes, returns ",
     TT "N/(image f)", ".",
     PARA,
     TT "coker", " is a synonym for ", TT "cokernel", ".",
     PARA,
     "The generators of the cokernel are provided by the generators of the target
     of ", TT "f", ".  In other words, ", TT "cover target f", " and ", TT "cover cokernel f", " are equal.",
     SeeAlso => {cover,
	  image,
	  kernel,
	  coimage,
	  "matrices to and from modules"}
     }
document { 
     Key => (cokernel,ChainComplexMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (cokernel,GradedModuleMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => {(cokernel,Matrix),(cokernel,RingElement)},
     Headline => "cokernel of a map",
     Usage => {"cokernel f", EM " or "," coker f"},
     Inputs => {
	  "f" => "a module homomorphism M --> N"
	  },
     Outputs => {
	  Module => {"the cokernel (i.e. ", TT "N/image(f)", ") of ", TT "f" }
	  },
     "An argument f which is a ", TO RingElement, " is interpreted as a one by one matrix.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "M = cokernel matrix{{2*a-b,3*c-5*d,a^2-b-3}}"
	  },
     "If the target of M is a submodule, the resulting module will be a ",
     TO "subquotient", " module.",
     EXAMPLE {
	  "f = map(a*M, M, a^3+a^2*b)",
	  "(target f,source f)",
	  "N = cokernel f",
	  "minimalPresentation N"
	  },
     SeeAlso => {
	  (image,Matrix),
	  (kernel,Matrix),
	  (coimage,Matrix),
	  (comodule,Module),
	  (minimalPresentation,Module)
	  }
     }

TEST ///
    R = QQ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover cokernel M_{1} ==  cover M ) )
///

