--- status: draft
--- author(s): Stillman
--- notes: 

doc ///
Node
  Key
    cokernel
  Headline
    cokernel of a map
  SeeAlso
    target
///

document { 
     Key => {
	 (cokernel, Matrix),
	 (cokernel, RingElement)
     },
     Headline => "cokernel of a map of modules",
     Usage => "cokernel f",
     Inputs => { { TT "f : A --> B", ofClass { Matrix, RingElement } } },
     Outputs => { {"the object ", TT "B/(image f)"} },
     PARA{ TT "coker", " is a synonym for ", TT "cokernel", "." },
     PARA{ "The generators of the cokernel are provided by the generators of the target
     	  of ", TT "f", ".  In other words, ", TT "cover target f", " and ", TT "cover cokernel f", " are equal." },
     PARA{ "An argument ", TT "f", " that is a ", TO RingElement, " is interpreted as a one by one matrix."},
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "M = cokernel matrix{{2*a-b,3*c-5*d,a^2-b-3}}"
	  },
     "If ", TT "f", " is a matrix, and the target of ", TT "f", " is a submodule, the resulting module will be a ",
     TO "subquotient", " module.",
     EXAMPLE {
	  "f = map(a*M, M, a^3+a^2*b)",
	  "(target f,source f)",
	  "N = cokernel f",
	  "minimalPresentation N"
	  },
     SeeAlso => { cover, image, kernel, coimage, comodule, minimalPresentation, "matrices to and from modules"}
     }

