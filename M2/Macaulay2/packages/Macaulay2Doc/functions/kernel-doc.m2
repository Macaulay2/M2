-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

undocumented {
    [kernel, DegreeLimit],
    [kernel, Strategy],
}

doc ///
Node
  Key
    kernel
  Headline
    kernel of a map of modules or rings
  Description
   Text
    See the separate documentation nodes for the two cases. To compute
    the kernel of a map of free modules the command @TO syz@ is usually faster,
    since it computes only the generators of the kernel, not the relations on them,
    as is necessary to return the kernel as a module.
  SeeAlso
    source
    syz
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
     Key => {(kernel,Matrix),
	  (kernel, RingElement)},
     Headline => "kernel of a map of modules",
     Usage => "kernel f, kernel a",
     Inputs => {
	  "f" => {"a map of modules ", TT "M --> N"}
	  },
     Outputs => {
	  Module => {"the kernel of f, a submodule of M"}
	  },
     PARA{},
     "The kernel is the submodule of M of all elements mapping to zero under ", TT "f", ".",
     "If f is a RingElement it is interpreted as a 1 by 1 matrix",".",
     EXAMPLE lines ///
     	  R = ZZ/32003[a,b]/(ideal(a,b))^3
	  M = R^1/(ideal a^2)
	  mat = matrix{{a^2,b^2},{b,a}}
	  ker mat
          presentation ker mat
	  syz mat
          f = map(M++M, M++M, mat)
	  ker f
	  ///,
     SeeAlso => {syz, 
	         cokernel,
		 image,
		 map,
		 matrix
		 }
	  }
     

document {
    Key => [kernel, SubringLimit],
    Headline => "stop after finding enough elements of a subring",
    TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
    causes the computation of the kernel of a ring map to stop after ", TT "n", "
    elements have been discovered."
}
