needs "D-modules.m2"

document { WeylClosure,
     TT "WeylClosure I", " -- computes the Weyl closure of a finite rank
     ideal I",
     BR, NOINDENT,
     TT "WeylClosure (I, f)", " -- 
     compute the partial Weyl closure of a finite rank ideal I with
     respect to f",
     
     PARA,
     "Let R = K(x_1..x_n)<d_1..d_n> denote the ring of differential
     operators with rational function coefficients. The Weyl closure
     of an ideal I in D, is the intersection of the extended ideal RI
     with D.  It consists of all operators which vanish on the common
     holomorphic solutions of I, and is thus analogous to the radical
     operation on a commutative ideal.",
     
     PARA,
     "The partial Weyl closure of I with respect to a polynomial f
     is the intersection of the extended ideal D[f^{-1}]I with D.",

     PARA,
     "The Weyl closure is computed by localizing D/I with respect to
     a polynomial f vanishing on the singular locus, and computing
     the kernel of the map D --> D/I --> (D/I)[f^{-1}].",
     
     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,Dx, WeylAlgebra => {x=>Dx}]",
     EXAMPLE "I = ideal(x*Dx-2)",
     EXAMPLE "WeylClosure I",

     PARA,
     "Caveats and known problems :",
     MENU{"The ideal I should be finite rank, which can be tested
	  manually by Drank.", "The Weyl closure of non-finite rank
	  ideals or arbitrary submodules has not been implemented."},
	  
     SEEALSO {"Dlocalize", "singLocus", "Drank"}
     }
