--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => cotangentSheaf,
     Headline => "cotangent sheaf of a projective variety",
     SeeAlso => {tangentSheaf, ProjectiveVariety}
     }
document { 
     Key => {(cotangentSheaf,ProjectiveVariety),[cotangentSheaf,Minimize]},
     Usage => "cotangentSheaf X",
     Inputs => {
	  "X",
	  Minimize => Boolean => {"whether to apply ", TO "minimalPresentation", " to the result before returning it"}
	  },
     Outputs => {CoherentSheaf => "the cotangent sheaf of the projective variety"
	  },
     "This function computes the cotangent sheaf of the projective variety ", TT "X.",
     PARA{},
     "As an example we verify Gauss-Bonnet's theorem on a plane quartic curve:",
     EXAMPLE {
	  "X=Proj(QQ[x,y,z]/(x^4+y^4+z^4))",
	  "genus(X)",
	  "omega = cotangentSheaf(X)",
	  "degree omega"
	  },
     SeeAlso => {tangentSheaf, ProjectiveVariety}
     }

document { 
     Key => (cotangentSheaf,ZZ,ProjectiveVariety),
     Headline => "exterior powers of the cotangent sheaf of a projective variety",
     Usage => "cotangentSheaf(p,X)",
     Inputs => {"p", "X",
	  Minimize => Boolean => {"whether to apply ", TO "minimalPresentation", " to the result before returning it"}
	  },
     Outputs => {CoherentSheaf => {"the ", TT "p", "-th exterior power of
     the cotangent sheaf"}
	  },
     "This function computes the ", TT "p", "-th exterior power of
     the cotangent sheaf of a variety ", TT "X", ".",
     PARA{},
     "As an example we compute h^11 on a K3 surface (a quartic in projective threespace):",
     EXAMPLE {
	  "K3 = Proj(QQ[x_0..x_3]/(x_0^4+x_1^4+x_2^4+x_3^4-11*x_0*x_1*x_2*x_3))",
	  "omega1 = cotangentSheaf(1,K3);",
	  "HH^1(omega1)"
	  },
     PARA{},
     "As a second example we compute Hodge numbers of the Fermat quintic in projective fourspace:", 
     EXAMPLE {
	  "FermatQuintic = Proj(QQ[x_0..x_4]/(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5))",
	  "omega1 = cotangentSheaf(1,FermatQuintic);",
	  "HH^1(omega1)",
	  "omega2 = cotangentSheaf(2,FermatQuintic);",
	  "HH^1(omega2)",
	  "HH^2(omega1)"
	  },
     SeeAlso => {tangentSheaf}
     }

