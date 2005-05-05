--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => cotangentSheaf,
     Headline => "cotangent sheaf of a projective variety",
     SeeAlso => {tangentSheaf, ProjectiveVariety}
     }
document { 
     Key => (cotangentSheaf,ProjectiveVariety),
     Usage => "cotangentSheaf X",
     Inputs => {"X" => ""
	  },
     Outputs => {CoherentSheaf => ""
	  },
     "Computes the cotangent sheaf of the projective variety X.",
     PARA,
     "Verifying Gauss-Bonnet's theorem on a plane quartic curve:",
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
     Usage => "cotangentSheaf(p,X)",
     Inputs => {"p" => "", "X" => ""
	  },
     Outputs => {CoherentSheaf => ""
	  },
     "Computes the ", TT "p", "-th exterior power of
     the cotangent sheaf of a variety ", TT "X", ".",
     PARA,
     "h^11 of a K3 surface (quartic in projective threespace):",
     EXAMPLE {
	  "K3 = Proj(QQ[x_0..x_3]/(x_0^4+x_1^4+x_2^4+x_3^4-11*x_0*x_1*x_2*x_3))",
	  "omega1 = cotangentSheaf(1,K3);",
	  "HH^1(omega1)"
	  },
     "Hodge numbers of the Fermat quintic in projective fourspace:", 
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

 -- doc10.m2:982:     Key => (cotangentSheaf, ProjectiveVariety),
 -- doc10.m2:987:     Key => cotangentSheaf,
 -- doc10.m2:992:     Key => (cotangentSheaf, ZZ, ProjectiveVariety),
