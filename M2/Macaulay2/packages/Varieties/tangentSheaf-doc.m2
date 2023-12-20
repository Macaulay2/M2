--- status: draft
--- author(s): Popescu 
--- notes: 

document { 
     Key => tangentSheaf,
     Headline => "tangent sheaf of a projective variety",
     SeeAlso => {cotangentSheaf, sheaf,ProjectiveVariety}
     }
document { 
     Key => {(tangentSheaf,ProjectiveVariety),[tangentSheaf,Minimize]},
     Usage => "tangentSheaf X",
     Inputs => {
	  "X",
	  Minimize => Boolean => {"whether to apply ", TO "minimalPresentation", " to the result before returning it"}
	  },
     Outputs => {CoherentSheaf
	  },
     "Computes the tangent sheaf of the projective variety ", TT "X",
     PARA{},
     "Tangent sheaf of the projective plane:",
     EXAMPLE {
	  "P = Proj(QQ[a,b,c])",
	  "TP = tangentSheaf(P)",
	  "HH^0(TP(-1))",
	  "HH^1(TP(-3))"
	  },
     "Tangent sheaf of a plane nodal and cuspidal curve:",
     EXAMPLE {
	  "Node = Proj(QQ[a,b,c]/ideal(b^2*c-a^2*(a+c)))",
	  "Cusp = Proj(QQ[a,b,c]/ideal(b^2*c-a^3))",
	  "TNode = tangentSheaf(Node)",
	  "HH^0(TNode)",
	  "HH^1(TNode)",
	  "TCusp = tangentSheaf(Cusp)",
	  "HH^0(TCusp)",
	  "HH^1(TCusp)"
	  --- update this once we can compute the kernel of
	  --- a H^0(phi) for a sheaf map phi :F -> G
	  },
     SeeAlso => {cotangentSheaf, sheaf,ProjectiveVariety}
     }
