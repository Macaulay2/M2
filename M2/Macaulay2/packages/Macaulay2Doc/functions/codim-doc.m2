--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document {
     Key => codim,
     Headline => "compute the codimension",
     SeeAlso => {dim}
     }

document { 
     Key => {[(codim,Ideal), Generic],[(codim,Module), Generic],
	  [(codim,CoherentSheaf), Generic],
	  [(codim,PolynomialRing), Generic],[(codim,Variety), Generic],
	  [(codim,QuotientRing), Generic]},
     Usage => "codim(...,Generic=>true)",
     Consequences => {
	  "Allows the computation of the codimension to proceed without an error message, even if the ring is
	  defined over the integers.  The computation proceeds by effectively 
	  tensoring first with the rational numbers."
	  }
     }
document { 
     Key => {(codim,QuotientRing),(codim, PolynomialRing)},
     Usage => "codim R",
     Inputs => {"R"},
     Outputs => {ZZ => {"the codimension of ", TT "R"}},
     "Computes the codimension of the presentation ideal of ", TT "R",
     " over its ambient polynomial ring.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
          "codim R"
	  },
     "However, the following may not be the expected result.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
          "codim R",
	  "codim (R/x)"	  
	  },
     SeeAlso => {(dim,QuotientRing)}
     }
document { 
     Key => {(codim,Module)},
     Headline =>"codimension of the support of a module",
     Usage => "codim M",
     Inputs => {"M" => {"a module over a ring ", TT "R"}
	  },
     Outputs => {ZZ
	  },
     "Computes the codimension of the support of the module as given by ", TT "dim(R) - dim(M)", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
          "M = coker matrix{{a,b},{c,d}}",
          "codim M"
	  },
     PARA {
	  "The returned value is the usual codimension if ", TT "R", 
	  " is an integral domain or, more generally, equidimensional."
	  },
     SeeAlso => {(dim,Module)}
     }

document { 
     Key => {(codim,CoherentSheaf)},
     Headline =>"codimension of the support of a coherent sheaf on a projective variety",
     Usage => "codim F",
     Inputs => {"F" => {"a coherent sheaf over a ", TO "ProjectiveVariety", TT " X"}
	  },
     Outputs => {ZZ
	  },
     "Computes the codimension of the support of ", TT "F", " as given by ", TT "dim(R) - dim(M)",
     " where ", TT "M", " is the module representing ", TT "F", " over the homogeneous coordinate ring ",
     TT "R", " of ", TT "X", ".",
     EXAMPLE {
	  "R = ZZ/31991[a,b,c,d];",
          "I = monomialCurveIdeal(R,{1,3,5})",
          "projplane = Proj(R)",
          "II = sheaf module I",
          "can = sheafExt^1(II,OO_projplane^1(-4))",
          "codim can"
	  },
     Caveat => {"The returned value is the usual codimension if ", TT "R", 
	  " is an integral domain or, more generally, equidimensional."},
     SeeAlso => {(dim,Module)}
     }

document { 
     Key => {(codim,MonomialIdeal)},
     Usage => "codim I",
     Inputs => {"I"},
     Outputs => {ZZ},
     "Computes the codimension of the monomial ideal ", TT "I", ".",
     EXAMPLE {
	  "R = ZZ/101[a..e];",
	  "codim monomialIdeal (b,c,d)",
	  "codim monomialIdeal (b^3,c^2)",
	  },
     SeeAlso => {(dim,Ideal),(dim,MonomialIdeal),(codim,Ideal)}
     }

document { 
     Key => {(codim,Ideal)},
     Usage => "codim I",
     Inputs => {"I"},
     Outputs => {ZZ => { TT "dim(R) - dim(R/I)", ", where ", TT "R", " is the ring containing ", TT "I", "." }},
     PARA {
	  "When R is equidimensional, this quantity is the codimension of the ideal ", TT "I", "."
	  },
     EXAMPLE {
	  "R = ZZ/101[a..e];",
	  "I = monomialCurveIdeal(R,{2,3,5,7})",
	  "J = ideal presentation singularLocus(R/I);",
	  "codim J",
	  "radical J"
	  },
     "The following may not be the expected result, because the ring is not equidimensional.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
	  "codim ideal(x,y)"
	  },
     SeeAlso => {(dim,Ideal),(dim,MonomialIdeal),(codim,MonomialIdeal)}
     }

document { 
     Key => (codim,Variety),
     Headline => "codimension of a variety",
     Usage => "codim V",
     Inputs => {"V"},
     Outputs => {ZZ},
     "Computes the codimension of a variety ", TT "V", ".",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_3];",
	  "M = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}",
	  "V = Proj(R/minors(2,M));",
	  "codim V"
	  },
     Caveat => {"The returned value is the usual codimension if the base ring
	  is an integral domain or, more generally, equidimensional."},
     SeeAlso => {(codim,QuotientRing)}
     }

