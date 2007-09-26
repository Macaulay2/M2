--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => codim,
     Headline => "compute the codimension",
     Caveat => {"Over the integers, an error message is given, unless the option ", TT "Generic => True", "
	  is used, in which case the computation proceeds by effectively 
	  tensoring first with the rational numbers."},
     SeeAlso => {dim}
     }
document { 
     Key => {(codim,QuotientRing),(codim, PolynomialRing)},
     Usage => "codim R",
     Inputs => {"R"
	  },
     Outputs => {ZZ
	  },
     "Computes the codimension of the presentation ideal of ", TT "R",
     " over its ambient polynomial ring.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
          "codim R"
	  },
     "However the following may not be the expected result.",
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
     Caveat => {"The returned value is the usual codimension if ", TT "R", 
	  " is an integral domain or, more generally, equidimensional."},
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
     Key => {(codim,Ideal),(codim,MonomialIdeal)},
     Usage => "codim I",
     Inputs => {"I"
	  },
     Outputs => {ZZ
	  },
     "Computes the codimension of the ideal ", TT "I", ".",
     EXAMPLE {
	  "R = ZZ/101[a..e];",
	  "I = monomialCurveIdeal(R,{2,3,5,7})",
	  "J = ideal presentation singularLocus(R/I)",
	  "codim J",
	  "radical J"	  
	  },
     Caveat => {"We don't really compute the codimension of ", TT "I",
	  " when the basering ", TT "R", " of ", TT "I", " is not equidimensional! 
	  What codim actually computes is ", TT "dim(R)-dim(R/I)."},
     SeeAlso => {(dim,Ideal),(dim,MonomialIdeal)}
     }
document { 
     Key => (codim,ProjectiveVariety),
     Headline => "codimension of the projective variety",
     Usage => "codim V",
     Inputs => {"V"
	  },
     Outputs => {ZZ
	  },
     "Computes the codimension of the projective variety ", TT "V", ".",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_3];",
	  "M = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}",
	  "V = Proj(R/minors(2,M));",
	  "codim V"
	  },
     Caveat => {"The returned value is the usual codimension if the base graded ring 
	  is an integral domain or, more generally, equidimensional."},
     SeeAlso => {(codim,QuotientRing)}
     }

