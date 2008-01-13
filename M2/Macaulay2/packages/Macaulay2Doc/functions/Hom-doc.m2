--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => Hom,
     Headline => "module of homomorphisms"
     }

document { 
     Key => {(Hom,Module,Module),
	  (Hom,Module,Ideal),
	  (Hom,Module,Ring),
	  (Hom,Ideal,Module),
	  (Hom,Ideal,Ideal),
	  (Hom,Ideal,Ring),
	  (Hom,Ring,Module),
	  (Hom,Ring,Ideal)},
     Headline => "module of homomorphisms",
     Usage => "Hom(M,N)",
     Inputs => {
	  "M","N"
	  },
     Outputs => {
	  Module => {"The module Hom_R(M,N), where M and N are both R-modules"}
	  },
     "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     PARA{},
     EXAMPLE lines ///
     	  R = QQ[x,y]/(y^2-x^3);
	  M = image matrix{{x,y}}
	  H = Hom(M,M)
	  H1 = prune H
	  ///,
     "Specific homomorphisms may be obtained using ", TO homomorphism, ".",
     EXAMPLE lines ///
	  f1 = homomorphism H_{0}
	  f2 = homomorphism H_{1}
	  f3 = homomorphism H_{2}
	  ///,
     "In this example, f1 is the identity map, f2 is multiplication by x,
     and f3 maps x to y and y to x^2.",
     PARA{},
     SeeAlso => {homomorphism, Ext}
     }

document { 
     Key => {(Hom,Module,ChainComplex),(Hom,ChainComplex,Module)},
     Headline => "",
     Usage => "Hom(M,C)\nHom(C,M)",
     Inputs => {
	  "M",
	  "C"
	  },
     Outputs => {
	  ChainComplex => {"The chain complex whose ", TT "i", "-th spot is ", 
	       TT "Hom(M,C_i)", ", in the first case, or ", TT "Hom(C_(-i),M)", " in the second case"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  C = res coker vars R
	  M = R^1/(a,b)
	  C' = Hom(C,M)
	  C'.dd_-1
	  C'.dd^2 == 0
	  ///,
     Caveat => {"Hom of two chain complexes is not yet implemented"},
     SeeAlso => {resolution}
     }
document { 
     Key => {(Hom,Matrix,Module),(Hom,Module,Matrix),
	  (Hom,Module,ChainComplexMap),(Hom,ChainComplexMap,Module)},
     Headline => "induced map on Hom modules",
     Usage => "Hom(f,M)\nHom(M,f)",
     Inputs => {
	  "f" => {"a map ", TT "N1 --> N2", " of modules or ", ofClass ChainComplex}, 
	  "M" => {"over the same ring ", TT "R", " as ", TT "f"}
	  },
     Outputs => {
	  Matrix => {"either the map ", TT "Hom_R(N2,M) --> Hom_R(N1,M)", " in the first case,
	    or the map ", TT "Hom_R(M,N1) --> Hom_R(M,N2)", " in the second case"}
	  },
     "If ", TT "f", " is a map of chain complexes, then the result is a ", ofClass ChainComplexMap, ".",
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  I = ideal(a*b,c*d);
	  J = I + ideal(a*d);
	  f = inducedMap(module J,module I)
	  g = Hom(R^3,f)
	  ker g
	  image g
	  ///,
     Caveat => "Not all possible combinations are implemented yet",
     SeeAlso => {}
     }
document { 
     Key => {(Hom,CoherentSheaf,CoherentSheaf),
	  (Hom, SheafOfRings, SheafOfRings),
	  (Hom, CoherentSheaf, SheafOfRings),
	  (Hom, SheafOfRings, CoherentSheaf)},
     Headline => "global Hom",
     Usage => "Hom(F,G)",
     Inputs => {
	  "F", "G" => {"both should be sheaves on a 
	               projective variety or scheme ", TT "X = Proj R"},
	  },
     Outputs => {
	  Module => {"over the coefficient field of ", TT "R"}
	  },
     "If ", TT "F", " or ", TT "G", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     EXAMPLE lines ///
          R = QQ[a..d];
	  P3 = Proj R
	  I = monomialCurveIdeal(R,{1,3,4})
	  G = sheaf module I
	  Hom(OO_P3,G(3))
	  HH^0(G(3))
          ///,
     SeeAlso => {sheafHom, Ext, sheafExt}
     }

