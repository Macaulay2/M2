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
     PARA{
     	  "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
	  },
     EXAMPLE lines ///
     	  R = QQ[x,y]/(y^2-x^3);
	  M = image matrix{{x,y}}
	  H = Hom(M,M)
	  ///,
     PARA {
	  "To recover the modules used to create a Hom-module, use the function ", TO "formation", "."
	  },
     PARA {      
     	  "Specific homomorphisms may be obtained using ", TO homomorphism, ", as follows."
	  },
     EXAMPLE lines ///
	  f0 = homomorphism H_{0}
	  f1 = homomorphism H_{1}
	  ///,
     PARA {
	  "In the example above, ", TT "f0", " is the identity map, and ", TT "f1", " maps x to y and y to x^2."
	  },
     SeeAlso => {homomorphism, Ext, compose, formation}
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

multidoc ///
Node
 Key
  (Hom,Matrix,Matrix)
 Headline
  induced map on Hom
 Usage
  Hom(f,g)
 Inputs
  f:
  g:
 Outputs
  :
   the map on Hom induced by the maps {\tt f} and {\tt g}
Node
 Key
  (Hom,Matrix,Module)
 Headline
  induced map on Hom
 Usage
  Hom(f,M)
 Inputs
  f:
  M:
 Outputs
  :
   the induced map on Hom
 Description
  Example
   R = QQ[x]
   f = vars R
   M = image f
   g = Hom(f,M)
   target g
   source g
Node
 Key
  (Hom,Module,Matrix)
 Headline
  induced map on Hom
 Usage
  Hom(M,f)
 Inputs
  M:
  f:
 Outputs
  :
   the induced map on Hom
 Description
  -- the code for Hom(Module,Matrix) is wrong, so we simplify this example temporarily
  Example
   R = QQ[x]
   f = vars R
   M = coker presentation image f
   g = Hom(M,f)
   target g
   source g
Node
 Key
  (Hom,Module,ChainComplexMap)
 Headline
  induced map on Hom
 Usage
  Hom(M,f)
 Inputs
  M:
  f:
 Outputs
  :
   the induced map on Hom
Node
 Key
  (Hom,ChainComplexMap,Module)
 Headline
  induced map on Hom
 Usage
  Hom(f,M)
 Inputs
  M:
  f:
 Outputs
  :
   the induced map on Hom
///

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
     PARA{"If ", TT "F", " or ", TT "G", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way."},
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

document {
     Key => {(compose,Module,Module,Module), compose},
     Headline => "composition as a pairing on Hom-modules",
     Usage => "compose(M,N,P)",
     Inputs => { "M", "N", "P" },
     Outputs => { { "The map ", TT "Hom(M,N) ** Hom(N,P) -> Hom(M,P)", " provided by composition of homomorphisms." } },
     PARA { "The modules should be defined over the same ring." },
     PARA { "In the following example we check that the map does implement composition." },
     EXAMPLE lines ///
	R = QQ[x,y]
	M = image vars R ++ R^2
	f = compose(M,M,M);
	H = Hom(M,M);
	g = H_{0}
	h = homomorphism g
	f * (g ** g)
	h' = homomorphism oo
	h' === h * h
	assert oo
     ///,
     SeeAlso => {Hom, homomorphism, homomorphism'}
     }
