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
	  (Hom,Ring,Ring),
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

-----------------------------------------------------------------------------

document {
    Key => End,
    Headline => "module of endomorphisms",
    TT "End M", " -- constructs the module of endomorphisms of ", TT "M", "."}

-----------------------------------------------------------------------------

document {
     Key => {(reshape,Module,Module,Matrix),reshape},
     Headline => "reshape a matrix",
     Usage => "reshape(F,G,f)",
     Inputs => {
	  "F" => "a free module",
	  "G" => "a free module",
	  "f"
	  },
     Outputs => {
	  { " ", TT "F <-- G", " obtained from f by 
     	       taking elements from the first column of ", TT "f", ", 
	       then the second, and
     	       so on, filling them into the result column by column."
	       }
	  },
     "Currently, it is assumed
     that ", TT "f", " and the result both have the same 
     number of entries.  The resulting map has the same degree that ", TT "f", " has,
     but it is easy to spoil homogeneity by giving incorrect free modules.",
     EXAMPLE lines ///
	  f = matrix{{1,3,5,7,9,11},{2,4,6,8,10,12}}
	  reshape(ZZ^3,ZZ^4,f)
	  ///
     }

-----------------------------------------------------------------------------

document {
     Key => {(adjoint',Matrix,Module,Module),adjoint'},
     Headline => "an adjoint map",
     Usage => "adjoint'(f,G,H)",
     Inputs => {
	  "f" => {"a homomorphism ", TT "F --> Hom(G,H)", " between modules"},
	  "G" => "a free module",
	  "H" => "a free module"
	  },
     Outputs => {
	  {"the adjoint homomorphism ", TT "F ** G --> H"}
	  },
     PARA {
     	  "Recall that ", TO "**", " refers to the tensor product of modules.  If ", TT "f", " is
	  homogeneous, then the resulting matrix will be homogeneous."
	  },
     EXAMPLE {
	  "R = QQ[x_1 .. x_12];",
	  "f = genericMatrix(R,6,2)",
	  "g = adjoint'(f,R^2,R^3)",
	  "isHomogeneous g"
	  },
     SeeAlso => {adjoint, flip, reshape, (symbol**,Module,Module), dual}
     }

document {
     Key => {(adjoint,Matrix,Module,Module),adjoint},
     Headline => "an adjoint map",
     Usage => "adjoint(f,F,G)",
     Inputs => {
	  "f" => {"a homomorphism ", TT "F ** G --> H"},
	  "F" => "a free module",
	  "G" => "a free module"
	  },
     Outputs => {{"the adjoint homomorphism ", TT "F --> Hom(G,H)"}},
     PARA{"Recall that ", TO "**", " refers to the tensor product of modules."},
     EXAMPLE lines ///
	  R = QQ[x_1 .. x_24];
	  f = genericMatrix(R,2,4*3)
     	  isHomogeneous f
	  g = adjoint(f,R^4,R^3)
	  ///,
     PARA{"If ", TT "f", " is homogeneous, and ", TT "source f === F ** G", 
     	  " (including the grading), then the resulting matrix will be homogeneous."},
     EXAMPLE lines ///
	  g = adjoint(f,R^4,R^{-1,-1,-1})
	  isHomogeneous g
	  ///,
     SeeAlso => {adjoint', flip, reshape, (symbol**,Module,Module), dual}
     }

-----------------------------------------------------------------------------

document {
     Key => {homomorphism',(homomorphism', Matrix)},
     Headline => "get the element of Hom from a homomorphism",
     Usage => "homomorphism' f",
     Inputs => {
	  "f" => {"of the form M --> N"},
	  },
     Outputs => {
	  {"the map ", TT "R^1 --> Hom(M,N)", ", corresponding to the map ", TT "f", ""}
	  },
     EXAMPLE lines ///
	  R = QQ[x,y,z]
	  f = vars R ++ vars R
	  g = homomorphism' f
	  target g === Hom(source f, target f)
	  ///,
     PARA {
	  "We can undo the process with ", TO "homomorphism", "."
	  },      
     EXAMPLE lines ///
	  f' = homomorphism g
     	  f === f'
	  ///,
     SeeAlso => {homomorphism}
     }

document {
     Key => {homomorphism,(homomorphism, Matrix)},
     Headline => "get the homomorphism from element of Hom",
     Usage => "homomorphism f",
     Inputs => {
	  "f" => {"of the form Hom(M,N) <-- R^1, where Hom(M,N) has been
	  previously computed, and R is the ring of f, M and N"},
	  },
     Outputs => {
	  {"the ", TO "Matrix", " ", TT "M --> N", ", corresponding to the element ", TT "f", ""}
	  },
     EXAMPLE lines ///
	  R = QQ[x,y,z,Degrees=>{2,3,1}]/(y^2-x^3)
	  H = Hom(ideal(x,y), R^1)
	  f = H_{1}
	  g = homomorphism f
	  ///,
     "The source and target are what they should be.",
     EXAMPLE lines ///
	  source g === module ideal(x,y)
	  target g === R^1
	  ///,
     PARA {
	  "Except for a possible redistribution of degrees between the map and modules,
	  we can undo the process with ", TO "homomorphism'", "."
	  },      
     EXAMPLE lines ///
	  f' = homomorphism' g
	  f === f'
     	  f - f'
     	  degree f, degree f'
     	  degrees f, degrees f'
	  ///,
     PARA{
	  "After ", TO2((minimalPresentation,Module),"pruning"), " a Hom module, one cannot use 
	  homomorphism directly.  Instead, first apply the pruning map:"
	  },
     EXAMPLE lines ///
          H1 = prune H
	  homomorphism(H1.cache.pruningMap * H1_{1})
          ///,
     SeeAlso => {Hom,prune,random,basis}
     }

-----------------------------------------------------------------------------

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
