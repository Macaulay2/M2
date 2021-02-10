-- -*- coding: utf-8 -*-
--		Copyright 1993-2007 by Daniel R. Grayson 


document {
     Key => {(Ext,Module,Module),(Ext,Ideal,Ideal),(Ext,Ideal,Module),(Ext,Ideal,Ring),(Ext,Module,Ideal),(Ext,Module,Ring)},
     Headline => "total Ext module",
     Usage => "Ext(M,N)",
     Inputs => { "M" => {ofClass{Ideal,Ring}}, "N" => {ofClass{Ideal,Ring}}},
     Outputs => {
	  TEX { "the $Ext$ module of $M$ and $N$,
	       as a multigraded module, with the modules $Ext^i(M,N)$ for all values of $i$ appearing simultaneously." }},
     PARA { "The modules ", TT "M", " and ", TT "N", " should be graded (homogeneous) modules over the same ring." },
     PARA { "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way." },
     PARA TEX {
	  "The computation of the total Ext module is possible for modules over the
	  ring $R$ of a complete intersection, according the algorithm
	  of Shamash-Eisenbud-Avramov-Buchweitz.  The result is provided as a finitely
	  presented module over a new ring with one additional variable of degree
	  ", TT "{-2,-d}", " for each equation of degree ", TT "d", " defining $R$.  The 
	  variables in this new ring have degree length 1 more than the degree length of 
	  the original ring, i.e., is multigraded, with the
	  degree ", TT "d", " part of $Ext^n(M,N)$ appearing as the degree
	  ", TT "prepend(-n,d)", " part of ", TT "Ext(M,N)", ".  We illustrate this in 
	  the following example."
	  },
     EXAMPLE lines ///
     R = QQ[x,y]/(x^3,y^2);
     N = cokernel matrix {{x^2, x*y}}
     H = Ext(N,N);
     ring H
     S = ring H;
     H
     isHomogeneous H
     rank source basis( { -2,-3 }, H)
     rank source basis( { -3 }, Ext^2(N,N) )
     rank source basis( { -4,-5 }, H)
     rank source basis( { -5 }, Ext^4(N,N) )
     hilbertSeries H
     hilbertSeries(H,Order=>11)
     ///,
     PARA{ "The result of the computation is cached for future reference." }
     }

document {
     Key => {(Ext,ZZ,Module,Module),
	  (Ext, ZZ, Ideal, Ideal),
	  (Ext, ZZ, Ideal, Module),
	  (Ext, ZZ, Module, Ideal),
	  (Ext, ZZ, Ideal, Ring),
	  (Ext, ZZ, Module, Ring)
	  },
     Usage => "Ext^i(M,N)",
     Headline => "Ext module",
     Inputs => { "i", "M", "N" },
     Outputs => {
	  { "the ", TT "i", "-th ", TT "Ext", " module of ", TT "M", " and ", TT "N" }
	  },
     "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M = R^1/I
	  Ext^1(M,R)
	  Ext^2(M,R)
	  Ext^3(M,R)
	  Ext^1(I,R)
          ///,
     "As an efficiency consideration, it is generally much more efficient to compute
     Ext^i(R^1/I,N) rather than Ext^(i-1)(I,N).  The latter first computes a presentation 
     of the ideal I, and then a free resolution of that.  For many examples, the
     difference in time and space required can be very large.",
     SeeAlso => {resolution,Tor,Hom,monomialCurveIdeal,(Ext,ZZ,Matrix,Module),(Ext,ZZ,Module,Matrix)}
     }

document {
     Key => {(Ext, ZZ, Matrix, Module),
	  (Ext, ZZ, Matrix, Ideal),
	  (Ext, ZZ, Matrix, Ring)
	  },
     Usage => "Ext^i(f,N)",
     Headline => "map between Ext modules",
     Inputs => { "i", "f" => "M1 --> M2", "N" },
     Outputs => {
	  Matrix => {TEX ///the map $Ext^i(M2,N) \rightarrow{} Ext^i(M1,N)$///}
	  },
     "If ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M1 = R^1/I
	  M2 = R^1/ideal(I_0,I_1)
	  f = inducedMap(M1,M2)
	  Ext^1(f,R)
	  g = Ext^2(f,R)
	  source g == Ext^2(M1,R)
	  target g == Ext^2(M2,R)
	  Ext^3(f,R)
          ///,
     SeeAlso => {resolution,Tor,Hom,(Ext,ZZ,Module,Module)}
     }

document {
     Key => {(Ext, ZZ, Module, Matrix),
	  (Ext, ZZ, Ideal, Matrix)
	  },
     Usage => "Ext^i(M,f)",
     Headline => "map between Ext modules",
     Inputs => { "i", "M", "f" => "N1 --> N2"},
     Outputs => {
	  Matrix => {TEX ///the induced map $Ext^i(M,N1) \rightarrow{} Ext^i(M,N2)$///}
	  },
     "If ", TT "M", " is an ideal, it is regarded as a module in the evident way.",
     PARA{},
     -- the code for Hom(Module,Matrix) is wrong, so we disable this example temporarily
     -- EXAMPLE lines ///
     -- 	  R = ZZ/32003[a..d];
     -- 	  I = monomialCurveIdeal(R,{1,3,4})
     -- 	  M = R^1/I
     -- 	  f = map(R^1,module I,gens I)
     -- 	  Ext^1(M,f)
     -- 	  g = Ext^2(M,f)
     -- 	  source g == Ext^2(M,source f)
     -- 	  target g == Ext^2(M,target f)
     -- 	  Ext^3(f,R)
     --      ///,
     SeeAlso => {resolution,Tor,Hom,(Ext,ZZ,Module,Module),(Ext,ZZ,Matrix,Module)}
     }

document {
     Key => {
	  (Ext, ZZ, CoherentSheaf, SumOfTwists),
	  (Ext, ZZ, SheafOfRings, SumOfTwists)
	  },
     Usage => "Ext^i(M,N(>=d))",
     Headline => "global Ext",
     Inputs => { "i", "M", "N(>=d)"},
     Outputs => {
	  Module => {"The R-module ", TEX ///$\oplus_{j \geq d} Ext^i_X(M,N(j))$///}
	  },
     "If ", TT "M", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X = Proj R", ".",
     PARA{},
     "As an example, we consider the rational quartic curve in 
     ", TEX "$P^3$", ".",
     EXAMPLE lines ///
     	  S = QQ[a..d];
	  I = monomialCurveIdeal(S,{1,3,4})
	  R = S/I
	  X = Proj R
     	  IX = sheaf (module I ** R)
     	  Ext^1(IX,OO_X(>=-3))
	  Ext^0(IX,OO_X(>=-10))
	  ///,
     PARA{},
     "The method used may be found in: 
     Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
     PARA{},
     TEX ///If the vector space $Ext^i(M,N)$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,CoherentSheaf), ".",
     SeeAlso => {resolution,Tor,Hom,HH,sheafExt}
     }

document {
     Key => {
	  (Ext, ZZ, CoherentSheaf, CoherentSheaf),
	  (Ext, ZZ, SheafOfRings, CoherentSheaf),
	  (Ext, ZZ, CoherentSheaf, SheafOfRings),
	  (Ext, ZZ, SheafOfRings, SheafOfRings)
	  },
     Usage => "Ext^i(M,N)",
     Headline => "global Ext",
     Inputs => { "i", "M", "N"},
     Outputs => {
	  Module => {"The global Ext module ", TEX "$Ext^i_X(M,N)$"}
	  },
     "If ", TT "M", " or ", TT "N", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
     PARA{},
     "As an example, we compute Hom_X(I_X,OO_X), and Ext^1_X(I_X,OO_X), for the rational quartic curve in 
     ", TEX "$P^3$", ".",
     EXAMPLE lines ///
     	  S = QQ[a..d];
	  I = monomialCurveIdeal(S,{1,3,4})
	  R = S/I
	  X = Proj R
     	  IX = sheaf (module I ** R)
     	  Ext^1(IX,OO_X)
	  Hom(IX,OO_X)
	  ///,
     "The Ext^1 being zero says that the point corresponding to I on the Hilbert scheme is
     smooth (unobstructed), and vector space dimension of Hom tells us that the
     dimension of the component at the point I is 16.",
     PARA{},
     "The method used may be found in: 
     Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
     PARA{},
     TEX ///If the module $\oplus_{d\geq 0} Ext^i(M,N(d))$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,SumOfTwists), ".",
     SeeAlso => {resolution,Tor,Hom,HH,sheafExt,(Ext,ZZ,CoherentSheaf,SumOfTwists)}
     }

document {
     Key => Ext,
     Headline => "compute an Ext module"
     }

document {
     Key => dd,
     Headline => "differential in a chain complex",
     TT "dd", " -- a symbol used as a key in a chain complex, under which
     are stored the differentials.",
     PARA{},
     "The differentials are stored as the members in a chain complex
     map of degree ", TT "-1", ".  The map ", TT "C.dd_i", " is the
     one whose source is the module ", TT "C_i", ", and it is arranged to
     be zero if no map has been stored in that spot.",
     EXAMPLE {
	  "R = ZZ/101[a,b];",
      	  "C = resolution cokernel vars R",
      	  "C.dd",
      	  "C.dd_2",
	  },
     SeeAlso => "ChainComplex"
     }

document {
     Key => Tor,
     Headline => "Tor module"
     }
document {
     Key => {(Tor,ZZ,Module,Module),(Tor,ZZ,Module,Ring),(Tor,ZZ,Ideal,Ideal),(Tor,ZZ,Ideal,Module),(Tor,ZZ,Module,Ideal),(Tor,ZZ,Ideal,Ring)},
     Headline => "compute a Tor module",
     Usage => "Tor_i(M,N)",
     Inputs => { "i", "M", "N" },
     Outputs => {
	  { "the ", TT "i", "-th ", TT "Tor", " module of ", TT "M", " and ", TT "N" }
	  },
     PARA {
	  "If ", TT "N", " is a ring (instead of a module), then the free module of rank 1 over it is used instead.  If
	  ", TT "M", " or ", TT "N", " is an ideal of ring, then the underlying module is used instead."
	  }
     }

document {
     Key => MonomialIdeal, 
     Headline => "the class of all monomial ideals handled by the engine",
     "Monomial ideals are kinds of ideals, but many algorithms are much faster.
     Generally, any routines available for ideals are also available for monomial
     ideals.",
     EXAMPLE lines ///
         R = QQ[a..d];
	 I = monomialIdeal(a*b*c,b*c*d,a^2*d,b^3*c)
	 I^2
	 I + monomialIdeal(b*c)
	 I : monomialIdeal(b*c)
	 radical I
	 associatedPrimes I
	 primaryDecomposition I
     ///,
     HEADER3 "Specialized functions only available for monomial ideals",
     UL {
	  TO (borel,MonomialIdeal),
	  TO (isBorel,MonomialIdeal),
	  TO (symbol-, MonomialIdeal, MonomialIdeal),
	  TO (dual, MonomialIdeal),
	  TO independentSets,
	  TO "PrimaryDecomposition::irreducibleDecomposition",
	  TO standardPairs,
	  },
     EXAMPLE lines ///
         borel I
	 isBorel I
	 I - monomialIdeal(b^3*c,b^4)
	 standardPairs I
	 independentSets I
	 dual I
     ///,
     "The ring of a monomial ideal must be a commutative polynomial ring.  This ring must
     not be a skew commuting ring, and/or a quotient ring.",
     }

document {
     Key => (symbol -, MonomialIdeal, MonomialIdeal),
     Headline => "monomial ideal difference",
     Usage => "I - J",
     Inputs => {
	  "I", "J"
	  },
     Outputs => { 
	  MonomialIdeal => {"generated by those minimal generators of I which are not in J"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  I = monomialIdeal(a^3,b^2,a*b*c)
	  J = monomialIdeal(a^2,b^3,a*b*c)
	  I - J
	  J - I
	  I - (I-J)
          ///,
     SeeAlso => {MonomialIdeal}
     }

--document { (complement, MonomialIdeal),
--					  -- compute { m^c : m minimal generator of I }, where
--					  -- m^c is the product of the variables 
--					  -- not in the support of m.",
--     TT "complement I", " -- computes the complement of a monomial ideal.",
--     PARA{},
--     "The complement of ", TT "I", " is defined to be the ideal generated by those monomials
--     which have no factor in common with some monomial of ", TT "I", ".",
--     SeeAlso => MonomialIdeal
--     }
--
--document { (complement, MonomialIdeal, Monomial),
--					  -- (I,p) -- compute { p/m : m in I }, where p is a given monomial",
--     TT "complement(I,p)", " -- ",
--     SeeAlso => MonomialIdeal
--     }     
--

document {
     Key => {[(dual,MonomialIdeal),Strategy], [(dual,MonomialIdeal,List),Strategy], [(dual,MonomialIdeal,RingElement),Strategy]},
     PARA {
	  "Specify ", TT "Strategy => 1", " to test an older strategy for performing the computation."
	  }
     }

document {
     Key => (dual,MonomialIdeal,List),
     Headline => "the Alexander dual",
     Usage => "dual(I,e)",
     Inputs => {
	  "I" => {"a monomial ideal"},
	  "e" => {"a list of exponents"}
	  },
     Outputs => { {"the Alexander dual of ", TT "I", " formed with respect to the monomial whose exponents are listed in ", TT "e" } },
     PARA { "The monomial corresponding to ", TT "e", " should be divisible by all of the minimal generators of ", TT "I", "." },
     PARA {
	  "The computation is done by calling the ", TO "frobby", " library, written by B. H. Roune;
	  setting ", TO "gbTrace", " to a positive value will cause a message to be printed when it is called."
	  },
     SeeAlso => {(dual,MonomialIdeal)}
     }

document {
     Key => (dual,MonomialIdeal,RingElement),
     Headline => "the Alexander dual",
     Usage => "dual(I,m)",
     Inputs => {
	  "I" => {"a monomial ideal"},
	  "m" => {"a monomial"}
	  },
     Outputs => { {"the Alexander dual of ", TT "I", " formed with respect to ", TT "m", "." } },
     "The monomial ", TT "m", " should be divisible by all
     of the minimal generators of ", TT "I", ".",
     SeeAlso => {(dual,MonomialIdeal)}
     }

document {
     Key => (dual,MonomialIdeal),
     Headline => "the Alexander dual of a monomial ideal",
     Usage => "dual I",
     Inputs => {
	  "I" => {"a monomial ideal"}
	  },
     Outputs => {
	  {"the Alexander dual of ", TT "I"}
	  },
     "If ", TT "I", "is a square free monomial ideal then ",
     TT "I", " is the Stanley-Reisner ideal of a simplicial 
     complex.  In this case, ", TT "dual I", " is the 
     Stanley-Reisner ideal associated to the dual complex.  
     In particular, ", TT "dual I", " is obtained by 
     switching the roles of minimal generators and prime 
     components.",
     EXAMPLE {
	  "QQ[a,b,c,d];",
	  "I = monomialIdeal(a*b, b*c, c*d)",
	  "dual I",
	  "intersect(monomialIdeal(a,b), 
	       monomialIdeal(b,c),
	       monomialIdeal(c,d))",
	  "dual dual I" 
	  },
     PARA{},
     "For a general monomial ideal, the Alexander dual 
     defined as follows:  Given two list of nonnegative 
     integers ", TT "a", " and ", TT "b", "for which ", 
     TT "a_i >= b_i", " for all ", TT "i", " let ", 
     TT "a\\b", " denote the list whose ", TT "i", "-th 
     entry is ", TT "a_i+1-b_i", "if ", TT "b_i >= 1", 
     "and ", TT "0", "otherwise.  The Alexander dual with 
     respect to ", TT "a", " is the ideal generated by 
     a monomial ", TT "x^a\\b", " for each irreducible 
     component ", TT "(x_i^b_i)", " of ", TT "I", ".  
     If ", TT "a", " is not provided, it is assumed to be 
     the least common multiple of the minimal generators
     of ", TT "I", ".",
     EXAMPLE {
     	  "QQ[x,y,z];",
	  "I = monomialIdeal(x^3, x*y, y*z^2)", 
	  "dual(I, {4,4,4})",
	  "intersect( monomialIdeal(x^2),
	       monomialIdeal(x^4, y^4),
	       monomialIdeal(y^4, z^3))",
	  },  
     PARA{},
     "One always has ", TT "dual( dual(I, a), a) == I", 
     " however ", TT "dual dual I", "may not equal ", 
     TT "I", ".",
     EXAMPLE lines ///
	  QQ[x,y,z];
	  J = monomialIdeal( x^3*y^2, x*y^4, x*z, y^2*z)
	  dual dual J
	  dual( dual(J, {3,4,1}), {3,4,1})
	  ///,
     PARA{},
     "See Ezra Miller's Ph.D. thesis 'Resolutions and 
     Duality for Monomial Ideals'.",
     PARA { "Implemented by Greg Smith." },
     PARA {
	  "The computation is done by calling the ", TO "frobby", " library, written by B. H. Roune;
	  setting ", TO "gbTrace", " to a positive value will cause a message to be printed when it is called."
	  }
     }

document {
     Key => {
	  monomialSubideal,
	  (monomialSubideal, Ideal)},
     Headline => "find the largest monomial ideal in an ideal",
     Usage => "monomialSubideal I",
     Inputs => {
	  "I" => Ideal
	  },
     Outputs => {
	  MonomialIdeal => {"the largest monomial ideal contained in ", TT "I"}
	  },
     EXAMPLE lines ///
	  QQ[a,b,c,d];
	  I = ideal(b*c, c^2 - b*d, -a*c+b^2)
	  monomialSubideal I
	  ///,
     PARA{},
     "Implemented by Greg Smith."
     }

document {
     Key => {isBorel,(isBorel, MonomialIdeal)},
     Headline => "whether an ideal is fixed by upper triangular changes of coordinates"
     }

document {
     Key => GradedModule,
     Headline => "the class of all graded modules",
     "A new graded module can be made with 'M = new GradedModule'.
     The i-th module can be installed with a statement like ", TT "M#i=N", ",
     and can be retrieved with an expression like ", TT "M_i", ".  The ground
     ring should be installed with a statement like ", TT "M.ring = R", ".",
     SeeAlso => "GradedModuleMap"
     }

document {
     Key => GradedModuleMap,
     Headline => "the class of all maps between graded modules",
     SeeAlso => "GradedModule"
     }

document {
     Key => {gradedModule,(gradedModule, Module), (gradedModule, List), (gradedModule, Sequence), (gradedModule, ChainComplex)},
     Headline => "make a graded module",
     Usage => "gradedModule v",
     Inputs => { "v" => List => "a module, or a list or sequence of modules" },
     Outputs => {{"the graded module with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModule ZZ^2
     	  gradedModule(ZZ^2,ZZ^3,ZZ^400)
     ///,
     "If ", TT "v", " is ", ofClass ChainComplex, " then the return value is the graded module underlying it.",
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  C = res coker vars R
	  gradedModule C
     ///,
     }

document {
     Key => {gradedModuleMap,(gradedModuleMap, Sequence), (gradedModuleMap, ModuleMap), (gradedModuleMap, List)},
     Headline => "make a map of graded modules",
     Usage => "gradedModuleMap v",
     Inputs => { "v" => List => "a matrix, or a list or sequence of matrices" },
     Outputs => {{"the graded module map with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModuleMap(random(ZZ^3,ZZ^4),random(ZZ^2,ZZ^2))
     ///
     }

document {
     Key => ChainComplex,
     Headline => "the class of all chain complexes",
     "For an overview of creating and using chain complexes in Macaulay2, see ",
     TO "chain complexes", ".",
     PARA{},
     "Common ways to create a chain complex",
     UL {
	  TO chainComplex,
	  TO (resolution,Module),
	  },
     "Information about a chain complex",
     UL {
	  TO (length,ChainComplex),
	  TO (min,GradedModule),
	  TO (max,GradedModule),
	  TO (betti,GradedModule),
	  TO (ring,ChainComplex)
	  },
     "Operations on chain complexes",
     UL {
	  TO (dual,ChainComplex),
	  TO (symbol++,ChainComplex,ChainComplex),
	  TO (symbol**,ChainComplex,Module),
	  TO (symbol**,ChainComplex,ChainComplex),
	  TO (Hom,ChainComplex,Module),
	  TO (symbol SPACE, ChainComplex, Array)
	  },
     }

document {
     Key => (complete, ChainComplex),
     Headline => "complete the internal parts",
     TT "complete C", " -- fills in the modules of a chain complex
     obtained as a resolution with information from the engine.",
     PARA{},
     "For the sake of efficiency, when a chain complex arises as
     a resolution of a module, the free modules are not filled in until
     they are needed.  This routine can be used to fill them all in, and
     is called internally when a chain complex is printed.
     Normally users will not need this function, unless they use ", TO "#", " to
     obtain the modules of the chain complex, or use ", TO "keys", " to
     see which spots are occupied by modules.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = resolution cokernel vars R;",
      	  "keys C",
      	  "complete C;",
      	  "keys C"
	  }
     }


document {
     Key => (length, ChainComplex),
     Headline => "length of a chain complex or graded module",
     TT "length C", " -- the length of a chain complex.",
     PARA{},
     "The length of a chain complex is defined to be the difference
     between the smallest and largest indices of spots occupied by
     modules, even if those modules happen to be zero."
     }

document {
     Key => ChainComplexMap,
     Headline => "the class of all maps between chain complexes",
     "The usual algebraic operations are available: addition, subtraction,
     scalar multiplication, and composition.  The identity map from a
     chain complex to itself can be produced with ", TO "id", ".  An
     attempt to add (subtract, or compare) a ring element to a chain complex
     will result in the ring element being multiplied by the appropriate
     identity map."
     }


document { Key => {extend},
     Headline => "extend a module map to a chain map, if possible" }
document {
     Key => {(extend,ChainComplex,ChainComplex,Matrix),[extend,Verify]},
     Usage => "extend(D,C,f0)",
     Inputs => {
	  "D",
	  "C",
	  "f0" => { "a map from ", TT "C_0", " to ", TT "D_0" },
	  Verify => Boolean => {"whether to check that the map extends"}
	  },
     Outputs => { { "a chain complex map ", TT "f: D <--- C", " of degree 0 extending ", TT "f0", " in the sense that ", TT "f_0==f0" } },
     EXAMPLE {
	  "R = ZZ/101[a..c]",
	  "I = image vars R",
	  "J = image symmetricPower (2,vars R)",
	  "g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))",
	  "g_1",
	  "g_2"
	  },
     SeeAlso => {(symbol _,ChainComplex,ZZ),(symbol _,ChainComplexMap,ZZ),(cone,ChainComplexMap) }
     }

document { Key => cone,
     Headline => "mapping cone of a chain map" }
document {
     Key => (cone,ChainComplexMap),
     Usage => "cone f",
     Inputs => { "f" },
     Outputs => { {"the mapping cone of a ", TT "f", ""} },
     {
	  EXAMPLE {
	       "R = ZZ/101[x,y,z]",
	       "m = image vars R",
	       "m2 = image symmetricPower(2,vars R)",
	       "M = R^1/m2",
	       "N = R^1/m",
	       "C = cone extend(resolution N,resolution M,id_(R^1))",
	       },
	  "Let's check that the homology is correct; for example, ", TT "HH_0", " should be zero.",
	  EXAMPLE "prune HH_0 C",
	  "Let's check that ", TT "HH_1", " is isomorphic to ", TT "m/m2", ".",
	  EXAMPLE {
	       "prune HH_1 C",
	       "prune (m/m2)"
	       }
	  }}

document {
     Key => {nullhomotopy,(nullhomotopy, ChainComplexMap)},
     Headline => "make a null homotopy",
     TT "nullhomotopy f", " -- produce a nullhomotopy for a map f of 
     chain complexes.",
     PARA{}, 
     "Whether f is null homotopic is not checked.",
     PARA{},
     "Here is part of an example provided by Luchezar Avramov.  We
     construct a random module over a complete intersection, resolve 
     it over the polynomial ring, and produce a null homotopy for the
     map that is multiplication by one of the defining equations
     for the complete intersection.",
     EXAMPLE {
	  "A = ZZ/101[x,y];",
      	  "M = cokernel random(A^3, A^{-2,-2})",
      	  "R = cokernel matrix {{x^3,y^4}}",
      	  "N = prune (M**R)",
      	  "C = resolution N",
      	  "d = C.dd",
      	  "s = nullhomotopy (x^3 * id_C)",
      	  "s*d + d*s",
      	  "s^2"
	  },
     }

document {
     Key => {(symbol ++,ChainComplex,ChainComplex),(symbol ++,GradedModule,GradedModule),
	  (symbol ++,ChainComplexMap,ChainComplexMap)},
     Headline => "direct sum",
     TT "C++D", " -- direct sum of chain complexes and maps",
     EXAMPLE lines ///
	  C = resolution cokernel matrix {{4,5}}
      	  C ++ C[-2]
     ///
     }

document {
     Key => (components, ChainComplex),
     TT "components C", " -- returns from which C was formed, if C
     was formed as a direct sum.",
     PARA{},
     SeeAlso => "isDirectSum"
     }

document {
     Key => (symbol SPACE, ChainComplex, Array),
     Headline => "degree shift",
     Usage => "D = C[i]",
     Inputs => {
	  "C" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "D" => {"a new chain complex ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", ".  The signs of the
	       differentials are reversed if ", TT "i", " is odd."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R
     C[3]
     ///
     }

document {
     Key => (symbol SPACE, ChainComplexMap, Array),
     Headline => "degree shift",
     Usage => "g = f[i]",
     Inputs => {
	  "f" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "g" => {"a new chain complex map ", TT "g", " in which ", TT "g_j", " is ", TT "f_(i+j)", "."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R;
     f = id_C
     f[3]
     ///
     }

document {
     Key => (symbol SPACE, GradedModule, Array),
     Headline => "degree shift",
     TT "C[i]", " -- shifts the graded module ", TT "C", ", producing a new graded module
     ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", "."
     }

document {
     Key => (dual, ChainComplex),
     Headline => "dual",
     TT "dual C", " -- the dual of a chain complex."
     }

document {
     Key => syzygyScheme,
     Headline => "construct a syzygy scheme",
     TT "syzygyScheme(C,i,v)", " -- produce the syzygy scheme from a map
     ", TT "v : R^j ---> C_i", " which selects some syzygies from a resolution ", TT "C", "."
     }

document {
     Key => (sum, ChainComplex),
     Headline => "direct sum of the components of a chain complex",
     TT "sum C", " -- yields the sum of the modules in a chain complex.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = res coker vars R",
      	  "sum C",
      	  "degrees oo",
	  },
     SeeAlso => {"sum",(sum, ChainComplexMap)}
     }

document {
     Key => (sum, ChainComplexMap),
     Headline => "direct sum of the components of a chain map",
     TT "sum C", " -- yields the sum of the modules in a chain complex map.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "C = res coker vars R",
      	  "sum C.dd",
      	  "betti oo",
	  },
     SeeAlso => {"sum", (sum, ChainComplex)}
     }

document {
     Key => (NewMethod, ChainComplex),
     Headline => "make a new chain complex from scratch",
     TT "C = new ChainComplex", " -- make a new chain complex.",
     PARA{},
     "The new chain complex is initialized with a differential of
     degree ", TT "-1", " accessible as ", TT "C.dd", " and of type
     ", TO "ChainComplexMap", ".  You can take the new chain complex and
     fill in the ring, the modules, and the differentials.",
     EXAMPLE {
	  "C = new ChainComplex;",
      	  "C.ring = ZZ;",
      	  "C#2 = ZZ^1;",
      	  "C#3 = ZZ^2;",
      	  "C.dd#3 = matrix {{3,-11}};",
      	  "C",
      	  "C.dd"
	  },
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplex), (symbol**, ChainComplex, Module),(symbol**, Module, ChainComplex)},
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of two chain complexes.",
     PARA{},
     "The result, ", TT "E", ", is a chain complex.  Each module ", TT "E_k", " 
     in it is a direct sum of modules of the form ", TT "C_i**D_j", " with
     ", TT "i+j=k", ", and the preferred keys for the components of this direct
     sum are the pairs ", TT "(i,j)", ", sorted increasing values of ", TT "i", ".  For
     information about how to use preferred keys, see ", TO "directSum", "."
     }

document {
     Key => (symbol **, ChainComplex, GradedModule),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a chain complex with a graded module.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => (symbol **, GradedModule, ChainComplex),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a graded module with a chain complex.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => {(symbol **, ChainComplexMap, ChainComplex),(symbol**, ChainComplexMap, Module)},
     Headline => "tensor product",
     TT "f ** C", " -- tensor product of a map of chain complexes with a chain complex.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplexMap),(symbol**, Module, ChainComplexMap)},
     Headline => "tensor product",
     TT "C ** f", " -- tensor product of a chain complex with a map of chain complexes.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (symbol **,ChainComplex,Ring),
     Usage => "C ** R",
     Inputs => {"C","R"},
     Outputs => {{"the tensor product of ", TT "C", " with ", TT "R" }},
     EXAMPLE lines ///
     R = QQ[a..d];
     C = res coker vars R
     S = R[x]
     C**S
     ///
     }

document {
     Key => (symbol **, ChainComplexMap, ChainComplexMap),
     Headline => "tensor product",
     TT "f ** g", " -- tensor product of two maps of chain complexes.",
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => {tensorAssociativity,
	  (tensorAssociativity, Module, Module, Module),
	  (tensorAssociativity, ChainComplex, ChainComplex, ChainComplex),
	  (tensorAssociativity, GradedModule, GradedModule, GradedModule)},
     Headline => "associativity isomorphisms for tensor products",
     TT "tensorAssociativity(A,B,C)", " -- produces the isomorphism from
     A**(B**C) to (A**B)**C.",
     PARA{},
     "Currently implemented for modules, graded modules, and chain complexes.",
     SeeAlso => {"ChainComplex", "Module"}
     }

document {
     Key => (symbol SPACE, Module, Array),
     Headline => "make a chain complex from a module",
     TT "M[n]", " -- create a chain complex with the module M concentrated
     in degree -n.",
     PARA{},
     SeeAlso => "ChainComplex"
     }


document {
     Key => (dual,ChainComplexMap),
     Headline => "dual of a chain complex",
     Usage => "dual C",
     Inputs => { "C" },
     Outputs => { { "the dual of the chain complex ", TT "C", "" } },
     EXAMPLE {
	  "R = QQ[a..f]",
	  "M = coker genericMatrix(R,a,2,3)",
	  "res M",
	  "dual oo"
	  }
     }



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
