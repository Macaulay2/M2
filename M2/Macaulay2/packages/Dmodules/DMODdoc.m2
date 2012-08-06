-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

TEST /// input "Dmodules/TST/AnnFs.tst.m2" ///
TEST /// input "Dmodules/TST/DHom.tst.m2" ///
TEST /// input "Dmodules/TST/Dbasic.tst.m2" ///
TEST /// input "Dmodules/TST/Ddual.tst.m2" ///
TEST /// input "Dmodules/TST/DeRham.tst.m2" ///
TEST /// input "Dmodules/TST/Dlocalize.tst.m2" ///
TEST /// input "Dmodules/TST/Dresolution.tst.m2" ///
TEST /// input "Dmodules/TST/Drestriction.tst.m2" ///
TEST /// input "Dmodules/TST/WeylClosure.tst.m2" ///
TEST /// input "Dmodules/TST/b-function.ideal.tst.m2" ///
TEST /// input "Dmodules/TST/b-function.module.tst.m2" ///
TEST /// input "Dmodules/TST/localCohom.tst.m2" ///
TEST /// input "Dmodules/TST/makeCyclic.tst.m2" ///
TEST /// input "Dmodules/TST/paramBpoly.tst.m2" ///
TEST /// input "Dmodules/TST/stafford.tst.m2" ///
TEST /// input "Dmodules/TST/CC.tst.m2" ///
TEST /// input "Dmodules/TST/localBFunction.tst.m2" ///
TEST /// input "Dmodules/TST/multiplierIdeals.tst.m2" ///

document {
     Key => "Dmodules",
     Headline => "algorithms for D-modules",
     HEADER3 "How to make Weyl algebras:",
     UL{TO {"WeylAlgebra", " -- 
	       The class of Weyl algebras"}},
     
     HEADER3 "Basic commands:",
     UL{
	  TO {"gbw", " -- Groebner bases with respect to weight vectors"},
	  TO {"inw", " -- initial ideals with respect to weight vectors"},
	  TO {"Fourier", " -- Fourier transform"},
	  TO {"Dtransposition", " -- standard transposition"},
	  TO {"makeCyclic", " -- cyclic presentation"},
	  TO {"makeWeylAlgebra", 
	       " -- Weyl algebra associated to a polynomial ring"},
	  TO {"stafford", " -- compute 2 generators for an ideal in the Weyl algebra"}
	  },
     HEADER3 "Some examples of D-modules:",
     UL{TO {"gkz", " -- Gelfand-Kapranov-Zelevinsky hypergeometric system"},
	  TO {"AppellF1", " -- Appell F1 system"},
	  TO {"PolyAnn", " -- annihilator of a polynomial"},
	  TO {"RatAnn", " -- annihilator of a rational function"}},
     HEADER3 "Basic invariants of D-modules:",
     UL{TO {"Ddim", " -- dimension"}, 
	  TO {"holonomicRank"," -- holonomic rank"}, 
	  TO {"charIdeal", " -- characteristic ideal"},
	  TO {"singLocus", " -- singular locus"}},
     HEADER3 "B-functions:",
     UL {
	  TO {"bFunction", " -- b-function"}, 
	  TO {"globalBFunction", " -- global b-function"},
	  TO {"globalB", " -- global b-function and b-operator"},
	  TO {"globalBoperator", " -- global b-operator"},
	  TO {"generalB", " -- generalized Bernstein-Sato polynomial"},
	  TO {"localBFunction", " -- local b-function"},
	  TO {"paramBpoly", " -- Bernstein-Sato polynomials of a
	       polynomial with parametric coefficients"},
	  TO {"factorBFunction", " -- factors a univariate polynomial"},
	  TO {"bFunctionRoots", " -- gets roots of a b-function"},
	  TO {"getIntRoots", " -- gets integer roots of a b-function"},
	  {TO {"AnnFs"},  " -- annihilator ideal of ", EM "f", SUP "s" },
	  {TO {"AnnIFs"}, " -- annihilator ideal  of ", EM "f", SUP "s", 
	       " for an arbitrary D-module"}
	  },
     HEADER3 "Resolutions and Functors:",
     UL{TO {"Dresolution", " -- resolutions"}, 
	  TO {"Dlocalize", " -- localization"}, 
     	  TO {"WeylClosure", " -- Weyl closure"},
	  TO {"Ddual", " -- holonomic dual"}, 
	  TO {"Drestriction", " -- derived restriction"},
	  TO {"Dintegration", " -- derived integration"},
	  TO {"DHom", " -- homomorphisms between holonomic D-modules"},
	  TO {"DExt", " -- Ext between holonomic D-modules"},
	  TO {"PolyExt", 
	       " -- Ext between a holonomic D-module and a polynomial ring"},
	  TO {"RatExt", 
	       " -- rational Ext"}
	  },     
     HEADER3 "Applications:",
     UL{
	  TO {"localCohom", "-- local cohomology"},
	  TO {"deRham", " -- deRham cohomology"},
	  TO {"PolySols", " -- polynomial solutions of finite rank systems"},
	  TO {"RatSols", " -- rational solutions of finite rank systems"},
	  TO {"diffOps", " -- differential operators on affine varieties"},
	  {TO "populateCechComplexCC", ", ", TO "pruneCechComplexCC", 
	       " -- characteristic cycles of local cohomology"},
	  TO {"logCohomology", " -- logarithmic cohomology groups"},
	  TO {"lct", "-- log canonical threshold"},
	  {TO "multiplierIdeal", ", ", TO "isInMultiplierIdeal", ", ", TO "jumpingCoefficients", " -- multiplier ideals"},
	  TO "hasRationalSing"
	  },
     HEADER3 "Programming aids:",
     UL{TO {"createDpairs", " -- tags coordinate and derivation variables"},
	  TO {"Dtrace", " -- toggles verbose comments"},
	  TO {"setHomSwitch", " -- toggles use of homogeneous Weyl algebra"}}
     }
-----------------------------------------------

scan({
    -- some optional arguments
    	 SetVariables
         },
    s -> if s =!= null then document {
         Key => s,
         Headline => "name for an optional argument",
         "A symbol used as the name of an optional argument, for some function(s)."
         }
    )

document {
     Key => bFunction,
     Headline => "b-function",
     UL {
	  {TO (bFunction, Ideal,List), " - for an ideal"},
	  {TO (bFunction, Module,List,List), " - for a module"}  
	  }
     }

document {
     Key => [bFunction,Strategy],
     Headline => "specify strategy for computing b-function",
     UL { 
	  {BOLD "IntRing", " -- the simplest algorithm available. 
     	       The idea is to compute ", EM "in", SUB "(-w,w)", EM "(I) ", "
     	       intersect it with ", 
	       EM {"k[t", SUB "1", ",...,t", SUB "n", "]"}, 
	       "(", EM {"t", SUB "i", " = x", SUB "i", "D", SUB "i"}, ")",
     	       "Call the ideal obtained ", EM "J", ". 
	       Finally ", 
	       EM {"J + (t", SUB "1", 
		    " + ... + t", SUB "n", "- s) \\cap k[s]"},
	       " is generated by the b-function that we are looking for."
	       },
	  {BOLD "TryGeneric", " -- checks whether the ideal is generic 
	       and if that is the case uses Alg.5.1.5 
	       in Saito-Sturmfels-Takayama (1999) otherwise is eqivalent 
	       to ", TT "NonGeneric",
     	       },
	  {BOLD "NonGeneric", 
	       " -- uses 5.1.6 in Saito-Sturmfels-Takayama (1999)"
	       },
	  {"Default:", BOLD "IntRing"}
	  }
     }
document {
     Key => NonGeneric,
     Headline => "a strategy option for b-functions",
     "see ", TO "bFunction"
     }
document {
     Key => TryGeneric,
     Headline => "a strategy option for b-functions",
     "see ", TO "bFunction"
     }
document {
     Key => IntRing,
     Headline => "a strategy option for b-functions",
     "see ", TO "bFunction"
     }
document {
     Key => (bFunction, Ideal, List),
     Headline => "b-function of an ideal",
     Usage => "b = bFunction(I,w)",
     Inputs => {
	  "I" => {"a holonomic ideal in the Weyl algebra ", 
	       EM {"A", SUB "n", "(K)"}, "."},
	  "w" => {"a list of integer weights corresponding 
	       to the differential variables in the Weyl algebra."}
	       },
    Outputs => {
	  "b" => {"a polynomial ", EM "b(s)", " which is the b-function of ", 
	       EM "I", " with respect to ", EM "w"}
	  },
     "Use ", TO "setHomSwitch", "(true) to force all the subroutines 
     to use homogenized ", TO "WeylAlgebra",
     PARA {  
	  BOLD "Definition. ", "The b-function ", EM "b(s)", 
	  " is defined as the monic generator  
	  of the intersection of ", 
	  EM {"in", SUB "(-w,w)", "(I)"}, " and ", 
	  EM "K[s]",
	  ", where ", 
	  EM {"s = [w", SUB "1", "t", SUB "1", " + ... + w", 
	       SUB "n", "t", SUB "n", "]"}, 
	  " (here ", EM {"t", SUB "i", " = x", SUB "i", "D", SUB "i"}, ")."}, 
     EXAMPLE {
	  "R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]",
     	  "I = ideal(x_1, D_2-1)",
     	  "bFunction(I,{1, 0})",
     	  },
     Caveat => {
	  "The ring of I should not have any parameters: 
     	  it should be a pure Weyl algebra. Similarly, this ring 
	  should not be a homogeneous ", TO "WeylAlgebra"
	  },
     SeeAlso => { "globalBFunction", "factorBFunction" }
     }

document {
     Key => (bFunction, Module, List, List),
     Headline => "b-function of a holonomic D-module",
     Usage => "b = bFunction(M,w,m)",
     Inputs => {
	  "M" => {"a holonomic module over a Weyl algebra ", 
	       EM {"A", SUB "n", "(K)"}},
	  "w" => {"a list of integer weights corresponding 
	       to the differential variables in the Weyl algebra"},
	  "m" => {"a list of integers, each of which is 
	       the shift for the corresponding component"}
	       },
     Outputs => {
	  "b" => {"a polynomial ", EM "b(s)", " which is the b-function of ", 
	       	EM "M", " with respect to ", EM "w", 
	       	" and ", EM "m"}
	  },
     "The algorithm represents ", EM "M", " as ", EM "F/N", 
     " where ", EM "F", " is free and ", 
     EM "N", " is a submodule of ", EM "F", ". 
     Then it computes b-functions ", EM {"b", SUB "i", "(s)"}, 
     " for ", EM {"N \\cap F", SUB "i"}, " (i-th component of ", EM "F", 
     ") and outputs ",
     EM {"lcm{ b", SUB "i", "(s-m", SUB "i",") }"},
     EXAMPLE{
	  "R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
	  "M = cokernel matrix {{x^2, 0, 0}, {0, dx^3, 0}, {0, 0, x^3}}",
	  "factorBFunction bFunction(M, {1}, {0,0,0})",
	  "factorBFunction bFunction(M, {1}, {1,2,3})"
     	  },
     Caveat => {
	  "The Weyl algebra should not have any parameters. 
     	  Similarly, it should not be a homogeneous Weyl algebra"
	  },
     SeeAlso => { "globalBFunction", "factorBFunction" }
     }


document {
     Key => [globalBFunction,Strategy],
     Headline => "specify strategy for computing global b-function",
     UL { 
	  {BOLD "IntRing, TryGeneric, NonGeneric", 
	       " -- passed to ", TO "bFunction",  ", see ", 
	       TO [bFunction,Strategy] },
	  {BOLD "ViaAnnFs", " -- computes ", 
	       EM "J(s)=Ann(f", SUP "s", EM ")", " and then intersects ", 
	       EM "J(s)+D[s]f}", " with ", EM "K[s]"},
	  {BOLD "ReducedB", " -- computes ", EM "b(s)/(s+1)", 
	       " by taking the intersection of ",
	       EM "J(s)+D[s](f,df/dx1,...,df/dxn)", " with ", EM "K[s]",
	       ", then multiplies by ", EM "s+1", "."},
	  {BOLD "GeneralBernsteinSato", " -- calls ", TO "generalB", "{f}."},
	  {"Default: ", BOLD "GeneralBernsteinSato"}
	  }
     }

document {
     Key => ViaAnnFs,
     Headline => "a strategy option for global b-functions",
     "see ", TO "globalBFunction"
     }
document {
     Key => ReducedB,
     Headline => "a strategy option for global b-functions",
     "see ", TO "globalBFunction"
     }

document { 
     Key => {(globalBFunction,RingElement), globalBFunction},
     Headline => "global b-function (else known as the Bernstein-Sato polynomial)",
     Usage => "b = globalBFunction f",
     Inputs => {
	  "f" => {"a polynomial"}
	  },
     Outputs => {
	  "b" => RingElement => {"the b-function ", EM "b(s)",  " in ", EM "Q[s]"}
	  },
     PARA {
	  BOLD "Definition. ", "Let ", 
	  EM "D = A_{2n}(K) = K<x_1,...,x_n,d_1,...,d_n>", 
	  " be a Weyl algebra. 
	  The Bernstein-Sato polynomial of a polynomial f is defined 
	  to be the monic generator of the ideal of all polynomials ", 
	  EM "b(s)", " in ", EM "K[s]", " such that
	  ", EM " b(s) f^s = Q(s,x,d) f^{s+1}", " where ", EM "Q", 
	  " lives in ", EM "D[s]."},
     PARA {
	  BOLD "Algorithm. ", 
	  "Let ", 
	  EM "I_f = D<t,dt>*<t-f, d_1+df/dx_1*dt, ..., d_n+df/dx_n*dt> ",
	  "Let ", EM "B(s) = bFunction(I, {1, 0, ..., 0})", 
	  " where 1 in the weight that corresponds to ", EM "dt. ", 
	  "Then the global b-function is ", EM "b_f = B(-s-1)"},
     EXAMPLE lines ///
	  R = QQ[x]
     	  f = x^10
    	  b = globalBFunction f
	  factorBFunction b
     	  ///,
     SeeAlso => { "bFunction", "factorBFunction", "generalB", "globalB" }
     }

document { 
     Key => {(generalB,List,RingElement), (generalB,List), generalB},
     Headline => "global generalized Bernstein-Sato polynomial",
     Usage => "b = generalB(F,g), b = generalB F",
     Inputs => {
	  "F" => {"a list of polynomials"},
	  "g" => {"a polynomial"}
	  },
     Outputs => {
	  "b" => RingElement => {"the general Bernstein-Sato polynomial ", 
	       EM "b(s)",  " in ", EM "Q[s]"}
	  },
     "Bernstein-Sato polynomial for an arbitrary affine variety was introduced in ",
     "Budur, Mustata, and Saito ", "``Bernstein--Sato polynomials of arbitrary varieties''. ",
     "If the option ", TO "Exponent", " is specified, then the m-generalized Bernstein-Sato polynomial is computed. ",
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for definitions.",     
     EXAMPLE lines ///
     W = makeWA(QQ[x_1..x_3]);
     factorBFunction generalB ({x_2^2-x_1*x_3, x_1^3-x_3^2}, x_2)
     ///,
     Caveat => {
	  "The input could be either in a polynomial ring or the Weyl algebra. In the latter case the algebra 
	  should not have any central variables and should not be a homogeneous Weyl algebra."
	  },
     SeeAlso => { "bFunction", "globalBFunction", "lct", "multiplierIdeal" }
     }
document {
     Key => {[generalB,Strategy], InitialIdeal, StarIdeal},
     Headline => "specify strategy for computing generalized Bernstein-Sato polynomial",
     UL { 
	  { BOLD "InitialIdeal", 
	       " -- use the initial ideal in_{(-w,w)} Ann f^s; this Strategy is the fastest on many examples, 
	       but can not be used with the ", TO "Exponent", " option." },
	  { BOLD "StarIdeal", 
	       " -- use ``star ideal'', the (-w,w) homogeneous elements of the Ann f^s"}
	  },
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals''."
     }
document {
     Key => {[generalB,Exponent], Exponent},
     Headline => "specify exponent m for m-generalized Bernstein-Sato polynomial",
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals''."
     }
document {
     Key => ViaLinearAlgebra,
     Headline => "an option for generalB=>Strategy",
     "see ", TO "generalB"
     } 
document { 
     Key => {(lct,Ideal), lct},
     Headline => "compute the log canonical threshold for an ideal",
     Usage => "l = lct I",
     Inputs => {
	  "I" => {"an ideal in a polynomial ring"}
	  },
     Outputs => {
	  "l" => RingElement => {"a rational number, the log canonical threshold of ", EM "I"}
	  },
     EXAMPLE lines ///
     QQ[x_1..x_3];
     I = ideal (x_2^2-x_1*x_3, x_1^3-x_3^2);
     lct I
     ///,
     SeeAlso => { "bFunction", "generalB" }
     }
document {
     Key => [lct,Strategy],
     Headline => "specify strategy for computing lct",
     UL { 
	  {BOLD "ViaBFunction", " -- use ", TO "bFunction" },
	  {BOLD "GeneralBernsteinSato", " -- use ", TO "globalB" }
	  }
     }
document {
     Key => GeneralBernsteinSato,
     Headline => "a strategy option for lct, globalBFunction",
     "see ", TO "lct", TO "globalBFunction", "."
     }
document {
     Key => ViaBFunction,
     Headline => "a strategy option for lct",
     "see ", TO "lct"
     }
document {
     Key => {(factorBFunction, RingElement),factorBFunction},
     Headline => "factorization of a b-function",
     Usage => "bFunction b",
     Inputs => {
	  "b" => {"a polynomial obtained via one of the b-function routines"}
	  },
     Outputs => {
	  Product => {"the factorization of ", TT "b"}
	  },
     BOLD "Fact. ", "The roots of any b-function are rational.",
     EXAMPLE {
	"R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
     	  "f = x^10",
     	  "b = globalBFunction f",
     	  "factorBFunction b"
     	  },
     Caveat => {
	  "f should be an output of one of the b-function routines"
     	  },
     SeeAlso => { 
	  "bFunction",
	  "globalBFunction"
	  }
     }  

document {
     Key => {(getIntRoots, RingElement), getIntRoots},
     Headline => "get integer roots of a b-function",
     Usage => "getIntRoots b",
     Inputs => {
     	  "b" => "the output of one of the b-function routines" 
	  },
     Outputs => {
	  List => {"the list of the integer roots of ", TT "b"}   
	  },
     SeeAlso => {"globalBFunction", "bFunction"}
     }

document {
     Key => {(bFunctionRoots, RingElement), bFunctionRoots},
     Headline => "get roots of a b-function",
     Usage => "bFunctionRoots b",
     Inputs => {
     	  "b" => "the output of one of the b-function routines" 
	  },
     Outputs => {
	  List => {"the list of the roots of ", TT "b"}   
	  },
     SeeAlso => {"globalBFunction", "bFunction", "generalB"}
     }

document {
     Key => {(globalB, Ideal, RingElement), globalB},
     Headline => "compute global b-function and b-operator 
          for a D-module and a polynomial",
     Usage => "H = globalB(I,f)", 
     Inputs => {
	  "I" => {"a holonomic ideal"},
	  "f" => {"a polynomial in a Weyl algebra 
	       (should not contain differential variables)"}
	       },
     Outputs => {
	  "H" => HashTable => {"containing the keys ",  
	       TT "Bpolynomial", " and ", TT "Boperator"}
	  },
     "The algorithm used here is a modification of the original
     algorithm of Oaku for computing Bernstein-Sato polynomials",
     EXAMPLE lines ///
	  R = QQ[x, dx, WeylAlgebra => {x=>dx}]
     	  f = x^7
     	  b = globalB(ideal dx, f)
     	  factorBFunction b.Bpolynomial 
     	  ///,
     SeeAlso => { "bFunction", "globalBFunction", "factorBFunction" }
     }  
document {
     Key => Boperator,
     Headline => "a key attached by globalB and Dlocalize",
     SeeAlso => { "globalB", "Dlocalize" }
     }
document {
     Key => Bpolynomial,
     Headline => "a key attached by globalB",
     "see ", TO "globalB"
     }
document {
     Key => {(globalBoperator, RingElement), globalBoperator},
     Headline => "compute a b-operator of a polynomial",
     SeeAlso => {"globalB"}
     } 

document {
     Key => AnnFs,
     Headline => "the annihilating ideal of f^s",
     "Either a single or a list of polynomials can be supplied for ",  EM "f", ".",
     SeeAlso => {"AnnIFs", "WeylAlgebra"}
     }

document {
     Key => {(AnnFs, RingElement)},
     Headline => "the annihilating ideal of f^s",
     Usage => "AnnFs f",
     Inputs => {
	  "f" => { 
	       "a polynomial in a Weyl algebra ", EM {"A", SUB "n"},  
	       " (should contain no differential variables)" 
	       }
	  },
     Outputs => {
	  Ideal => {"an ideal of ", EM {"A", SUB "n", "[s]"}}
	  },
     "The annihilator ideal is needed to compute a D-module 
     representation of the localization of ", 
     EM {"k[x", SUB "1", ",...,x", SUB "n", "]"}, " at ", EM "f", ".",
     EXAMPLE lines ///
     	  R = QQ[x_1..x_4, z, d_1..d_4, Dz, WeylAlgebra => toList(1..4)/(i -> x_i => d_i) | {z=>Dz}]
     	  f = x_1 + x_2 * z + x_3 * z^2 + x_4 * z^3
     	  AnnFs f
     	  ///,
     Caveat => {"The ring of ", TT "f", " should not have any parameters, 
     	  i.e., it should be a pure Weyl algebra. 
	  Also this ring should not be a homogeneous Weyl algebra."},
     SeeAlso => {"AnnIFs", "WeylAlgebra"}
     }  

document {
     Key => {(AnnIFs, Ideal,RingElement), AnnIFs}, 
     Headline => "the annihilating ideal of f^s for an arbitrary D-module", 
     Usage => "AnnIFs(I,f)",
     Inputs => {
	  "I" => {
	       "that represents a holonomic D-module", 
	       EM {"A", SUB "n", "/I"}
	       },
	  "f" => {"a polynomial in a Weyl algebra ", EM {"A", SUB "n"},  
	       " (should contain no differential variables)"}
	  },
     Outputs => {
	  Ideal => {"the annihilating ideal of ", TEX "A_n[f^{-1},s] f^s", " tensored with ",
	       TEX "A_n/I", " over the ring of polynomials" }
	  },
     EXAMPLE lines ///
	  W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
	  AnnIFs (ideal dx, x^2)
	  ///, 
     Caveat => {"Caveats and known problems: The ring of f should not have any 
	  parameters: it should be a pure Weyl algebra. Similarly, 
	  this ring should not be a homogeneous Weyl algebra."
     	  },
     SeeAlso => {"AnnFs", "WeylAlgebra"}
     }  

document {
     Key => {(AnnFs, List)},
     Headline => "the annihilating ideal of f_1^{s_1}...f_r^{s_r}",
     Usage => "AnnFs F",
     Inputs => {
	  "F" => { 
	       "{f_1,...,f_r}, a list of polynomials in n variables                           
	       (f_i has to be an element of A_n, the Weyl algebra)"
	       }
	  },
     Outputs => {
	  Ideal => {"an ideal in A_n<t_1,..., t_r,dt_1,...,dt_r>"}
	  },
     EXAMPLE lines ///
     W = makeWA ( QQ[x_1..x_3] ) 
     AnnFs {x_2^2-x_1*x_3, x_1^3-x_3^2}
     ///,
     SeeAlso => {"AnnIFs", "WeylAlgebra"}
     }  

document {
     Key => {(Dtrace, ZZ), Dtrace},
     Headline => "set the depth of comments made by D-module routines",
     Usage => "Dtrace n",
     Inputs => {
	  "n" => { "new level" }
	  },
     Outputs => {
	  ZZ => { "old level" }
	  },
     SeeAlso => {"getDtrace"}
     }  
document {
     Key => getDtrace,
     Headline => "(internal) -- get the INFOLEVEL switch",
     SeeAlso => {"Dtrace"}
     }  

document {
     Key => {(setHomSwitch, Boolean), setHomSwitch},
     Headline => "toggles the use of homogeneous Weyl algebra",
     Usage => "setHomSwitch n",
     Inputs => {
	  "n" => Boolean => { "new value" }
	  },
     Outputs => {
	  Boolean => "old value"
	  },
     SeeAlso => {"getHomSwitch"}
     }  

document {
     Key => getHomSwitch,
     Headline => "(internal) -- get the HOMOGENIZATION switch",
     SeeAlso => {"setHomSwitch"}
     }  

document {
     Key => [localCohom,Strategy],
     Headline => "specify strategy for local cohomology",
     "This option together with ", TO "LocStrategy", " determines a strategy for ", 
     TT "localCohom(...Ideal...)", " and ", TT "localCohom(...Ideal, Module...)", ".",
     UL { 
	  {BOLD "Walther", " -- the algorithm of U. Walther that uses Cech complex."},
	  {BOLD "LocStrategy => null", 
	       " -- used only for ", TT "localCohom(...Ideal...)", 
	       ", localizations are done by straitforward computation of 
	       annihilators and b-polynomials as described in [1]."},
	  {BOLD "LocStrategy => OaTaWa", 
	       " -- localizations are done following Oaku-Takayama-Walther method."},
	  {BOLD "LocStrategy => Oaku", 
	       " -- localizations are done following Oaku's algorithm."},
	  {BOLD "OaTa", " -- restriction algorithm is used, 
	       which is due to T. Oaku and N. Takayama [2]"}   
	  },
          --Caveat => {"When WaltherOTW strategy is used the error 'Bad luck!' 
          --may appear. This means your are not a lucky individual...
	  --The glitch is due to the fact that the localizations are iterated 
	  --for this particular strategy; it was resolved for WaltherOaku, 
	  --a strategy that considers everyone lucky."
	  --},
     "For detailed description of the algorithms see",
     UL {
	  {BOLD "[1]", "U. Walther, ", 
	       EM "Algorithmic computation of local cohomology 
	       modules and the local cohomological dimension of algebraic 
	       varieties (JPAA (139), 1999.)"
	       },
	  {BOLD "[2]", "Oaku, Takayama", 
	       EM "Algorithms for D-modules..."
	       }
	  }    	      
     } 
document {
     Key => Walther,
     Headline => "an option for localCohom=>Strategy",
     "see ", TO "localCohom"
     } 
document {
     Key => OaTa,
     Headline => "an option for localCohom=>Strategy",
     "see ", TO "localCohom"
     } 
document {
     Key => LocStrategy }
document {
     Key => [localCohom,LocStrategy],
     Headline => "specify localization strategy for local cohomology",
     "See ", TO [localCohom,Strategy]
     }
document {
     Key => OaTaWa,
     Headline => "an option for localCohom => LocStrategy",
     SeeAlso => "localCohom"
     }
document {
     Key => localCohom,
     Headline => "local cohomology",
     "Local cohomology of a polynomial ring:",
     UL {
	  {TO (localCohom, Ideal)},
	  {TO (localCohom, List, Ideal)},
	  {TO (localCohom, ZZ, Ideal)}
	  },
     "Local cohomology of a holonomic module:",
     UL {
	  {TO (localCohom, Ideal, Module)},
	  {TO (localCohom, ZZ, Ideal, Module)},
	  {TO (localCohom, List, Ideal, Module)}
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }
document {
     Key => (localCohom, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "H = localCohom I", 
     Inputs => {
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       }
	  },
     Outputs => {
	  "H" => {
	       "each entry of ", TT "H", " has an integer key and 
	       contains the cohomology module in the corresponding degree."
	       }
	  },
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom I",
     	  "pruneLocalCohom h"
	  },
     Caveat => {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."},
     SeeAlso => {"pruneLocalCohom"}
     }  

document {
     Key => (localCohom, List, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "localCohom(l,I)",
     Inputs => { "l", "I" },
     Outputs => { { "the local cohomology of ", TT "I", " in the degrees specified by ", EM "l" } },
     "See ", TO (localCohom, Ideal), " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom({1,2}, I)",
     	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, ZZ, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "localCohom(d,I)",
     Inputs => { "d", "I" },
     Outputs => {{ "the local cohomology of ", TT "I", " in degree ", EM "d" }},
     "See ", TO (localCohom, Ideal), " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom (2,I)",
     	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "H = localCohom(I,M)", 
     Inputs => {
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       },
	  "M" => {
	       "a holonomic module over Weyl algebra ", 
	       EM{"A", SUB "n", "(k)"}
	       }
	  },
     Outputs => {
	  "H" => {
	       "each entry of ", TT "H", " has an integer key and 
	       contains the cohomology module in the corresponding degree."
	       }
	  },
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom(I, W^1 / ideal{dX,dY,dZ})",
     	  "pruneLocalCohom h"
	  },
     Caveat => {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."},
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, ZZ, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "localCohom(d,I,M)",
     Inputs => { "d", "I", "M" },
     Outputs => {{
	  "the local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degree ", EM "d", ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  }},
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom(2, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, List, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "localCohom(l,I,M)",
     Inputs => { "l", "I", "M" },
     Outputs => {{
	  "the local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degrees listed in ", EM "l", 
	  ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  }},
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom({1,2}, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => {(pruneLocalCohom, HashTable), pruneLocalCohom},
     Headline => "prunes local cohomology modules",
     Usage => "pruneLocalCohom H",
     Inputs => {{"the output of ", TO "localCohom"}},
     Outputs => {HashTable},
     "This function applies ", TO "Dprune", " to all the keys of ", TT "H", ".", 
     SeeAlso => {"localCohom", "Dprune"} 
     }
document {
     Key => [paramBpoly,GroundField],
     Headline => "characteristic for modular computation"
     }
document {
     Key => GroundField
     }
document {
     Key => {(paramBpoly, RingElement, String), paramBpoly},
     Headline => "compute the list of all possible Bernstein-Sato polynomials 
     for a polynomial with parametric coefficients",
     Usage => "paramBpoly(f,filename)", 	  
     Inputs => {
     	  "f" => RingElement => {
	       "a polynomial in Weyl algebra ", EM "A_n(Q)"
	       },
	  "filename" => String => {"the base name for the output files"}
	  },
     Outputs => {
	  List => {"all possible Bernstein-Sato polynomials"}
	  },
--     Consequences => { "Creates  the file <filename.tex> that contains the list of 
--     the Bernstein-Sato polynomials and the corresponding constructible sets."
--	  },     
     EXAMPLE lines ///
	  A =  (QQ [a,b,c]) [x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	  paramBpoly(a*x^2 + b*x*y + c*y^2, "quadratic")
	  ///,
     Caveat => {
	  "A finite field should be used in place of ", EM "Q", 
	  " in order to speed up computations. This routine works only
	  on relatively small examples."
	  },
     SeeAlso => {"globalBFunction"}
     }  
document {
     Key => {(makeCyclic, Matrix), makeCyclic},
     Headline => "finds a cyclic generator of a D-module",
     Usage => "H = makeCyclic M", 
     Inputs => {
	  "M" => Matrix => {
	       "that specifies a map such that ", TT "coker M", " is a 
	       holonomic D-module"
	       }
	  },
     Outputs => {
	  "H" => HashTable => {"where ", TT "H.Generator", " is a cyclic generator
	       and ", TT "H.AnnG", " is the annihilator ideal 
	       of this generator"} 
	  },
     "It is proven that every holonomic module is cyclic and 
     there is an algorithm for computing a cyclic generator.",
     EXAMPLE lines ///
	  W = QQ[x, dx, WeylAlgebra => {x=>dx}]
	  M = matrix {{dx,0,0},{0,dx,0},{0,0,dx}} -- coker M = QQ[x]^3 
	  h = makeCyclic M
	  ///,
     Caveat => {"The module ", EM "M", " must be holonomic."},
     SeeAlso => {"isHolonomic"}
     }  

document {
     Key => Generator,
     Headline => "a key created by makeCyclic",
     "See ", TO "makeCyclic", "."
     }
document {
     Key => AnnG,
     Headline => "a key created by makeCyclic",
     "See ", TO "makeCyclic", "."
     }

document {
     Key => {isHolonomic, (isHolonomic, Module), (isHolonomic, Ideal)},
     Headline => "determines whether a D-module (or ideal in Weyl algebra) is holonomic",
     Usage => "isHolonomic M, isHolonomic I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
     	  Boolean
	  },
     "A module is holonomic if it has dimension ", EM "n",
     ", the number of variables in the Weyl algebra ", EM "D = ", 
     BOLD "C", "<", EM {"x", SUB "1",",...,x", SUB "n",
	  ",d", SUB "1", ",...,d", SUB "n"}, ">",
     EXAMPLE lines ///
          A = matrix{{1,1,1},{0,1,2}}
	  b = {3,4}
          I = gkz(A,b)
     	  isHolonomic I     	  	  
     	  ///,
     SeeAlso => {"Ddim", "holonomicRank"}
     } 

document {
     Key => [DHom,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => {DHom, (DHom,Module,Module), (DHom,Module,Module,List), (DHom,Ideal,Ideal)},
     Headline=>"D-homomorphisms between holonomic D-modules",
     Usage => "DHom(M,N), DHom(M,N,w), DHom(I,J)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "N" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "J" => Ideal => {"which represents the module ", EM "N = D/J"},
	  "w" => List => "a positive weight vector"  
	  },
     Outputs => { 
	  HashTable => {" a basis of D-homomorphisms between holonomic D-modules ", 
	       EM "M", " and ", EM "N" }
     	  },
     "The set of D-homomorphisms between two holonomic modules ", EM "M",
     " and ", EM "N",
     " is a finite-dimensional vector space over the ground field.
     Since a homomorphism is defined by where it sends a set of generators,
     the output of this command is a list of matrices whose columns
     correspond to the images of the generators of ", EM "M", ".  Here
     the generators of ", EM "M", " are determined from its presentation
     by generators and relations.",
     PARA {
	  "The procedure calls ", TO "Drestriction", ", which uses ", 
	  EM "w", " if specified."
	  },
     PARA {
	  "The algorithm used appears in the paper 'Computing homomorphisms
	  between holonomic D-modules' by Tsai-Walther(2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(D-1)
	     N = W^1/ideal((D-1)^2)
	     DHom(M,N)
	     ///,
     Caveat => {"Input modules ", EM "M", ", ", EM "N", ", ", 
	  EM "D/I", " and ", EM "D/J", " should be holonomic."},
     SeeAlso => {"DExt", "Drestriction"}
     }

document {
     Key => [DExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => [DExt,Special]
     }
document {
     Key => Special,
     SeeAlso => "DExt"
     }
document {
     Key => None,
     Headline => "an option for DExt=>Special",
     SeeAlso => "DExt"
     }
document {
     Key => [DExt,Output]}
document {
     Key => Output
     }
document {
     Key => [DExt,Info]
     }
document {
     Key => Info
     }
document {
     Key => {DExt, (DExt, Module, Module), (DExt, Module, Module, List)},
     Headline => "Ext groups between holonomic modules",
     Usage => "DExt(M,N), DExt(M,N,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "N" => Module => {"over the Weyl algebra ", EM "D"},
	  "w" => List => "a positive weight vector"  
	  },
     Outputs => { 
	  HashTable => {" the ", 
	       TT "Ext"," groups between holonomic D-modules", EM "M", 
	       " and ", EM "N" }
     	  },
     "The ", TEX "Ext", " groups between D-modules ", EM "M"," and ", EM "N",
     " are the derived functors of ", TEX "Hom", ", and are finite-dimensional
     vector spaces over the ground field when ", EM "M", " and ", EM "N", " are holonomic.",
     PARA {
	  "The procedure calls ", TO "Drestriction", ", which uses ", 
	  EM "w", " if specified."
	  },
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x*(D-1))
	     N = W^1/ideal((D-1)^2)
	     DExt(M,N)
	     ///,
     Caveat =>{
	  "Input modules M, N should be holonomic.",
	  "Does not yet compute explicit reprentations of Ext groups
	  such as Yoneda representation."
	  },
     SeeAlso => {"DHom", "Drestriction"}
     }

document {
     Key => [PolySols,Alg],
     Headline => "algorithm for finding polynomial solutions",
     UL {
	  {BOLD "GD", " -- uses Groebner deformations"},
	  {BOLD "Duality", " -- uses homological duality"}
	  }     
     }
document {
     Key => Alg }
document {
     Key => GD,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols" 
     }
document {
     Key => Duality,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols" 
     }
document {
     Key => {PolySols, (PolySols,Module), (PolySols,Ideal,List), (PolySols,Module,List), (PolySols,Ideal)},
     Headline => "polynomial solutions of a holonomic system",
     Usage => "PolySols I, PolySols M, PolySols(I,w), PolySols(M,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"holonomic ideal in the Weyl algebra ", EM "D"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  List => {"a basis of the polynomial solutions of ", EM "I", 
	       "(or of D-homomorhpisms between ", EM "M", " and the polynomial ring)",
	       " using ", EM "w", 
	       " for Groebner deformations"}
     	  },
     "The polynomial solutions of a holonomic system form a
     finite-dimensional vector space.
     There are two algorithms implemented to get these solutions.
     The first algorithm is based on Groebner deformations and
     works for ideals ", EM "I", " of PDE's -- see the paper 'Polynomial
     and rational solutions of a holonomic system' by 
     Oaku-Takayama-Tsai (2000).  The second algorithm is based
     on homological algebra -- see the paper 'Computing
     homomorphims between holonomic D-modules' by Tsai-Walther (2000).",
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     I = ideal(D^2, (x-1)*D-1)
	     PolySols I
	     ///,
     SeeAlso => {"RatSols", "Dintegration"}
     },


document {
     Key => [PolyExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => {PolyExt, (PolyExt,Module), (PolyExt,ZZ,Ideal), (PolyExt,ZZ,Module), (PolyExt,Ideal)},
     Headline => "Ext groups between a holonomic module and a polynomial ring",
     Usage => "PolyExt M, PolyExt I; RatExt(i,M), RatExt(i,I)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
     	  { ofClass HashTable, " or ", ofClass Module, ", the ", 
	       TEX "Ext^i"," group(s) between holonomic ", EM "M", 
	       " and the polynomial ring" }
     	  },
     "The ", TT "Ext", " groups between a D-module ", EM "M", 
     " and the polynomial ring are the derived functors of ", TT "Hom", 
     ", and are finite-dimensional vector spaces over the ground field when ",
     EM "M", " is holonomic.",
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x^2*D^2)
	     PolyExt(M)
	     ///,
     Caveat =>{"Does not yet compute explicit representations of
	  Ext groups such as Yoneda representation."},
     SeeAlso => {"PolySols", "RatExt", "DExt", "Dintegration"}
     }

document {
     Key => {RatSols, (RatSols,Ideal,List,List), (RatSols,Ideal,RingElement,List), 
	  (RatSols,Ideal,List), (RatSols,Ideal,RingElement), (RatSols,Ideal)},
     Headline => "rational solutions of a holonomic system",
     Usage => "RatSols I, RatSols(I,f), RatSols(I,f,w), RatSols(I,ff), RatSols(I,ff,w)",
     Inputs => {
	  "I" => Ideal => {"holonomic ideal in the Weyl algebra ", EM "D"},
	  "f" => RingElement => "a polynomial",
	  "ff" => List => "a list of polynomials",
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  List => {"a basis of the rational solutions of ", EM "I", " with poles along ", EM "f", 
	       " or along the polynomials in ", TT "ff", " using ", EM "w", 
	       " for Groebner deformations"}
     	  },
     "The rational solutions of a holonomic system form a
     finite-dimensional vector space.
     The only possibilities for the poles of a rational solution
     are the codimension one components of the singular locus.
     An algorithm to compute rational solutions 
     is based on Groebner deformations and
     works for ideals ", EM "I", " of PDE's -- see the paper 'Polynomial
     and rational solutions of a holonomic system' by 
     Oaku-Takayama-Tsai (2000).",
     
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     I = ideal((x+1)*D+5)
	     RatSols I
	     ///,
     Caveat =>{"The most efficient method to find rational solutions is
	  to find the singular locus, then try to find its irreducible
	  factors.  With these, call RatSols(I, ff, w), where w
	  should be generic enough so that the PolySols routine
	  will not complain of a non-generic weight vector."},     
     SeeAlso => {"PolySols", "RatExt", "DHom"} 
     },

document {
     Key => [RatExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => {RatExt, (RatExt,Module), (RatExt,ZZ,Ideal,RingElement), (RatExt,ZZ,Ideal), 
	  (RatExt,Ideal,RingElement), (RatExt,Ideal),(RatExt,ZZ,Module,RingElement), 
	  (RatExt,ZZ,Module), (RatExt,Module,RingElement)},
     Headline => "Ext(holonomic D-module, polynomial ring localized at the sigular locus)",
     Usage => "RatExt M, RatExt I; RatExt(M,f), RatExt(I,f);
               RatExt(i,M), RatExt(i,I); RatExt(i,M,f), RatExt(i,I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
     	  { ofClass HashTable, " or ", ofClass Module, ", the ", 
	       TEX "Ext^i"," group(s) between holonomic ", EM "M", 
	       " and the polynomial ring localized at the singular locus of ", EM "M", 
	       " (or at ", EM "f", " if specified)" } 
     	  },
     "The Ext groups between M and N
     are the derived functors of Hom, and are finite-dimensional
     vector spaces over the ground field when M and N are holonomic.",
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x*D+5)
	     RatExt M
	     ///,
     Caveat =>{"Input modules M or D/I should be holonomic."},
     SeeAlso => {"Dresolution", "Dintegration"}
     }

--document {
--     Key => WeylAlgebra,
--     TT "WeylAlgebra", " --
--     name for an optional argument for a monoid that
--     specifies that a PolynomialRing created from it will
--     be a Weyl Algebra.",
--
--     PARA{},
--     "The n-th Weyl algebra is the associative ring on 2n variables,
--     e.g., K<x_1..x_n, D_1..D_n>, where all the variables commute except
--     for (D_i x_i = x_i D_i + 1).  It can be viewed as the ring
--     of algebraic differential operators on affine space K^n.",
--
--     PARA{},
--     "A simple example:",
--     EXAMPLE {
--	"W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
--     	"x*Dx", 
--     	"Dx*x"},     
--     PARA{},
--     "Caveats and known problems:",
--     UL{"The variables can be called by any name, but for each
--	  pair such as x => Dx, the commutative variable (in this case x)
--	  must be listed before the derivation variable (in this case Dx)"}
--     }

document {
     Key => {(createDpairs,PolynomialRing), createDpairs},
     Headline => "pairs up the variables in Weyl algebra ",
     Usage => "createDpairs A",
     Inputs => {
	  "A" => "the Weyl algebra"
	  },
     Consequences => {
      	  {"attaches to ", TT "A", " a pair of keys to help distinguish the
     	  coordinate variables from the derivation variables."}    	  
	  },
     "Since the Weyl algebra has commutation rules, this routine
     attaches to the Weyl algebra two keys to organize the
     variables.  The first key 'dpairVars' contains 3 lists: a list of the coordinate
     variables, a list of the derivative variables, and a list
     of the central variables.  The second key 'dpairInds' also contains 3 lists
     of the corresponding indices to 'dpairVars'.",
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     createDpairs W 
	     W.dpairVars
	     W.dpairInds
	     ///,
     SeeAlso => {"WeylAlgebra"}
     },

document {
     Key => dpairInds,
     Headline => "a key attached by createDpairs",
     "see ", TO "createDpairs"
     }
document {
     Key => dpairVars,
     Headline => "a key attached by createDpairs",
     "see ", TO "createDpairs"
     }

document {
     Key => {Fourier, (Fourier,Matrix), (Fourier,RingElement), (Fourier,Ideal)},
     Headline => "Fourier transform for Weyl algebra",
     Usage => "Fourier A",
     Inputs => {
	  "A" => {ofClass RingElement, ", ", ofClass Ideal, ", or ", 
	       ofClass Matrix} 
	  },
     Outputs => {
	  {ofClass RingElement, ", ", ofClass Ideal, ", or", 
	       ofClass Matrix, 
	       " of the same type as ", TT "A", " -- the Fourier transform of ", TT "A"}
	  },
     "The Fourier transform is the automorphism of the Weyl algebra
     which sends ", EM {"x",SUB "i"}, " to ", EM {"D", SUB "i"}, " 
     and ", EM  {"D", SUB "i"}, " to ", EM {"-x",SUB "i"}, ".",
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     L = x^2*Dy + y*Dy^2 + 3*Dx^5*Dy       
	     Fourier L
	     ///,
     SeeAlso => {"WeylAlgebra"}
     },

document {
     Key => {Dtransposition, (Dtransposition,Matrix), (Dtransposition,Ideal), 
	  (Dtransposition,ChainComplex), (Dtransposition,RingElement)},
     Headline => "standard transposition for Weyl algebra",
     Usage => "Dtransposition A",
     Inputs => {
	  "A" => {ofClass RingElement, ", ", ofClass Ideal, ", ", 
	       ofClass Matrix, ", or ", ofClass ChainComplex} 
	  },
     Outputs => {
	  {ofClass RingElement, ", ", ofClass Ideal, ", ", 
	       ofClass Matrix, ", or ", ofClass ChainComplex,
	       " of the same type as ", TT "A", " -- the standard transpose of ", TT "A"}
	  },
     "The standard transposition is the involution of the Weyl algebra
     which sends ", EM {"x", SUP "a","d", SUP "b"}, " to ", 
     EM {"(-d)", SUP "b", "x", SUP "a"}, ".
     It provides the equivalence in the Weyl algebra between left
     and right D-modules.",
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     L = x^2*Dy + y*Dy^2 + 3*Dx^5*Dy       
	     Dtransposition L
	     ///,
     Caveat =>{"The standard transposition of a left ideal should be a right
	  ideal, however M2 currently doesn't support right modules.
	  Thus the output is left ideal generated by the transposition
	  of the previous generators."},
     SeeAlso => {"WeylAlgebra"}
     },

document {
     Key => {singLocus, (singLocus,Module), (singLocus,Ideal)},
     Headline => "singular locus of a D-module",
     Usage => "singLocus M, singLocus I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  Ideal => {"the singular locus of ", EM "M"}
	  },
     "The singular locus of the system of PDE's given by ", EM "I",
     " generalizes the notion of singular point of an ODE.
     Geometrically, the singular locus of a D-module ", EM "M", 
     " equals the projection
     of the characteristic variety of ", EM "M", " minus the zero section
     of the cotangent bundle to the base affine space ", BOLD "C", SUP EM "n", ".",
     PARA {
	  "For details of the algorithm for computing singular locus 
	  see the book 'Groebner deformations
	  of hypergeometric differential equations' by 
	  Saito-Sturmfels-Takayama (1999)."},
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy)
	     singLocus I
	     ///,
     SeeAlso => {"charIdeal", "holonomicRank", "Ddim"}
     },

document {
     Key => {charIdeal, (charIdeal,Ideal), (charIdeal,Module)},
     Headline => "characteristic ideal of a D-module",
     Usage => "charIdeal M, charIdeal I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  Ideal => {"the characteristic ideal of ", EM "M"}
	  },
     "The characteristic ideal of ", EM "M", " is the annihilator of ",
     EM "gr(M)", " under a good filtration with respect to the order
     filtration. If ", EM "D", " is the Weyl
     algebra ", BOLD "C", TT "<", EM "x_1,....,x_n,d_1,...,d_n", TT ">", 
     ", then the order filtration
     corresponds to the weight vector (0,...,0,1...,1).
     The characteristic ideal
     lives in the associated graded ring of ", EM "D", " with respect to
     the order filtration, and this is a commutative polynomial ring ",
     BOLD "C", TT "[", EM "x_1,....,x_n,xi_1,...,xi_n", TT "]", 
     " -- here the ", EM "xi", "'s are the symbols of the ", EM "d", "'s. 
     The zero locus of the characteristic
     ideal is equal to the characteristic variety of ", EM "D/I", ", which
     is an invariant of a D-module.",
     PARA {
	  "The algorithm to compute the characteristic ideal consists of computing
	  the initial ideal of I with respect to the weight vector
	  (0,...,0,1...,1).  See the book 'Groebner deformations
	  of hypergeometric differential equations' by 
	  Saito-Sturmfels-Takayama (1999) for more details."},
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy)
	     charIdeal I
	     ///,
     SeeAlso => {"Ddim", "singLocus", "holonomicRank"}
     },

document {
     Key => {Drank, (Drank,Module), (Drank,Ideal) },
     Headline => "an old name of holonomicRank",
     SeeAlso => {"holonomicRank"}
}

document {
     Key => {holonomicRank, (holonomicRank,Module), (holonomicRank,Ideal) },
     Headline => "rank of a D-module",
     Usage => "holonomicRank M, holonomicRank I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  ZZ => {"the rank of ", EM "M"}
	  },
     "The rank of a D-module ", EM "M = D^r/N", " provides analytic information
     about the system of PDE's given by ", EM "N", ". In particular, a theorem of 
     Cauchy states that the dimension of holomorphic solutions to ", EM "N", " in a
     neighborhood of a nonsinugular point is equal to the rank.",
     PARA {
	  "The rank of a D-module is defined algebraically as follows. 
	  Let ", EM "D", " denote the Weyl algebra ", 
	  BOLD "C", TT "<", EM "x_1,....,x_n,d_1,...,d_n", TT ">",
	  " and let ", EM "R", " denote the ring of differential operators ",
	  BOLD "C", TT "(", EM "x_1,...,x_n", TT ")",TT "<", EM "d_1,...,d_n", TT ">",
	  " with rational function coefficients.
	  Then the rank of ", EM "M = D^r/N", " is equal to the dimension of ", 
	  EM "R^r/RN"," as a vector space over ", BOLD "C", "(", EM "x_1,...,x_n", ")."},
     PARA {
	  "See the book 'Groebner deformations of hypergeometric differential equations' by 
	  Saito-Sturmfels-Takayama (1999) for more details of the algorithm."},
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy) 
	     holonomicRank I
	     ///,
     SeeAlso => {"charIdeal", "singLocus", "Ddim"}
     },

document {
     Key => {Ddim, (Ddim,Ideal), (Ddim,Module)},
     Headline => "dimension of a D-module", 
     Usage => "Ddim M, Ddim I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  ZZ => {"the dimension of ", EM "M"}
	  },
     PARA {
	  "The dimension of ", EM "M", " is equal to the dimension of
	  the associated graded module with respect to the Bernstein 
	  filtration." },
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy) 
	     Ddim I
	     ///,
     SeeAlso => {"charIdeal", "holonomicRank", "singLocus"}
     },

document {
     Key => {(makeWeylAlgebra,PolynomialRing), makeWeylAlgebra, 
	  [makeWeylAlgebra,SetVariables]},
     Headline => "Weyl algebra corresponding to a polynomial ring",
     Usage => "makeWeylAlgebra R",
     Inputs => {
	  "R"=>"a (commutative) ring of polynomials",
	  SetVariables=>Boolean=>"whether to set variables of the created algebra to be global"
	  },
     Outputs => {
	  PolynomialRing => "the (non-commutative) Weyl algebra" 
	  },
     "Given a polynomial ring ", EM "R", " with variables ", EM "x_1,..,x_n", 
     ", this routine returns a Weyl algebra with variables ", EM "x_1,..,x_n",
     " and ", EM "dx_1,..,dx_n", ".", 
     EXAMPLE lines ///
	     R = QQ[x,y,z]
	     W = makeWeylAlgebra R
	     ///,
     "Abbreviations :",
     UL{"makeWA"},
     Caveat =>{"The polynomial ring R must be commutative."},
     SeeAlso => {"WeylAlgebra"}
     }

document {
     Key => {Ddual, (Ddual,Module), (Ddual,Ideal)},
     Headline => "holonomic dual of a D-module",
     Usage => "Ddual M, Ddual I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  Module => {"the holonomic dual of ", EM "M"}
	  },
     "If M is a holonomic left D-module, then ", 
     BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")", 
     " is a holonomic right D-module.  The holonomic dual is defined to be the left
     module associated to ",
     BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")", 
     ".  The dual is obtained by
     computing a free resolution of ", EM "M", ", dualizing, and applying
     the standard transposition to the ", EM "n", "-th homology.",
     EXAMPLE lines ///
             I = AppellF1({1,0,-3,2})
	     Ddual I
	     ///,
     Caveat =>{"The input module ", EM "M", " should be holonomic.  The user should
	  check this manually with the script ", TT "Ddim", "."},
     SeeAlso => {"Ddim", "Dtransposition"}
     }

document {
     Key => {[Dlocalize,Strategy],[DlocalizeAll,Strategy],[Dlocalization,Strategy],[DlocalizeMap,Strategy]},
     Headline=>"strategy for computing a localization of a D-module",
     UL{
	  {BOLD "Oaku", " -- use the Oaku algorithm"},
	  {BOLD "OTW", " -- use the Oaku-Takayama-Walther algorithm"}
	  }
     }
document {
     Key => Oaku,
     Headline => "an option for Dlocalize=>Strategy",
     "see ", TO "Dlocalize"
     } 
document {
     Key => OTW,
     Headline => "an option for Dlocalize=>Strategy",
     "see ", TO "Dlocalize"
     } 

document {
     Key => {Dlocalize, (Dlocalize,Ideal,RingElement), (Dlocalize,Module,RingElement)},
     Headline => "localization of a D-module",

     Usage => "Dlocalize(M,f), Dlocalize(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  Module => {"the localized module ", TEX "M_f = M[f^{-1}]", " as a D-module"}
	  },
     "One of the nice things about D-modules is that if a finitely
     generated D-module is specializable along ", EM "f", ", then it's localization 
     with respect to ", EM "f", " is also finitely generated.  For instance,
     this is true for all holonomic D-modules.",
     
     PARA{},
     "There are two different algorithms for localization implemented.  
     The first appears in the
     paper 'A localization algorithm for D-modules' by Oaku-Takayama-Walther
     (1999).  The second is due to Oaku and appears in the paper
     'Algorithmic computation of local cohomology modules and the
     cohomological dimension of algebraic varieties' by Walther(1999)",

     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	Mf = Dlocalize(M, f)
	///,
     SeeAlso => {"DlocalizeAll", "DlocalizeMap", "AnnFs", "Dintegration"}
     }

document {
     Key => {DlocalizeMap, (DlocalizeMap,Ideal,RingElement), (DlocalizeMap,Module,RingElement)},
     Headline => "localization map from a D-module to its localization",
     Usage => "DlocalizeMap(M,f), DlocalizeMap(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  Matrix => {"which represents the natural map from ", TEX "M", " to ", TEX "M_f = M[f^{-1}]"}
	  },
     "A supplementary function for ", TO "Dlocalize", 
     " that computes the localization map.",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	DlocalizeMap(M, f)
	///,
     SeeAlso => {"Dlocalize", "AnnFs", "Dintegration"}
     }


document {
     Key => {DlocalizeAll, (DlocalizeAll,Ideal,RingElement), (DlocalizeAll,Module,RingElement)},
     Headline => "localization of a D-module (extended version)",
     Usage => "DlocalizeAll(M,f), DlocalizeAll(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  HashTable => {"which contains the localized module", TEX "M_f = M[f^{-1}]", 
	       " and some additional information"}
	  },
     "An extension of ", TO "Dlocalize", 
     " that in addition computes the localization map, 
     the b-function, and the power ", TEX "s", " of the generator ", TEX "f^s", ".",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	DlocalizeAll(M, f)
	///,
     SeeAlso => {"Dlocalize", "AnnFs", "Dintegration"}
     }

     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of M with respect to f and
     some auxilary information",
     BR{},
     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of D/I with respect to f and some
     auxilary information",
     PARA{},


document {
     Key => {Dlocalization, (Dlocalization,Ideal,RingElement), (Dlocalization,Module,RingElement),
     	  DlocalizationAll, (DlocalizationAll,Ideal,RingElement), (DlocalizationAll,Module,RingElement),
	  DlocalizationMap, (DlocalizationMap,Ideal,RingElement), (DlocalizationMap,Module,RingElement)},
     Headline => "Dlocalization* is an OBSOLETE name for Dlocalize*", 
     "See ", TO "Dlocalize", "."
     }
document {
     Key => LocModule,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => GeneratorPower,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => LocMap,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => annFS,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => IntegrateBfunction,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => Bfunction,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }

document {
     Key => {[Dres,Strategy],[Dresolution,Strategy]},
     Headline => "strategy for computing a resolution of a D-module",
     UL { 
	  {BOLD "Schreyer", 
	       " -- uses Schreyer method in homogeneous Weyl algebra"},
	  {BOLD "Vhomogenize", " -- uses V-homogenization method of Oaku"}
     	  }
     }
document {
     Key => {[Dres,LengthLimit],[Dresolution,LengthLimit]},
     Headline => "the limit for the length of a resolution of a D-module",
     "In case the actual length of the resolution exceeds the limit, it is truncated.
     The default value is infinity." 
     }
document {
     Key => Schreyer,
     Headline => "strategy for computing a resolution of a D-module"
     }
document {
     Key => Vhomogenize,
     Headline => "strategy for computing a resolution of a D-module"
     }

document {
     Key => {Dresolution, (Dresolution,Module), (Dresolution,Ideal,List), 
	  (Dresolution,Module,List), (Dresolution,Ideal)},
     Headline => "resolution of a D-module",
     Usage => "Dresolution M, Dresolution I, Dresolution(M,w), Dresolution(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  ChainComplex => {"a Schreyer resolution of the D-module ", EM "M", 
	       " or a resolution adapted to a weight vector ", EM "w", 
	       " of the form ", EM "(-u,u)"}
	  },
     "This routine computes various resolutions of a D-module.
     If no weight vector is specified, then the command
     produces a resolution by using the Schreyer order implemented
     in the engine.  If a weight vector ", EM "w", " of the form ", EM "(-u,u)", 
     " is specified, then the command produces a resolution with shifts
     which is adapted to the weight vector ", EM "w", ".  
     These ", EM "w", "-adapted resolutions are compatible
     with b-functions and used in the restriction algorithm.
     For ordinary resolutions, the user may use the command ", TT "resolution", ".
     Note that the notion of a minimal resolution is well-defined only in case 
     of homogenized Weyl algebra.",
     PARA{},
     "There are two strategies for constructing
     w-adapted resolutions.   The first strategy is to construct
     a Schreyer resolution in the homogenized Weyl algebra
     and then dehomogenize.  The second strategy is to homogenize
     with respect to the weight vector.
     These strategies are described in the paper
     'Algorithims for D-modules'
     by Oaku-Takayama(1999).",
     EXAMPLE lines ///
	     R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
	     I = ideal(x_1*D_1+3*x_2*D_2-1, D_1^3-D_2)
	     Dresolution(I,{-1,-1,1,1})
	     ///,
     "Abbreviations :",
     UL{"Dres"},

     SeeAlso => {"gbw", "Drestriction"}
     }

document {
     Key => {Dres, (Dres,Module), (Dres,Ideal,List), (Dres,Ideal), (Dres,Module,List)},
     Headline => "abbreviation for Dresolution",
     SeeAlso =>{"Dresolution"}
     }	

document {
     Key => {[DintegrationIdeal,Strategy],
	  [DintegrationComplex,Strategy],[Dintegrate,Strategy],
	  [DintegrationAll,Strategy],[DintegrationClasses,Strategy],
	  [Drestriction,Strategy],[DrestrictionIdeal,Strategy],
	  [DrestrictionComplex,Strategy],[Drestrict,Strategy],
	  [DrestrictionAll,Strategy],[DrestrictionClasses,Strategy]},
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
	  }

document {
     Key => {Drestriction, (Drestriction,ZZ,Module,List), (Drestriction,Ideal,List), 
	  (Drestriction,Module,List), (Drestriction,ZZ,Ideal,List)},
     Headline => "restriction modules of a D-module",
     Usage => "N = Drestriction(M,w), NI = Drestriction(I,w), Ni = Drestriction(i,M,w),
     NIi = Drestriction(i,I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => Module => {"the i-th derived integration module of ", EM "M"," with respect
     to the weight vector ", EM "w"},
     	  "N" => HashTable => {"contains entries of the form ", TT "i=>Ni"},
	  "NIi" => Module => {"the i-th derived integration module of ", EM "D/I", " with respect
     to the weight vector ", EM "w"},
     	  "NI" => HashTable => {"contains entries of the form ", TT "i=>NIi"}
	  },
     "The derived restriction modules of a D-module M are
     the derived inverse images in the sense of algebraic
     geometry but in the category of D-modules. 
     This routine computes restrictions to coordinate subspaces,
     where the subspace is determined
     by the strictly positive entries of the weight vector", EM "w", ",
     e.g., ", EM "{x_i = 0 : w_i > 0}", " if ", 
     EM "D = ", BOLD "C", EM "<x_1,...,x_n,d_1,...,d_n>", ".
     The input weight vector should be a list of ", EM "n", " numbers
     to induce the weight ", EM "(-w,w)", " on ", EM "D", ".",

     PARA {
	  "The algorithm used appears in the paper 'Algorithims for D-modules'
	  by Oaku-Takayama(1999).  The method is to compute an adapted resolution
	  with respect to the weight vector w and use the b-function with respect
	  to w to truncate the resolution."},
     EXAMPLE lines ///
	     R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
	     I = ideal(x_1, D_2-1) 
	     Drestriction(I,{1,0})
	     ///,
     Caveat =>{"The module ", EM "M", " should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector ", EM "w", " should be a list of ", EM "n",
	  " numbers if ", EM "M", 
	  " is a module over the ", EM "n", "-th Weyl algebra."},
     SeeAlso => {"DrestrictionAll", "DrestrictionClasses", "DrestrictionComplex", 
	  "DrestrictionIdeal", "Dresolution", "Dintegration"}
     }

document {
     Key => {DrestrictionIdeal, (DrestrictionIdeal, Ideal, List)},
     Headline => "restriction ideal of a D-module",
     Usage => "DrestrictionIdeal(I,w)",
     Inputs => {
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  Ideal => {"the restriction ideal of ", EM "M", " w.r.t. the weight vector ", EM "w"}
	  },
     "A suplementary function for ", TO "Drestriction", 
     " that computes the restriction ideal.",   
     EXAMPLE lines ///
          W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}];
     	  I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2); -- annihilator of 1/(t^2-y)
     	  DrestrictionIdeal(I, {1,4})
	  ///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}

document {
     Key => {DrestrictionAll, (DrestrictionAll, Module, List), (DrestrictionAll, Ideal, List)},
     Headline => "restriction modules of a D-module (extended version)",
     Usage => "N = DrestrictionAll(M,w), NI = DrestrictionAll(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the restriction complex, restriction classes, etc.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionAll(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionClasses", "DrestrictionComplex", 
	  "DrestrictionIdeal", "Dintegration"}
}

document {
     Key => {DrestrictionComplex, (DrestrictionComplex, Module, List), (DrestrictionComplex, Ideal, List)},
     Headline => "derived restriction complex of a D-module",
     Usage => "N = DrestrictionComplex(M,w), NI = DrestrictionComplex(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable 
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the derived restriction complex.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionComplex(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}


document {
     Key => {DrestrictionClasses, (DrestrictionClasses,ZZ,Module,List), (DrestrictionClasses,Ideal,List), (DrestrictionClasses,Module,List),
      (DrestrictionClasses,ZZ,Ideal,List)},
     Headline => "restriction classes of a D-module",
     Usage => "N = DrestrictionClasses(M,w), NI = DrestrictionClasses(I,w), Ni = DrestrictionClasses(i,M,w),
     NIi = DrestrictionClasses(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => HashTable,
     	  "N" => HashTable,
	  "NIi" => HashTable,
	  "NI" => HashTable
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the explicit cohomology classes of a derived restriction complex.",        
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionClasses(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}

document {
     Key => {Drestrict, (Drestrict, ZZ, Module, List), (Drestrict, Ideal, List), 
	  (Drestrict, Module, List), (Drestrict, ZZ, Ideal, List),
	  DrestrictAll, (DrestrictAll, Module, List), (DrestrictAll, Ideal, List),
	  DrestrictClasses, (DrestrictClasses,Module,List,ZZ), (DrestrictClasses,Ideal,List), 
	  (DrestrictClasses,Module,List),(DrestrictClasses,Ideal,List,ZZ),
	  DrestrictComplex, (DrestrictComplex,Module,List,ZZ), (DrestrictComplex,Ideal,List), 
	  (DrestrictComplex,Module,List),(DrestrictComplex,Ideal,List,ZZ),
	  DrestrictIdeal, (DrestrictIdeal,Ideal,List)},
     Headline => "an (OBSOLETE) abbreviation for Drestriction",
     SeeAlso =>{"Drestriction"}
     }	

document {
     Key => HomologyModules,
     Headline => "a key in a hashtable; an option of DExt",
     SeeAlso => {"Drestriction", "Dintegration", "DExt" }
     }
document {
     Key => GenCycles,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Exponents,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Cycles,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Boundaries,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => VResolution,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => BFunction,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Explicit,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }

document {
     Key => IntegrateComplex,
     Headline => "a key in the hashtable created by Dintegration",
     SeeAlso => {"Dintegration" }
     }

document {
     Key => [Dintegration,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => { Dintegration, (Dintegration,ZZ,Module,List), (Dintegration,Ideal,List), 
	  (Dintegration,Module,List), (Dintegration,ZZ,Ideal,List) },
     Headline => "integration modules of a D-module",
     Usage => "N = Dintegration(M,w), NI = Dintegration(I,w), Ni = Dintegration(i,M,w),
     NIi = Dintegration(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => Module => {"the i-th derived integration module of ", EM "M"," with respect
     to the weight vector ", EM "w"},
     	  "N" => HashTable => {"contains entries of the form ", TT "i=>Ni"},
	  "NIi" => Module => {"the i-th derived integration module of ", EM "D/I", " with respect
     to the weight vector ", EM "w"},
     	  "NI" => HashTable => {"contains entries of the form ", TT "i=>NIi"}
	  },
     "The derived integration modules of a D-module ", EM "M", " are
     the derived direct images in the category of D-modules. 
     This routine computes integration for projection to 
     coordinate subspaces, where the subspace is determined
     by the strictly positive entries of the weight vector ", EM "w", ",
     e.g., ", EM "{x_i = 0 : w_i > 0}", " if ", 
     EM "D = ", BOLD "C", EM "<x_1,...,x_n,d_1,...,d_n>", ".
     The input weight vector should be a list of ", EM "n", " numbers	    
     to induce the weight ", EM "(-w,w)", " on ", EM "D", ".",
     PARA "",
     "The algorithm used appears in the paper 'Algorithims for D-modules'
     by Oaku-Takayama(1999).  The method is to take the Fourier transform
     of M, then compute the derived restriction, then inverse
     Fourier transform back.",
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	Dintegration(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"DintegrationAll", "DintegrationClasses", "DintegrationComplex", 
	  "DintegrationIdeal", "Drestriction"}
 }

document {
     Key => {DintegrationIdeal, (DintegrationIdeal, Ideal, List)},
     Headline => "integration ideal of a D-module",
     Usage => "DintegrationIdeal(I,w)",
     Inputs => {
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  Ideal => {"the integration ideal of ", EM "M", " w.r.t. the weight vector ", EM "w"}
	  },
     "A suplementary function for ", TO "Dintegration", 
     " that computes the integration ideal.",   
     EXAMPLE lines ///
          W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}];
     	  I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2); -- annihilator of 1/(t^2-y)
     	  DintegrationIdeal(I, {1,4})
	  ///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}

document {
     Key => {DintegrationAll, (DintegrationAll, Module, List), (DintegrationAll, Ideal, List)},
     Headline => "integration modules of a D-module (extended version)",
     Usage => "N = DintegrationAll(M,w), NI = DintegrationAll(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the integration complex, integration classes, etc.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationAll(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationClasses", "DintegrationComplex", 
	  "DintegrationIdeal", "Drestriction"}
}

document {
     Key => {DintegrationComplex, (DintegrationComplex, Module, List), (DintegrationComplex, Ideal, List)},
     Headline => "derived integration complex of a D-module",
     Usage => "N = DintegrationComplex(M,w), NI = DintegrationComplex(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable 
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the derived integration complex.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationComplex(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}


document {
     Key => {DintegrationClasses, (DintegrationClasses,ZZ,Module,List), (DintegrationClasses,Ideal,List), (DintegrationClasses,Module,List),
      (DintegrationClasses,ZZ,Ideal,List)},
     Headline => "integration classes of a D-module",
     Usage => "N = DintegrationClasses(M,w), NI = DintegrationClasses(I,w), Ni = DintegrationClasses(i,M,w),
     NIi = DintegrationClasses(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => HashTable,
     	  "N" => HashTable,
	  "NIi" => HashTable,
	  "NI" => HashTable
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the explicit cohomology classes of a derived integration complex.",        
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationClasses(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}

document {
     Key => {
	  Dintegrate, (Dintegrate,ZZ,Module,List), (Dintegrate,Ideal,List), 
	  (Dintegrate,Module,List), (Dintegrate,ZZ,Ideal,List),
	  DintegrateAll, (DintegrateAll,Module,List), (DintegrateAll,Ideal,List),
	  DintegrateClasses, (DintegrateClasses,ZZ,Module,List), (DintegrateClasses,Ideal,List), 
	  (DintegrateClasses,Module,List),(DintegrateClasses,ZZ,Ideal,List),
	  DintegrateComplex, (DintegrateComplex,Module,List), (DintegrateComplex,Ideal,List),
	  DintegrateIdeal,(DintegrateIdeal, Ideal, List)
	  },
     Headline => "Dintegrate* is an (OBSOLETE) abbreviation for Dintegration*",
     SeeAlso =>{"Dintegration", "DintegrationAll", "DintegrationClasses", 
	  "DintegrationComplex", "DintegrationIdeal"}
     }	
document {
     Key => [gkz,Vars] }
document {
     Key => [AppellF1,Vars] }
document {
     Key => Vars }

document {
     Key => {gkz, --(gkz, Matrix), 
	  (gkz, Matrix, List)},
     Headline => "GKZ A-hypergeometric ideal",
     Usage => "gkz(A,b)",
     Inputs => {
	  "A" => Matrix,
	  "b" => List 
	  },
     Outputs => {
     	  Ideal => "which represents the Gel'fand-Kapranov-Zelevinsky hypergeometric system
     associated to the matrix A and the parameter vector b"
	  },
     "The GKZ hypergeometric system of PDE's associated to a (d x n)
     integer matrix A consists of the toric ideal I_A in the polynomial
     subring C[d_1,...,d_n] and Euler relations given by the entries
     of the vector (A theta - b), where theta is the vector
     (theta_1,...,theta_n)^t, and theta_i = x_i d_i.
     See the book 'Groebner deformations of hypergeometric differential 
     equations' by Saito-Sturmfels-Takayama (1999) for more details.",
     EXAMPLE lines ///
	A = matrix{{1,1,1},{0,1,2}}
     	b = {3,4}
     	I = gkz (A,b)
	///,
     Caveat =>{"gkz always returns a different ring and will use variables
	  x_1,...,x_n, D_1,...D_n."},
     SeeAlso => {"AppellF1"}
     },

document {
     Key => {(AppellF1, List), AppellF1},
     Headline => "Appell F1 system of PDE's",
     Usage => "AppellF1 {a0,a1,a2,a3}",
     Inputs => {
	  "{a0,a1,a2,a3}"
	  },
     Outputs => {
	  Ideal => "which represents Appell F1 system of PDE's associated to the
     	  parameters a0, a1, a2, and a3."
	  },
     EXAMPLE lines ///
	w = {1,4/5,-2,3/2}
     	I = AppellF1 w
	///,
     Caveat =>{"AppellF1 always returns a different ring and will
	  use variables x and y. Input should be a List of 4
	  numbers."},
     SeeAlso => {"gkz"}
     }

document {
     Key => {(PolyAnn, RingElement), PolyAnn},
     Headline => "annihilator of a polynomial in Weyl algebra",
     Usage => "PolyAnn f",
     Inputs => {
     	  "f" => RingElement => "polynomial"
	  },
     Outputs => {
     	  Ideal => {"the annihilating (left) ideal of ", EM "f", "in the Weyl algebra"}
	  },
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	f = x^2-y^3
     	I = PolyAnn f
	///,
     Caveat =>{"The input f should be an element of a Weyl algebra,
	  and not an element of a commutative polynomial ring.
	  However, f should only involve commutative variables."},
     SeeAlso => {"RatAnn"}
     }

document {
     Key => {RatAnn, (RatAnn, RingElement, RingElement), (RatAnn, RingElement)},
     Headline => "annihilator of a rational function in Weyl algebra",
     Usage => "RatAnn f, RatAnn(g,f)",
     Inputs => {
	  "f" => RingElement => "polynomial",
	  "g" => RingElement => "polynomial"
	  },
     Outputs => {
     	  Ideal => "left ideal of the Weyl algebra"
	  },
     TT "RatAnn f", " computes the annihilator ideal in the Weyl algebra of the rational
     function 1/f",
     BR{},
     TT "RatAnn(g,f)", " computes the annihilator ideal in the Weyl algebra of the rational
     function g/f",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	f = x^2-y^3
     	g = 2*x*y
     	I = RatAnn (g,f)
	///,
     Caveat =>{"The inputs f and g should be elements of a Weyl algebra,
	  and not elements of a commutative polynomial ring.
	  However, f and g should only use the commutative variables."},
     SeeAlso => {"PolyAnn"}
     }

document {
     Key => {WeylClosure, (WeylClosure, Ideal), (WeylClosure, Ideal, RingElement)},
     Headline => "Weyl closure of an ideal",
     Usage => "WeylClosure I, WeylClosure(I,f)",
     Inputs => {
	  "I" => Ideal => "a left ideal of the Weyl Algebra",
	  "f" => RingElement => "a polynomial"  
	  },
     Outputs => {
	  Ideal => {"the Weyl closure (w.r.t. ", TEX "f", ") of ", TEX "I"}
	  },
     "Let ", TEX "R = K(x_1..x_n)", "<", TEX "d_1..d_n", ">", " denote the ring of differential
     operators with rational function coefficients. The Weyl closure
     of an ideal ", TEX "I", " in ", TEX "D"," is the intersection of the extended ideal ", 
     TEX "RI", " with ", TEX "D", ".  It consists of all operators which vanish on the common
     holomorphic solutions of ", TEX "I", " and is thus analogous to the radical
     operation on a commutative ideal.",
     PARA "",
     "The partial Weyl closure of ", TEX "I", " with respect to a polynomial ", TEX "f",
     " is the intersection of the extended ideal ", TEX "D[f^{-1}] I", " with ", TEX "D", ".",
     PARA "",
     "The Weyl closure is computed by localizing ", TEX "D/I", " with respect to
     a polynomial f vanishing on the singular locus, and computing
     the kernel of the map ",  "D --> D/I --> (D/I)[f^{-1}]", ".",
     EXAMPLE lines ///
	W = QQ[x,Dx, WeylAlgebra => {x=>Dx}]
     	I = ideal(x*Dx-2)
     	WeylClosure I
	///,
     Caveat =>{"The ideal I should be finite holonomic rank, which can be tested
	  manually by holonomicRank.", "The Weyl closure of non-finite rank
	  ideals or arbitrary submodules has not been implemented."},
     SeeAlso => {"Dlocalize", "singLocus", "holonomicRank"}
     }

document {
     Key => [deRham,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => [deRhamAll,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => {(deRhamAll, RingElement), deRhamAll},
     Headline => "deRham complex for the complement of a hypersurface",
     Usage => "deRhamAll f",
     Inputs => {"f"},
     Outputs => {
	  HashTable => {"containing explicit cohomology classes 
	       in the deRham complex for the complement 
	       of the hypersurface ",  EM "{f = 0}", " and
     	       supplementary information"}	       
	  },
     "The routine deRhamAll can be used to compute cup product structures
     as in the paper 'The cup product structure for complements
     of affine varieties' by Walther(2000).",
     PARA{},
     "For a more basic functionality see ", TO "deRham", ".",
     EXAMPLE lines ///
	R = QQ[x,y]
     	f = x^2-y^3 
     	deRhamAll f
	///,
     SeeAlso => {"deRham", "Dlocalize", "Dintegration"}	
} 

document {
     Key => {deRham, (deRham, ZZ, RingElement), (deRham, RingElement)},
     Headline => "deRham cohomology groups for the complement of a hypersurface",
     Usage => "M = deRham f, Mi = deRham(i,f)",
     Inputs => {
	  "i" => ZZ,
	  "f" => RingElement
	  },
     Outputs => {
	  "Mi" => Module => {"the i-th deRham cohomology group of the complement 
	  of the hypersurface ",  EM "{f = 0}"},
	  "M" => HashTable => {"containing the entries of the form ", TT "i=>Mi"}  
	  },
     "The algorithm used appears in the paper 'An algorithm for deRham 
     cohomology groups of the complement of an affine variety via D-module
     computation' by Oaku-Takayama(1999).  
     The method is to compute the localization of the polynomial ring 
     by f, then compute the derived integration of the localization.",
     EXAMPLE lines ///
	R = QQ[x,y]
     	f = x^2-y^3 
     	deRham f
	deRham(1,f)
	///,
     SeeAlso => {"deRhamAll", "Dlocalize", "Dintegration"}
     }
document {
     Key => TransferCycles,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => CohomologyGroups,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => PreCycles,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => LocalizeMap,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => OmegaRes,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }

document {
     Key => {diffOps, (diffOps, RingElement, ZZ), (diffOps, Ideal, ZZ)},
     Headline => "differential operators of up to the given order 
     for a quotient polynomial ring",
     Usage => "diffOps (I, k), diffOps (f, k)",
     Inputs => {
	  "I" => Ideal => {"contained in a polynomial ring ", EM "R"},
	  "f" => RingElement => {"an element of a polynomial ring ", EM "R"},
	  "k" => ZZ => "which is nonnegative"
	  },
     Outputs => {
     	  HashTable => {"the differential operators of order at most ", EM "k",  
     	  "of the quotient ring ", EM "R/I", " (or ", EM "R/(f)", ")"}  
	  },
     "Given an ideal ", EM "I", " of a polynomial ring ", EM "R", " the set of
     differential operators of the quotient ring ", EM "R/I", " having order 
     less than or equal to ", EM "k", " forms a finitely generated module over ", 
     EM "R/I",". This routine returns its generating set.",     
     PARA{},
     "The output is in the form of a hash table.
     The key ", TT "BasisElts", " is a row vector of basic differential operators.
     The key ", TT "PolyGens", " is a matrix over ", EM "R", " whose column vectors represent 
     differential operators of ", EM "R/I", " in the following way.  For each column
     vector, consider its image in ", TT "R/I", ", then take its dot product with
     the ", TT "BasisElts", ". This gives a differential operator, and
     the set of these operators generates the differential operators of ",
     EM "R/I", " of order ", EM "k", " or less as an ", EM "(R/I)", "-module.",
     EXAMPLE lines ///
	R = QQ[x,y,z]
     	I = ideal(x^2-y*z) 
     	diffOps(I, 3)
	///,
     SeeAlso => {"putWeylAlgebra"}
     },

document {
     Key => PolyGens,
     Headline => "a key of the hashtable generated by diffOps",
     SeeAlso => "diffOps"
     }

document {
     Key => BasisElts,
     Headline => "a key of the hashtable generated by diffOps",
     SeeAlso => "diffOps"
     }

document {
     Key => {(putWeylAlgebra, HashTable), putWeylAlgebra},
     Headline => "transforms output of diffOps into elements of Weyl algebra",
     Usage => "putWeylAlgebra m",
     Inputs => {
     	  "the output of diffOps"
	  },
     Outputs => {
	  "the differential operators as elements of the Weyl algebra"
	  },
     "If I is an ideal of the polynomial ring R and m is the output of ", 
     TT "diffOps(I, k)", " then this routine returns elements of the Weyl
     algebra ", TT "W", " corresponding to ", TT "R", " whose images in ", TT "W/IW", 
     " are an ", TT "R/I", "-generating set for the differential operators of order at most ", 
     TT "k", ".",
     EXAMPLE lines ///
	R = QQ[x,y,z]
     	I = ideal(x^2-y*z) 
     	m = diffOps(I, 3)
     	putWeylAlgebra m
	///,
     SeeAlso => {"diffOps"}
     }

document {
     Key => {inw, (inw, Matrix, List), (inw, RingElement, List), (inw, Ideal, List)},
     Headline => "initial form/ideal w.r.t. a weight",
     Usage => "inF = inw(F,w), inI = inw(I,w), inM = inw(M,w)",
     Inputs => {
	  "F" => RingElement => "an element of the Weyl algebra",
	  "I" => Ideal => "in the Weyl algebra",
	  "M" => Matrix => "with entries in the Weyl algebra",
	  "w" => List => "of weights"
	  },
     Outputs => {
	  "inF" => RingElement => {"the initial form of ", EM "F", " with respect to the weight vector"}, 
	  "inI" => Ideal => {"the initial ideal of ", EM "I", " with respect to the weight vector"}, 
	  "inM" => Matrix => {"with the columns generating the initial module of the image of ", EM "M",
	       " with respect to the weight vector"}
	  },
     "This routine computes the initial ideal of a left ideal ", EM "I",  
     " of the Weyl algebra with respect to a weight vector ", EM "w = (u,v)",
     " where ", EM "u+v >= 0", ".
     In the case where u+v > 0, then the ideal lives in the 
     associated graded ring which is a commutative ring.  In the case
     where u+v = 0, then the ideal lives in the associated graded
     ring which is again the Weyl algebra.  In the general case ", 
     EM "u+v >= 0",
     " the associated graded ring is somewhere between.  There are
     two strategies to compute the initial ideal.  One is to homogenize
     to an ideal of the homogeneous Weyl algebra.  The other is
     to homogenize with respect to the weight vector ", EM "w", 
     ".",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy) 
     	inw(I, {1,3,3,-1})
     	inw(I, {-1,-3,1,3})
	///,
     Caveat =>{"The weight vector ", EM "w = (u,v)", " must have ", 
	  EM "u+v>=0", "."},
     SeeAlso => {"gbw", "setHomSwitch"}
     }

document {
     Key => {gbw, (gbw, Ideal, List), (gbw, Matrix, List)},
     Headline => "Groebner basis w.r.t. a weight",
     Usage => "gbI = gbw(I,w), gbM = gbw(M,w)",
     Inputs => {
	  "I" => Ideal => "in the Weyl algebra",
	  "M" => Matrix => "with entries in the Weyl algebra",
	  "w" => List => "of weights"
	  },
     Outputs => {
	  "gbI" => Ideal => "with the generators forming a Grobner basis 
	  of the ideal with respect to the weight vector", 
	  "gbM" => Matrix => "with the columns forming a Grobner basis
	  of the submodule generated by the columns of the matrix 
	  with respect to the weight vector"
	  },
     "This routine computes a Groebner basis of a left ideal ", EM "I",  
     " of the Weyl algebra with respect to a weight vector ", EM "w = (u,v)",
     " where either ", EM "u+v > 0", " or ", EM "u+v = 0", 
     ".  In the case where ", EM "u+v > 0",
     " the ordinary Buchberger algorithm works for any term order
     refining the weight order. In the case
     where ", EM "u+v = 0", " the Buchberger algorithm needs to be adapted to
     guarantee termination.  There are two strategies for doing this.  
     One is to homogenize
     to an ideal of the homogeneous Weyl algebra.  The other is
     to homogenize with respect to the weight vector ", EM "w", ".",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy) 
     	gbw(I, {1,3,3,-1})
     	gbw(I, {-1,-3,1,3})
	///,
     Caveat =>{"The weight vector ", EM "w = (u,v)", " must have ", 
	  EM "u+v>=0", "."},
     SeeAlso => {"inw", "setHomSwitch"}
     }

document {
     Key => {pInfo, (pInfo, ZZ, Thing), (pInfo, ZZ, List)},
     Headline => "prints tracing info",
     "Prints tracing information according to the print level set by ", 
	TT "Dtrace", ".",
     SeeAlso => { "Dtrace" }
     }
----------------------------------------------------------------------------
-- (better docs needed)
----------------------------------------------------------------------------
document {
     Key => {Dprune, (Dprune, Matrix), (Dprune, Module)},
     Headline => "prunes a matrix over a Weyl algebra",
     Usage => "Dprune M",
     Inputs => {
	  "M" => {ofClass Matrix, " or ", ofClass Module} 
	  },
     Outputs => {
	  {ofClass Matrix, " or ", ofClass Module, " of the same type as ", TT "M"} 
	  },
     -- "description",
     -- EXAMPLE lines ///
     --   	  XXXXXXXXXXXXXXX  
     -- 	  ///,
     -- Caveat => {},
     -- SeeAlso => {}
     }
document {
     Key => [Dprune,optGB],
     Headline => "indicates whether Grobner basis should be computed"
     }
document {
     Key => optGB,
     Headline => "indicates whether Grobner basis should be computed"
     }
document {
     Key => {
	  FourierInverse, (FourierInverse,Matrix), (FourierInverse,Ideal), 
	  (FourierInverse,ChainComplex), (FourierInverse,RingElement),
      	  (FourierInverse,Module)
	  },
     Headline => "Inverse Fourier map (D-modules)",
     " see ", TO "Fourier" 
     }

document {
     Key => {(stafford, Ideal), stafford},
     Headline => "computes 2 generators for a given ideal in the Weyl algebra",
     Usage => "stafford I",
     Inputs => {
	  "I" => "in the Weyl algebra"
	  },
     Outputs => {
     	  Ideal => "with 2 generators (that has the same extension as I in k(x)<dx>)"
	  },
     PARA {"A theorem of Stafford says that every ideal in the Weyl algebra 
     	  can be generated by 2 elements. This routine is the implementation of the 
     	  effective version of this theorem following the constructive proof in ",
     	  EM "A.Leykin, `Algorithmic proofs of two theorems of Stafford', 
     	  Journal of Symbolic Computation, 38(6):1535-1550, 2004."
	  },
     PARA {"The current implementation provides a weaker result: the 2 generators 
	  produced are guaranteed to generate only the extension of the ideal ", EM "I", 
	  " in the Weyl algebra with rational-function coefficients."
	  },  
     EXAMPLE lines ///
     R = QQ[x_1..x_4,D_1..D_4, WeylAlgebra=>(apply(4,i->x_(i+1)=>D_(i+1)))] 
     stafford ideal (D_1,D_2,D_3,D_4)
          ///,
     Caveat => {"The input should be generated by at least 2 generators. 
	  The output and input ideals are not equal necessarily."},
     SeeAlso => {"makeCyclic"}
}

document {
     Key => {BMM, (BMM, List, RingElement), (BMM, Ideal, RingElement)},
     Headline => "the characteristic cycle of the localized $D$-module",
     Usage => "BMM(cc,f), BMM(I,cc)",
     Inputs => {
	  "cc" => List => {"the characteristic cycle of a regular holonomic ", TEX "D-module $M$"},
	  "I" => Ideal => {"representing an `simple' ", TT "cc"}   
	  },
     Outputs => {
     	  "List" => TEX "the characteristic cycle of the localized module $M_f = M[f^{-1}]$" 
	  },
     PARA {"Provided a characteristic cycle in the form ", TT "{I_1 => m_1, ..., I_k => m_k}", 
     " with associated prime ideals ", TEX "I_1,...,I_k", " and the multiplicities ", TEX "m_1,...,m_k", 
     " of ", TEX "M", " along them, the routine computes the characteristic cycle of ", TEX "M_f", "."},
     PARA {"The method is based on a geometric formula given by V.Ginsburg in ", 
     	  EM "Characteristic varieties and vanishing cycles, Invent. Math. 84 (1986), 327--402.", " and 
	  reinterpreted by J.Briancon, P.Maisonobe and M.Merle in ", 
	  EM "Localisation de systemes differentiels, stratifications de
  	  Whitney et condition de Thom, Invent. Math. 117 (1994), 531--550", "."}, 
     EXAMPLE lines ///
     	  A =  QQ[x_1,x_2,a_1,a_2]
	  cc = {ideal A => 1} -- the characteristic ideal of R = CC[x_1,x_2] 
	  cc1 = BMM(cc,x_1)   -- cc of R_{x_1}
	  cc12 = BMM(cc1,x_2) -- cc of R_{x_1x_2}
	  ///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {pruneCechComplexCC,populateCechComplexCC}
     }

document {
     Key => {(pruneCechComplexCC, MutableHashTable), pruneCechComplexCC},
     Headline => "reduction of the Cech complex that produces characteristic 
     cycles of local cohomology modules",
     Usage => "pruneCechComplexCC M",
     Inputs => {
     	  "M" => {"the output of ", TO "populateCechComplexCC"}
	  },
     Outputs => {
     	  MutableHashTable
	  },
     "The function reduces the Cech complex skeleton produced by ", TO "populateCechComplexCC", 
     " leaving the pieces of the characteristic cycles of the chains that together constitute 
     the characteristic cycles of the local cohomology modules.",    
     EXAMPLE lines ///
W =  QQ[x_1..x_6, a_1..a_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
cc = {ideal W => 1};
///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {BMM,populateCechComplexCC}
     }

document {
     Key => {(populateCechComplexCC, Ideal, List), populateCechComplexCC},
     Headline => "Cech complex skeleton for the computation of the characteristic 
     cycles of local cohomology modules",
     Usage => "populateCechComplexCC(I,cc)",
     Inputs => {
	  "I" => {"at which the local cohomology modules ", TEX "H^i_I(M)", " are computed."},
	  "cc" => {"the characteristic cycle of a regular holonomic module ", TEX "M"} 
	  },
     Outputs => {
     	  MutableHashTable => {"with entries corresponding to the direct summands of the chains in the Cech complex"}
	  },
     "For the ideal ", TEX "I=(f_1,...,f_k)", " the routine computes the characteristic cycles of the localized modules ", 
     TEX "M_{f_{i_1},...,f_{i_k}}", " and places them in the corresponding places in the Cech complex.",
     EXAMPLE lines ///
W =  QQ[x_1..x_6, a_1..a_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
cc = {ideal W => 1};
///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {BMM,pruneCechComplexCC}
     }

document {     
     Key => {(logCohomology,RingElement),logCohomology},
     Headline => "logarithmic cohomology groups in two variables",
     Usage => "logCohomology f",
     Inputs => {
	  "f" => {"polynomial in two variables"}
	  },
     Outputs => {
	  HashTable => {"with entries 
	       {VResolution, Input, TransferCycles, CohomologyGroups, PreCycles, OmegaRes, LocalizeMap, BFunction}
	       "} 
	  },
     "For a polynomial ", TEX "f", " in two variables executes the algorithm described in
     Castro-Jimenez and Takayama \"The Computation of the Logarithmic Cohomology for Plane Curves\" (arXiv:0712.0001).",
     EXAMPLE lines ///
S=QQ[x,y];
f=x*y*(x-y);
logCohomology(f)
///,
     SeeAlso => {deRham}
     }

document { -- local?
     Key => {ExternalProduct, (ExternalProduct,ChainComplex,ChainComplex), (ExternalProduct,Module,Module)},
     Headline => "external product of modules or complexes"
     }
document { -- local?
     Key => [ExternalProduct,TwistMap],
     Headline => "indicates whether TwistMap should be computed"
     }
document {
     Key => TwistMap,
     Headline => "indicates whether TwistMap should be computed"
     }

document {
     Key => twistMap,
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => twistInvMap,
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => (projMap1),
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => (projMap2),
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }


-- END better docs needed


document { 
     Key => {(localBFunction,RingElement,Ideal), localBFunction},
     Headline => "local b-function (a.k.a. the local Bernstein-Sato polynomial)",
     Usage => "b = localBFunction(f,P)",
     Inputs => {
	  "f" => {"a polynomial"},
	  "P" => {"a (prime) ideal"}
	  },
     Outputs => {
	  "b" => RingElement => {"the local b-function ", EM "b_P(s)",  " in ", EM "Q[s]"}
	  },
     PARA {
	  "Computes the local b-function of f at P, if P is a prime ideal."
	  },
     Caveat => {"(feature) If P is not prime, computes the 
	  lcm of local b-functions over all primes containing P"},
     	  EXAMPLE lines ///
	  R = QQ[x,y]; f = x^2*(x+y+1); P = ideal(x,y);
    	  b = localBFunction(f,P)
	  factorBFunction b
     	  ///,
     SeeAlso => { "bFunction", "factorBFunction", "globalBFunction", "generalB", "globalB" }
     }

document { 
     Key => {(multiplierIdeal, Ideal, QQ), (multiplierIdeal, Ideal, ZZ), (multiplierIdeal, Ideal, List), 
	  [multiplierIdeal, Strategy], [jumpingCoefficients, Strategy], 
	  [multiplierIdeal, DegreeLimit], [jumpingCoefficients, DegreeLimit], 
	  multiplierIdeal},
     Headline => "multiplier ideal",
     Usage => "mI = multiplierIdeal(I,c)",
     Inputs => {
	  "I" => {"an ideal in a polynomial ring"},
	  "c" => {"coefficient (or a list of coefficients)"}
	  },
     Outputs => {
	  "mI" => Ideal => {"multiplier ideal ", EM "J_I(c)", " (or a list of)"}
	  },
     PARA {
	  "Computes the multiplier ideal for given ideal and coefficient. "
	  },
     "There are three options for ", BOLD "Strategy", ":",
	  UL { 
	       { BOLD "ViaElimination", " -- the default;"},
	       { BOLD "ViaLinearAlgebra", " -- skips one expensive elimination step by using linear algebra;"},
	       { BOLD "ViaColonIdeal", " -- same as elimination, but may be slightly faster."}
	  },
     "The option ", BOLD "DegreeLimit", 
     " specifies the maximal degree of polynomials to consider for membership in the multiplier ideal.",
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     Caveat => {
	  "When ", BOLD "Strategy=>ViaLinearAlgebra", " the option ", BOLD "DegreeLimit", 
	  " must be specified. The output it guaranteed to be the whole multiplier ideal only when dim(I)=0. ",
	  "For positive-dimensional input the up-to-specified-degree part of the multiplier ideal is returned."
	  },
     EXAMPLE lines ///
R = QQ[x_1..x_4];
multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "jumpingCoefficients" }
     }

document { 
     Key => {(jumpingCoefficients, Ideal), (jumpingCoefficients, Ideal, QQ, QQ), (jumpingCoefficients, Ideal, QQ, ZZ), (jumpingCoefficients, Ideal, ZZ, QQ), (jumpingCoefficients, Ideal, ZZ, ZZ), jumpingCoefficients},
     Headline => "jumping coefficients and corresponding multiplier ideals",
     Usage => "(cs, mI) = jumpingCoefficients I, (cs, mI) = jumpingCoefficients(I,a,b)",
     Inputs => {
	  "I" => {"an ideal in a polynomial ring"}
	  },
     Outputs => {
	  "cs" => List => {"the list of jumping coefficients"},
	  "mI" => List => {"the list of corresponding multiplier ideals"}
	  },
     PARA {
	  "Computes the jumping coefficients and their multiplier ideals in an open interval (a,b). By default a = 0, b = ", TO "analyticSpread", " I. ",
	  "The options are passed to ", TO "multiplierIDeal",".",
	  },
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     EXAMPLE lines ///
R = QQ[x_1..x_4];
jumpingCoefficients ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}
     	  ///,
     SeeAlso => { "multiplierIdeal" }
     }

document { 
     Key => {(hasRationalSing, List), hasRationalSing},
     Headline => "check if a complete intersection has at most rational singularities",
     Usage => "b = hasRationalSing F",
     Inputs => {
	  "F" => {"a regular sequence (of polynomials)"}
	  },
     Outputs => {
	  "b" => Boolean => {"answers: are the singularities of the given variety at most rational?"}
	  },
     PARA {
	  
	  },
     EXAMPLE lines ///
R = QQ[x_1..x_4];
multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "jumpingCoefficients" }
     }


document { 
     Key => {(isInMultiplierIdeal, RingElement, Ideal, QQ), isInMultiplierIdeal, [isInMultiplierIdeal,Strategy], 
	  generalizedBFunction, mGeneralizedBFunction},
     Headline => "multiplier ideal membership test",
     Usage => "b = isInMultiplierIdeal(g,I,c)",
     Inputs => {
	  "g" => {"a polynomial"},
	  "I" => {"an ideal in a polynomial ring"},
	  "c" => {"coefficient (or a list of coefficients)"}
	  },
     Outputs => {
	  "b" => Boolean => {"answers: is in the multiplier ideal ", EM "J_I(c)", "?"}
	  },
     PARA {
	  "Test if the given polynomial is in the multiplier ideal for given ideal and coefficient. ",
	  "In general, the test is cheaper than computing the whole multiplier ideal. "
	  },
     "There are two options for strategy:",
	  UL { 
	       { BOLD "generalizedBFunction", 
	       " -- via computation of the generalized Bernstein-Sato polynomial"},
	       { BOLD "mGeneralizedBFunction", 
	       " -- via computation of the m-generalized Bernstein-Sato polynomial"}
	  },
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     EXAMPLE lines ///
R = QQ[x_1..x_4];
isInMultiplierIdeal(x_1, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
isInMultiplierIdeal(x_1*x_2, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "multiplierIdeal", "jumpingCoefficients", "generalB" }
     }


end
------------------------------------------------------------------------------------------------------------
THE END
restart
loadPackage "Dmodules"
uninstallPackage "Dmodules"
installPackage("Dmodules", SeparateExec=>true, AbsoluteLinks=>false, RerunExamples=>true)
check Dmodules
