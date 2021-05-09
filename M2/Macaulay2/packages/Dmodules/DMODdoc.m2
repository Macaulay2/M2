-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

TEST /// input "Dmodules/TST/gkz.tst.m2" ///
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

load "Dmodules/DOC/tutorial.m2" -- basic tutorial
load "Dmodules/DOC/Dbasic.m2"   -- basic commands
load "Dmodules/DOC/Dsystems.m2" -- some examples of D-modules
load "Dmodules/DOC/canonicalSeries.m2" -- some examples of D-modules
load "Dmodules/DOC/general.m2"
load "Dmodules/DOC/localCohom.m2"

document {
     Key => "Dmodules",
     Headline => "algorithms for D-modules",
     
     "To begin, read the ", TO {"D-modules tutorial"}, ".",

     
     HEADER3 "How to make Weyl algebras:",
          
     UL{TO {"WeylAlgebra", " -- 
	       The class of Weyl algebras"},
     TO {"makeWeylAlgebra", 
	       " -- Weyl algebra associated to a polynomial ring"}},
     
     HEADER3 "Basic commands:",
     UL{
	  TO {"gbw", " -- Groebner bases with respect to weight vectors"},
	  TO {"inw", " -- initial ideals with respect to weight vectors"},
	  TO {"Fourier", " -- Fourier transform"},
	  TO {"Dtransposition", " -- standard transposition"},
	  TO {"makeCyclic", " -- cyclic presentation"},
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

-*
-- FIXME: this is excluded because the Macaulay2Doc package owns the WeylAlgebra key
document {
     Key => WeylAlgebra,
     TT "WeylAlgebra", " --
     name for an optional argument for a monoid that
     specifies that a PolynomialRing created from it will
     be a Weyl Algebra.",

     PARA{},
     "The n-th Weyl algebra is the associative ring on 2n variables,
     e.g., K<x_1..x_n, D_1..D_n>, where all the variables commute except
     for (D_i x_i = x_i D_i + 1).  It can be viewed as the ring
     of algebraic differential operators on affine space K^n.",

     PARA{},
     "A simple example:",
     EXAMPLE {
	"W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
     	"x*Dx", 
     	"Dx*x"},     
     PARA{},
     "Caveats and known problems:",
     UL{"The variables can be called by any name, but for each
	  pair such as x => Dx, the commutative variable (in this case x)
	  must be listed before the derivation variable (in this case Dx)"}
     }
*-

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
	       in Saito-Sturmfels-Takayama (1999) otherwise is equivalent 
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
     some auxiliary information",
     BR{},
     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of D/I with respect to f and some
     auxiliary information",
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
     "A supplementary function for ", TO "Drestriction", 
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
     "A supplementary function for ", TO "Dintegration", 
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
	  "The options are passed to ", TO "multiplierIdeal",".",
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
installPackage("Dmodules")
installPackage("Dmodules", SeparateExec=>true, RerunExamples=>true)
check Dmodules
