document { "D-modules Package",
     "Welcome to the online documentation for the Macaulay 2 
     D-modules package.",
     PARA,
     HEADER3 "How to make Weyl algebras:",
     MENU{TO {"WeylAlgebra", " -- 
	       The class of Weyl algebras"}},
     
     PARA,
     HEADER3 "Grobner deformations and other basic commands:",
     MENU{--TO {"gbw", " -- Grobner bases with respect to weight vectors"},
	  --TO {"inw", " -- initial ideals with respect to weight vectors"},
	  --TO {"Fourier", " -- Fourier transform"},
	  --TO {"Dtransposition", " -- standard transposition"},
	  TO {"makeCyclic", " -- find a cyclic presentation"}
	  },
     
     --PARA,
     --HEADER3 "Some examples of D-modules:",
     --MENU{TO {"gkz", " -- Gelfand-Kapranov-Zelevinsky hypergeometric system"},
--	  TO {"AppellF1", " -- Appell F1 system"},
--	  TO {"PolyAnn", " -- annihilator of a polynomial"},
--	  TO {"RatAnn", " -- annihilator of a rational function"}},
     --PARA,
     --HEADER3 "Basic invariants of D-modules:",
     --MENU{TO {"Ddim", " -- dimension"}, 
--	  TO {"Drank"," -- holonomic rank"}, 
	--  TO {"charIdeal", " -- characteristic ideal"},
	 -- TO {"singLocus", " -- singular locus"}},

     PARA,
     HEADER3 "B-functions:",
     MENU {
	  TO {"bFunction", " -- b-function"}, 
	  TO {"globalBFunction", " -- global b-function"},
	  TO {"globalB", " -- global b-function and b-operator"},
	  TO {"globalBoperator", " -- global b-operator"},
	  TO {"paramBpoly", " -- Bernstein-Sato polynomials of a
	       polynomial with parametric coefficients"},
	  TO {"factorBFunction", " -- factors a univariate polynomial"},
	  TO {"getIntRoots", " -- gets integer roots of a b-function"},
	  TO {"AnnFs", " -- annihilator of f^s"},
	  TO {"AnnIFs", " -- annihilator ideal for an arbitrary D-module"}
	  },
     PARA,
     HEADER3 "Applications:",
     MENU{
	  TO {"localCohom", "-- local cohomology"}
	  --TO {"deRham", " -- deRham cohomology"},
	  --TO {"PolySols", " -- polynomial solutions of finite rank systems"},
	  --TO {"RatSols", " -- rational solutions of finite rank systems"},
	  --TO {"diffOps", " -- differential operators on affine varieties"}
	  },
     PARA,
     HEADER3 "Miscellaneous:",
     MENU{
	  --TO {"createDpairs", 
	  --     " -- tags coordinate and derivation variables"},
	  TO {"Dtrace", " -- toggles verbose comments"},
	  TO {"setHomSwitch", 
	       " -- toggles use of homogeneous Weyl algebra"}
	  }
    }

///

     PARA,
     HEADER3 "B-functions:",
     MENU{
	  makeLink docBFunction,
	  makeLink docGlobalBFunction,
	  makeLink docParamBpoly,
	  makeLink docFactorBFunction,
	  makeLink docGetIntRoots,
	  makeLink docAnnFs,
	  makeLink docAnnIFs
	  --TO {"bFunction", " -- b-function"}, 
	  --TO {"globalBFunction", " -- global b-function"},
	  --TO {"paramBpoly", " -- Bernstein-Sato polynomials of a
	  --     polynomial with parametric coefficients"},
	  --TO {"factorBFunction", " -- factors a univariate polynomial"},
	  --TO {"getIntRoots", " -- gets integer roots of a univariate polynomial"},
	  --TO {"AnnFs", " -- annihilator of f^s"},
	  --TO {"AnnIFs", " -- annihilator of f^s in D/I"}
	  },
     
     PARA,
     HEADER3 "Resolutions and Functors:",
     MENU{TO {"Dresolution", " -- resolutions"}, 
	  TO {"Dlocalize", " -- localization"}, 
     	  TO {"WeylClosure", " -- Weyl closure"},
	  TO {"Ddual", " -- holonomic dual"}, 
	  TO {"Drestriction", " -- derived restriction"},
	  TO {"Dintegration", " -- derived integration"},
	  TO {"DHom", " -- homomorphisms between holonomic D-modules"},
	  TO {"DExt", " -- Ext between holonomic D-modules"}},

               
     PARA,
     HEADER3 "Programming aids:",
     MENU{TO {"createDpairs", " -- tags coordinate and derivation variables"},
	  TO {"Dtrace", " -- toggles verbose comments"},
	  TO {"setHomSwitch", " -- toggles use of homogeneous Weyl algebra"}}
     }
///-----------------------------------------------
document { (bFunction, Ideal, List),
     Headline => "b-function of an ideal",
     Usage => {
	  TT "bFunction(I,w)", " -- find the b-function of ", EM "I", 
	  " with respect to weight vector ", EM "w"
	  },
     Synopsis => {
     	  "b = bFunction(I,w)",
	  "I" => {"a holonomic ideal in the Weyl algebra ", 
	       EM {"A", SUB "n", "(K)"}, "."},
	  "w" => {"a list of integer weights corresponding 
	       to the differential variables in the Weyl algebra."},
	  "b" => {"a polynomial ", EM "b(s)", " which is the b-function of ", 
	       EM "I", " with respect to ", EM "w"}
	  },
     "Use ", TO "setHomSwitch", "(true) to force all the subroutines 
     to use homogenized ", TO "WeylAlgebra",
     PARA,  
     BOLD "Definition. ", "The b-function ", EM "b(s)", 
     " is defined as the monic generator  
     of the intersection of ", 
     EM {"in", SUB "(-w,w)", "(I)"}, " and ", 
     EM "K[s]",
     ", where ", 
     EM {"s = [w", SUB "1", "t", SUB "1", " + ... + w", 
	  SUB "n", "t", SUB "n", "]"}, 
     " (here ", EM {"t", SUB "i", " = x", SUB "i", "D", SUB "i"}, ").", 
     EXAMPLE {
	  "R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]",
     	  "I = ideal(x_1, D_2-1)",
     	  "bFunction(I,{1, 0})",
     	  },
     CAVEAT {
	  "The ring of I should not have any parameters: 
     	  it should be a pure Weyl algebra. Similarly, this ring 
	  should not be a homogeneous ", TO "WeylAlgebra"
	  },
     SEEALSO { "globalBFunction", "factorBFunction" }
     }

document { (bFunction => Strategy),
     Headline => "specify strategy for computing b-function",
     MENU { 
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
	       }
	  }
     }

document { (bFunction, Module, List, List),
     Headline => "b-function of a holonomic D-module",
     Usage => {
	  TT "bFunction(M,w,m)", " -- find the b-function of ", EM "M", 
	  " with respect to weight vector ", EM "w", " and shift vector ", 
	  EM "m"
	  },
     Synopsis => {
	  "b = bFunction(M,w,m)",
	  "M" => {"a holonomic module over a Weyl algebra ", 
	       EM {"A", SUB "n", "(K)"}},
	  "w" => {"a list of integer weights corresponding 
	       to the differential variables in the Weyl algebra"},
	  "m" => {"a list of integers, each of which is 
	       the shift for the corresponding component"},
	  "b" => {"a polynomial ", EM "b(s)", " which is the b-function of ", 
	       	EM "M", " with respect to ", EM "w", 
	       	" and ", EM "m"}
	  },
     PARA,
     "The algorithm represents ", EM "M", " as ", EM "F/N", 
     " where ", EM "F", " is free and ", 
     EM "N", " is a submodule of ", EM "F", ". 
     Then it computes b-functions ", EM {"b", SUB "i", "(s)"}, 
     " for ", EM {"N \\cap F", SUB "i"}, " (i-th component of ", EM "F", 
     ") and outputs ",
     EM {"lcm{ b", SUB "i", "(s-m", SUB "i",") }"},
     PARA,
     EXAMPLE{
	  "R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
	  "M = cokernel matrix {{x^2, 0, 0}, {0, dx^3, 0}, {0, 0, x^3}}",
	  "factorBFunction bFunction(M, {1}, {0,0,0})",
	  "factorBFunction bFunction(M, {1}, {1,2,3})"
     	  },
     CAVEAT => {
	  "The Weyl algebra should not have any parameters. 
     	  Similarly, it should not be a homogeneous Weyl algebra"
	  },
     SEEALSO => { "globalBFunction", "factorBFunction" }
     }



document {  (globalBFunction, RingElement),
     Headline => {"global b-function (else known as 
	  the Bernstein-Sato polynomial)"},
     Usage => {
	  TT "globalBFunction f", " -- find the global b-function of ", TT "f"
	  },
     Synopsis => {
     	  "b = globalBFunction(f)",
	  "f" => {"a polynomial in a Weyl algebra 
	       (should not contain differential variables)"},
	  "b" => {"the b-function ", EM "b(s)",  " in ", EM "Q[s]"}
	  },
     PARA,
     BOLD "Definition. ", "Let ", 
     EM "D = A_{2n}(K) = K[x_1,...,x_n,d_1,...,d_n]", 
     " be a Weyl algebra. 
     The Bernstein-Sato polynomial of a polynomial f is defined 
     to be the monic generator of the ideal of all polynomials ", 
     EM "b(s)", " in ", EM "K[s]", " such that
     ", EM " b(s) f^s = Q(s,x,d) f^{s+1}", " where ", EM "Q", 
     " lives in ", EM "D[s].",
     PARA,
     BOLD "Algorithm. ", 
     "Let ", 
     EM "I_f = D<t,dt>*<t-f, d_1+df/dx_1*dt, ..., d_n+df/dx_n*dt> ",
     "Let ", EM "B(s) = bFunction(I, {1, 0, ..., 0})", 
     " where 1 in the weight that corresponds to ", EM "dt. ", 
     "Then the global b-function is ", EM "b_f = B(-s-1)",
     EXAMPLE {
	  "R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
     	  "f = x^10",
    	  "globalBFunction f"
     	  },
     CAVEAT => {
	  "The Weyl algebra should not have any parameters. 
     	  Similarly, it should not be a homogeneous Weyl algebra"
	  },
     SEEALSO => { "bFunction", "factorBFunction" }
     }

document { (globalBFunction => Strategy),
     Headline => "specify strategy for computing global b-function",
     MENU { 
	  {BOLD "IntRing, TryGeneric, NonGeneric", 
	       " -- passed to ", TO "bFunction",  ", see ", 
	       TO (bFunction => Strategy) },
	  {BOLD "ViaAnnFs", " -- computes ", 
	       EM "J(s)=Ann(f", SUP "s", EM ")", " and then intersects ", 
	       EM "J(s)+D[s]f}", " with ", EM "K[s]"},
	  {BOLD "ReducedB", " -- computes ", EM "b(s)/(s+1)", 
	       " by taking the intersection of ",
	       EM "J(s)+D[s](f,df/dx1,...,df/dxn)", " with ", EM "K[s]"}
	  }
     }
               
document { (factorBFunction, RingElement),
     Headline => "factor b-function",
     Usage => {
	  TT "factorBFunction b", " -- factor polynomial ", TT "b"
	  },
     Synopsis => {
     	  "f = bFunction b",
	  "b" => {"a polynomial obtained via one of the b-function routines"},
	  "f" => {"the factorization of ", TT "b"}
	  },
     PARA,
     BOLD "Fact. ", "The roots of any b-function are rational.",
     EXAMPLE {
	  "R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
     	  "f = x^10",
     	  "b = globalBFunction f",
     	  "factorBFunction b"
     	  },
     CAVEAT {
	  "f should be an output of one of the b-function routines"
     	  },
     SEEALSO { 
	  "bFunction",
	  "globalBFunction"
	  }
     }  

document { (getIntRoots, RingElement),
     Headline => "get integer roots of a b-function"
     }

document { (globalB, Ideal, RingElement), 
     Headline => "compute global b-function and b-operator 
          for a D-module and a polynomial",
     Usage => {
	  TT "globalB(I,f)", " -- find global b-function and b-operator 
          for a D-module and a polynomial"
	  },
     Synopsis => {
	  "H = globalB(I,f)", 
	  "I" => {"a holonomic ideal"},
	  "f" => {"a polynomial in a Weyl algebra 
	       (should not contain differential variables)"},
	  "H" => {"a hashtable containing the fields ",  
	       TT "Bpolynomial", " and ", TT "Boperator"}
	  },
     PARA,
     "The algorithm used here is a modification of the original
     algorithm of Oaku for computing Bernstein-Sato polynomials",
     EXAMPLE {
	  "R = QQ[x, dx, WeylAlgebra => {x=>dx}]",
     	  "f = x^7",
     	  "b = globalB(ideal dx, f)",
     	  "factorBFunction b.Bpolynomial" 
     	  },
     SEEALSO { "bFunction", "globalBFunction", "factorBFunction" }
     }  

document { (globalBoperator, RingElement),
     Headline => "compute b-operator of a polynomial"
     } 

document { (AnnFs, RingElement),
     Headline => "annihilator of f^s",
     Usage => {
	  TT "AnnFs(f)", " -- find the annihilator ideal of ", 
	  EM {"f", SUP "s"}, " in the ring ", EM {"A", SUB "n", "[s]"}
	  },
     Synopsis => {
	  "I = AnnFs(f)",
	  "f" => { 
	       "a polynomial in a Weyl algebra ", EM {"A", SUB "n"},  
	       " (should contain no differential variables)" 
	       },
	  "I" => {"an ideal of ", EM {"A", SUB "n", "[s]"}}
	  },
     "The annihilator ideal is needed to compute a D-module 
     representation of the localization of ", 
     EM {"k[x", SUB "1", ",...,x", SUB "n", "]"}, " at ", EM "f", ".",
     EXAMPLE {
     	  "R = QQ[x_1..x_4, z, d_1..d_4, Dz, ", 
	  "       WeylAlgebra => (toList(1..4)/(i -> (x_i=>d_i)) | {z=>Dz})]",
     	  "f = x_1 + x_2 * z + x_3 * z^2 + x_4 * z^3",
     	  "AnnFs f"
     	  },
     CAVEAT {"
	  The ring of f should not have any parameters, 
     	  i.e. it should be a pure Weyl algebra. 
	  Also this ring should not be a homogeneous Weyl algebra."},
     SEEALSO {"AnnIFs", "WeylAlgebra"}
     }  

document { (AnnIFs, Ideal, RingElement), 
     Headline => "the annihilator ideal for an arbitrary D-module", 
     Usage => {
	  TT "AnnIFs(f)", " -- find the annihilator ideal of ", 
	  EM {"f", SUP "s", " \\otimes 1", SUB {"A", SUB "n", "/I"}}, 
	  " in the ring ", EM {"A", SUB "n", "[s]"}
	  },
     Synopsis => {
	  "J = AnnIFs(I,f)",
	  "I" => {
	       "represents a holonomic D-module ", 
	       EM {"A", SUB "n", "/I"}
	       },
	  "f" => {"an element of Weyl algebra", EM {"A", SUB "n"}},
	  "J" => {"the annihilating ideal"}
	  },
     EXAMPLE {
	  "W = QQ[x,dx, WeylAlgebra=>{x=>dx}]",
	  "AnnIFs (ideal dx, x^2)"
	  }, 
     CAVEAT => {"
     	  Caveats and known problems: The ring of f should not have any 
	  parameters: it should be a pure Weyl algebra. Similarly, 
	  this ring should not be a homogeneous Weyl algebra."
     	  },
     SEEALSO => {"AnnFs", "WeylAlgebra"}
     }  

document { (Dtrace, ZZ),
     Headline => "set the depth of comments made by D-module routines",
     Synopsis => {
	  "o = Dtrace n",
	  "n" => { "new level" },
	  "o" => { "old level" }
	  },
     SEEALSO {"getDtrace"}
     }  
document { getDtrace,
     Headline => "(internal) -- get the INFOLEVEL switch",
     SEEALSO {"Dtrace"}
     }  

document { (setHomSwitch, Boolean),
     Headline => "toggles use of homogeneous Weyl algebra",
     Usage => {"sets the switch that determines whether homogenized 
	  ", TO "WeylAlgebra", 
	  " is used in certain D-module algorithms"},
     Synopsis => {
	  "o = setHomSwitch n",
	  "n" => { "new value" },
	  "o" => { "old value" }
	  },
     SEEALSO {"getHomSwitch"}
     }  

document { getHomSwitch,
     Headline => "(internal) -- get the HOMOGENIZATION switch",
     SEEALSO {"setHomSwitch"}
     }  

document { (localCohom, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => {
	  TT "localCohom I", " -- find local cohomology ", 
	  EM {"H", SUB "I", "(R)"}, " where ", EM "I", 
	  " is an ideal of ", EM "R", ", which is a ring of polynomials" 
	  },
     Synopsis => { 
	  "H = localCohom I", 
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       },
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
     CAVEAT {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."}
     }  

document { (localCohom, List, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => {
	  TT "localCohom(l,I)",
	  " -- compute the local cohomology in the degrees specified by ", EM "l"
	  },
     "See ", TO "localCohom Ideal", " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom({1,2}, I)",
     	  "pruneLocalCohom h"
	  }
     }

document { (localCohom, ZZ, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => {
	  TT "localCohom(d,I)",
	  " -- compute the local cohomology in degree ", EM "d"
	  },
     "See ", TO "localCohom Ideal", " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom (2,I)",
     	  "pruneLocalCohom h"
	  }
     }

document { (localCohom => Strategy),
     Headline => "specify strategy for local cohomology",
     "This option together with ", TO "LocStrategy", " determines a strategy for ", 
     TT "localCohom(...Ideal...)", " and ", TT "localCohom(...Ideal, Module...)", ".",
     MENU { 
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
          --CAVEAT {"When WaltherOTW strategy is used the error 'Bad luck!' 
          --may appear. This means your are not a lucky individual...
	  --The glitch is due to the fact that the localizations are iterated 
	  --for this particular strategy; it was resolved for WaltherOaku, 
	  --a strategy that considers everyone lucky."
	  --},
     "For detailed description of the algorithms see",
     MENU {
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

document { (localCohom => LocStrategy),
     Headline => "specify localization strategy for local cohomology",
     SEEALSO "Strategy"
     }

document { (localCohom, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => {
	  TT "localCohom(I,M)", " -- find local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  },
     Synopsis => { 
	  "H = localCohom(I,M)", 
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       },
	  "M" => {
	       "a holonomic module over Weyl algebra ", 
	       EM{"A", SUB "n", "(k)"}
	       },
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
     CAVEAT {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."}
     }

document { (localCohom, ZZ, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => {
	  TT "localCohom(d,I,M)", " -- find local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degree ", EM "d", ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  },     
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom(2, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  }
     }

document { (localCohom, List, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => {
	  TT "localCohom(l,I,M)", " -- find local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degrees listed in ", EM "l", 
	  ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  },     
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom({1,2}, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  }
     }

document { (pruneLocalCohom, HashTable),
     Headline => "prunes local cohomology modules",
     SEEALSO {"localCohom"} 
     }

document { (paramBpoly, RingElement, String),
     Headline => "compute the list of all possible Bernstein-Sato polynomials 
     for a polynomial with parametric coefficients",
     Synopsis => { 
	  "L = paramBpoly(f,filename)", 	  
     	  "f" => {
	       "a polynomial in Weyl algebra ", EM "A_n(Q)"
	       },
	  "filename" => {"the base name for the output files"},
	  "L" => {"Bernstein-Sato polynomials"}
	  },
     "Also the file <filename.tex> that contains the list of 
     the BS-polynomials and the corresponding constructible sets.",
     EXAMPLE {
	  "A =  (QQ [a,b,c]) [x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]",
     	  "paramBpoly(a*x^2 + b*x*y + c*y^2, \"quadratic\")"
	  },
     CAVEAT {
	  "Z/pZ is used instead of Q in order to simplify computations,
	  where p is a large prime number. As of now the routine works only
	  on small examples, due to complexity of the Grobner bases 
	  computations used in the algorithm." 
	  }
     }  

document { (makeCyclic,Matrix),
     Headline => "finds a cyclic generator of a D-module",
     Synopsis => {
     	  "H = makeCyclic M", 
	  "M" => {
	       "a map such that ", TT "coker M", " is a 
	       holonomic D-module"
	       },
	  "H" => {TT "H.Generator", " is a cyclic generator
	       and ", TT "H.AnnG", " is the annihilator ideal 
	       of this generator"} 
	  },
     EXAMPLE{
	  "W = QQ[x, dx, WeylAlgebra => {x=>dx}]",
	  "M = matrix {{dx,0,0},{0,dx,0},{0,0,dx}} -- coker M = QQ[x]^3", 
	  "h = makeCyclic M"
	  }
     }  
