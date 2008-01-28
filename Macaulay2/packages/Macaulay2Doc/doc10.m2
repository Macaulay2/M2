--		Copyright 1993-2002 by Daniel R. Grayson


TEST ///
     R = QQ[x,y,z]
     C = res coker vars R
     D = C ++ C
     E = ker D_[0]
     E = coker D_[0]
     E = image D_[0]
     E = coimage D_[0]
///

document {
     Key => Resolution,
     Headline => "the class of all resolution computations",
     PARA{},
     "These resolutions are internal engine objects not meant to be examined
     by the user.",
     PARA{},
     "The symbol ", TT "Resolution", " is also used in a ", TO "ChainComplex", " to 
     store the resolution it comes from."
     }

document {  -- This node is used as an example in the node: Key
     Key => resolution, 
     Headline => "projective resolution"
     }
document {
     Key => [resolution,DegreeLimit],
     Headline => "compute only up to this degree",
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "resolution", " which specifies that the computation should halt
     after dealing with degree n.",
     PARA{},
     "This option is relevant only for homogeneous modules.",
     PARA{},
     Caveat => "One might get some matrix entries of slightly higher degree than requested.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2}}",
      	  "res(M,DegreeLimit => 1)",
      	  "res(M,DegreeLimit => 2)"
	  },
     }

document { -- This node is used as an example in the node: Key
     Key => [resolution,SyzygyLimit],
     Headline => "stop when this number of syzygies are obtained",
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     stop after a certain number of syzygies have computed.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}",
      	  "res(M,SyzygyLimit => 1)",
      	  "res(M,SyzygyLimit => 2)",
      	  "res(M,SyzygyLimit => infinity)"
	  }
     }

document {
     Key => [resolution,PairLimit],
     Headline => "stop when this number of pairs has been handled",
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced.",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
      	  "M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}",
      	  "res(M, PairLimit => 1)",
      	  "res(M, PairLimit => 10)",
      	  "res(M, PairLimit => 20)"
	  }
     }

document {
     Key => [resolution,StopBeforeComputation],
     Headline => "whether to stop the computation immediately",
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document {
     Key => LengthLimit,
     Headline => "stop when the resolution reaches this length",
     }

document {
     Key => [resolution,LengthLimit],
     Headline => "stop when the resolution reaches this length",
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA{},
     "For polynomial rings over a field or over the integers, the length
     is taken to be the dimension of the ring, so the complete resolution will
     be obtained.  For quotient rings of such rings, the same number is used,
     so the complete resolution may not be obtained.",
     PARA{},
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA{},
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }

document {
     Key => HardDegreeLimit,
     Headline => "compute only up to this degree",
     TT "HardDegreeLimit", " -- keyword for an optional argument which specifies
     that information above a specified degree is to be discarded."
     }

document {
     Key => [resolution,HardDegreeLimit],
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "The default value is ", TT "{}", ".",
     PARA{},
     "Information above the specified degree is discarded."
     }

document {
     Key => [resolution,Strategy],
     TT "Strategy => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Strategies are specified by number and the
     algorithms available are",
     UL {
	  SPAN (TT "Strategy => 0", " -- Compute syzygies on the Groebner bases of each syzygy
	       module.  The algorithm uses important speedups due to R. La Scala.
	       This algorithm appears to be on the average the fastest."),
	  SPAN (TT "Strategy => 1", " -- An older version of algorithm 0, which doesn't allow as 
	       much experimentation, but can sometimes be marginally faster."),
	  SPAN (TT "Strategy => 2", " -- Compute syzygies on the minimal generators of each 
	       matrix in the resolution.  Over quotient rings, it's preferred."),
	  SPAN (TT "Strategy => 3", " -- Same as algorithm 2, but compute those Hilbert functions 
	       which allow removal of S-pairs (a la Robbiano, et al.). Sometimes this
	       improvement can be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since 
     this makes an enormous improvement to the efficiency of the algorithm."
     }

document {
     Key => SortStrategy,
     Headline => "specify a strategy for sorting S-pairs",
     TT "SortStrategy", " -- a keyword for an optional argument which 
     specifies the strategy to be used for sorting S-pairs."
     }

document {
     Key => [resolution,SortStrategy],
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which 
     specifies the strategy to be used for sorting S-pairs.",
     PARA{},
     "Not implemented yet."
     }

TEST "
R = ZZ/101[x,y]
M = cokernel matrix {{x^2+y^4, y^2 + x*y^3 + 11, 1 + x*y^2}}
C = res M
assert (HH_-1 C == 0)
assert (HH_0 C == M)
assert (HH_1 C == 0)
assert (HH_2 C == 0)
assert (HH_3 C == 0)
assert (HH_4 C == 0)
"

document {   -- This node is used as an example for the documentation node: Key, Usage
     Key => (resolution,Module),
     Headline => "compute a projective resolution of a module",
     Usage => "resolution M\nres M",
     Inputs => { "M" },
     Outputs => { {"a free resolution of ", TT "M"} },
     "Warning: the resolution can have free modules with unexpected ranks
     when the module ", TT "M", " is not homogeneous.  Here is an example
     where even the lengths of the resolutions differ.  We compute
     a resolution of the kernel of a ring map in two ways.
     The ring ", TT "R", " is constructed naively, but the ring
     ", TT "S", " is constructed with variables of the right degrees
     so the ring map ", TT "g", " will turn out to be homogeneous.",
     EXAMPLE {
	  "k = ZZ/101; T = k[v..z];",
	  "m = matrix {{x,y,z,x^2*v,x*y*v,y^2*v,z*v,x*w,y^3*w,z*w}}",
	  "n = rank source m",
	  "R = k[u_1 .. u_n]",
	  "S = k[u_1 .. u_n,Degrees => degrees source m]",
	  "f = map(T,R,m)",
	  "g = map(T,S,m)",
	  "res ker f",
	  "res ker g",
	  "isHomogeneous f",
	  "isHomogeneous g"
	  },
     EXAMPLE {
	  "R = ZZ/32003[a..d]/(a^2+b^2+c^2+d^2);",
	  "M = coker vars R",
	  "C = resolution(M, LengthLimit=>6)"
	  },
     "For an overview of resolutions, in order of increasing detail, see",
     UL {
	  SPAN (TO "Hilbert functions and free resolutions"),
	  TO "free resolutions of modules",
	  SPAN (TO "computing resolutions", " -- most detailed")
	  },
     "Some useful related functions",
     UL {
	  TO (betti,ChainComplex),
	  TO (status,Resolution),
	  }
     }

document {
     Key => (resolution, Matrix),
     Headline => "given a module map represented by a matrix, produce a comparison map between resolutions of its source and target",
     Usage => "resolution f",
     Inputs => { "f" => {"a module homomorphism ", TT "N <--- M"} },
     Outputs => { {"a chain map from a projective resolution of the source of ", TT "f", " to a resolution of the target of ", TT "f" } },
     EXAMPLE {
	  "R = ZZ[x,y,z]",
	  "N = R^1/(x,y,z)",
	  "M = R^1/(x^2,y^2,x*y*z,z^2)",
	  "f = map(N,M,1)",
	  "res f"
	  },
     SeeAlso => { "free resolutions of modules" }
     }

document { -- This node is used as an example in the documentation nodes: Inputs, Outputs
     Key => {(resolution, Ideal),(resolution, MonomialIdeal)},
     Headline => "compute a projective resolution of (the quotient ring corresponding to) an ideal",
     Usage => "resolution I",
     Inputs => {
	  "I" => { "an ideal in a ring ", TT "R", ", say" }
	  },
     Outputs => {
	  {"a resolution of ", TT "R/I", " by projective ", TT "R", "-modules"}
	  },
     EXAMPLE {
	  "R = ZZ[a..d]",
	  "I = ideal(a,b,c,d)",
	  "C = res I",
	  "C_2",
	  "C.dd_2"
	  },
     SeeAlso => { (symbol _, ChainComplex, ZZ), dd, res, ideal }
     }

TEST ///
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (3,toList (t_1 .. t_9))			  -- 3 by 3
n = matrix pack (3,toList (u_1 .. u_9))			  -- 3 by 3

-- Try the following with various resolution algorithms

alg = 0
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1, Strategy => alg)
assert( rank C_2 == 2 )
C = res(M, LengthLimit => 4, DegreeLimit => 2, Strategy => alg)
assert( rank C_2 == 33 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4, Strategy => alg)
assert( rank C_5 == 0 )

alg = 1
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1, Strategy => alg)
assert( rank C_2 == 2 )
C = res(M, LengthLimit => 4, DegreeLimit => 2, Strategy => alg)
assert( rank C_2 == 33 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4, Strategy => alg)
assert( rank C_5 == 0 )

alg = 2
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1+2, Strategy => alg)
assert( rank C_2 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 2+2, Strategy => alg)
assert( rank C_2 == 34 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3+2, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4+2, Strategy => alg)
assert( rank C_5 == 0 )

-- This one fails
alg = 3
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1+2, Strategy => alg)
assert( rank C_2 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 2+2, Strategy => alg)
assert( rank C_2 == 34 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3+2, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4+2, Strategy => alg)
assert( rank C_5 == 0 )
///

document {
     Key => {status,(status, GroebnerBasis),(status, Resolution),(status, ChainComplex),
	  [status, TotalPairs],[status, Monomials],[status, PairsRemaining]},
     Headline => "status of a resolution computation",
     TT "status C", " -- displays the status of the computation of a
     chain complex ", TT "C", " constructed by ", TO "resolution", ".  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA{},
     "Options:",
     UL {
	  {TO TotalPairs, " -- display the total number of S-pairs, default value ",
	       toString (options status).TotalPairs },
	  {TO PairsRemaining, " -- display the number of S-pairs remaining, default value ",
	       toString (options status).PairsRemaining},
	  {TO Monomials, " -- display the number of monomials, default value ",
	       toString (options status).Monomials}
	  },
     EXAMPLE lines ///
          R = QQ[a..d]
	  D = res coker random(R^2,R^{4:-2})
	  status(D, TotalPairs => true, PairsRemaining => true, Monomials => true)
     ///
     }

document {
     Key => TotalPairs,
     Headline => "whether to display the total number of S-pairs",
     TT "TotalPairs", " -- an option for ", TO "status", " which specifies
     whether to display the total number of S-pairs."
     }

document {
     Key => PairsRemaining,
     Headline => "whether to display the number of S-pairs remaining",
     TT "PairsRemaining", " -- an option for ", TO "status", " which specifies
     whether to display number of S-pairs remaining."
     }

document {
     Key => Monomials,
     Headline => "whether to display the number of monomial",
     TT "Monomials", " -- an option for ", TO "status", " which specifies
     whether to display the number of monomials."
     }

document {
     Key => factor,
     Headline => "factor a ring element or a ZZ-module"
     }

document {
     Key => (factor,Module),
     Headline => "factor a ZZ-module",
     Usage => "factor M",
     Inputs => { "M" },
     Outputs => {{ "a symbolic expression describing the decomposition of ", TT "M", " into a direct sum of principal modules"}},
     "The ring of ", TT "M", " must be ", TO "ZZ", ".",
     PARA {},
     "In the following example we construct a module with a known (but disguised) factorization.",
     EXAMPLE lines ///
     	  f = random(ZZ^6, ZZ^4)
          M = subquotient ( f * diagonalMatrix{2,3,8,21}, f * diagonalMatrix{2*11,3*5*13,0,21*5} )
	  factor M
     ///}

document {
     Key => {(factor,RingElement),(factor,QQ),(factor,ZZ)},
     Headline => "factor a ring element",
     TT "factor x", " -- factors x.",
     PARA{},
     "The result is a ", TO "Product", " each of whose factors is a 
     ", TO "Power", " whose base is one of the factors found and whose
     exponent is an integer.",
     EXAMPLE {
	  "y = (2^15-4)/(2^15-5)",
      	  "x = factor y",
      	  "value x",
	  },
     "We may ", TO "peek", " inside ", TT "x", " to a high depth to see
     its true structure as ", TO "Expression", ".",
     EXAMPLE "peek'(100,x)",
     PARA{},
     "For small integers factorization is done by trial division.  Eventually
     we will have code for large integers.  For multivariate polynomials the
     factorization is done with code of Michael Messollen (see 
     ", TO "Singular-Libfac", ").  For univariate
     polynomials the factorization is in turn done with code of 
     Gert-Martin Greuel and Ruediger Stobbe (see ", TO "Singular-Factory", ").",
     EXAMPLE {
	  "R = ZZ/101[u]",
      	  "factor (u^3-1)",
	  },
     "The constant term is provided as the last factor, if it's not equal
     to 1.",
     EXAMPLE {
	  "F = frac(ZZ/101[t])",
      	  "factor ((t^3-1)/(t^3+1))",
	  },
     "The code for factoring in a fraction field is easy to read:",
     EXAMPLE "code(factor,F)"
     }

document {
     Key => integrate,
     Headline => "numerical integration",
     TT "integrate(f,a,b)", " -- integrate f from a to b numerically, using
     Gaussian quadrature.",
     EXAMPLE "integrate(sin,0,pi)"
     }

document {
     Key => {getWWW,(getWWW, String),(getWWW, String, Nothing),(getWWW, String, String)},
     Headline => "get a web page",
     TT "getWWW URL", " -- obtain the contents of the web page, together with the http headers, at the address given by ", TT "URL", ", from an http server.",
     BR{},
     TT "getWWW(URL,TEXT)", " -- obtain the contents of the web page addressed
     by ", TT "URL", " from an http server, using the POST method, provided 
     with ", TT "TEXT", ".",
     PARA{},
     "This doesn't work under Solaris because Sun doesn't provide sockets
     or name service to statically linked programs like this one.",
     PARA{},
     "Accessing a secure web site (whose URL begins with ", TT "https:", ")
     depends on your having installed ", TT "openssl", " on your system."
     }

document {
     Key => Descent,
     "A type of mutable hash table used by ", TO "showUserStructure", ", ", TO "showClassStructure", ", 
     and ", TO "showStructure", " to display their tree of results conveniently."
     }

document {
     Key => showUserStructure,
     Headline => "show relationship between types defined by user",
     TT "showUserStructure", " -- a command which displays the structure of 
     types defined by the user and assigned to global variables.",
     PARA{},
     "Each such class is displayed to the right of its parent.",
     PARA{},
     "A type is an instance of the class ", TO "Type", ".",
     EXAMPLE {
	  "X = new Type of List",
	  "Y = new Type of X",
	  "Z = new Type of X",
	  "showUserStructure",
	  },
     SeeAlso => { showStructure, parent, ancestors}
     }


document {
     Key => showStructure,
     Headline => "show relationship between types",
     TT "showStructure", " -- a command which displays the structure of types
     assigned to global variables.",
     BR{},
     TT "showStructure (X,Y,...)", " -- a command which displays the structure 
     of the types specified.",
     PARA{},
     "Each such type is displayed to the right of its ", TO "parent", ".",
     PARA{},
     "A type is an instance of ", TO "Type", ", by definition.",
     EXAMPLE {
	  "showStructure",
	  },
     SeeAlso => { "showClassStructure", "showUserStructure", ancestors }
     }

document {
     Key => showClassStructure,
     Headline => "show relationship between things",
     TT "showClassStructure", " -- a command which displays the structure of things
     assigned to global variables.",
     BR{},
     TT "showClassStructure (x,y,...)", " -- a command which displays the structure 
     of the things specified.",
     PARA{},
     "Each such type is displayed to the right of its ", TO "class", ".",
     EXAMPLE {
	  "showClassStructure",
	  },
     SeeAlso => { "showStructure", "showUserStructure" }
     }

document {
     Key => Variety,
     Headline => "the class of all algebraic varieties", 
     SeeAlso => "varieties"
     }

document {
     Key => AffineVariety,
     Headline => "the class of all affine varieties",
     "To create an affine variety, use ", TO "Spec", ".",
     EXAMPLE { "Spec(QQ[x,y])" }
     }

document {
     Key => ProjectiveVariety,
     Headline => "the class of all projective varieties",
     "To create a projective variety, use ", TO "Proj", ".",
     EXAMPLE { "Proj(QQ[x,y])" }
     }

document {
     Key => {(Spec, Ring),Spec},
     Headline => "make an affine variety",
     TT "Spec R", " -- create an affine variety or scheme from the ring ", TT "R", ".",
     EXAMPLE { "R = QQ[x,y];", "Spec R" }
     }

document {
     Key => {(Proj, Ring), Proj},
     Headline => "make a projective variety",
     TT "Proj R", " -- create a projective variety or scheme from the graded ring ", TT "R", ".",
     EXAMPLE { "R = QQ[x,y];", "Proj R" }
     }

document {
     Key => CoherentSheaf,
     Headline => "the class of all coherent sheaves"
     }

document {
     Key => sheaf,
     Headline => "make a coherent sheaf"
     }

document {
     Key => (sheaf, Variety, Module),
     Headline => "make a coherent sheaf",
     TT "sheaf(X,M)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the module ", TT "M", ".",
     PARA{},
     "If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is 
     the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module."
     }

document {
     Key => (sheaf, Variety, Ring),
     Headline => "make a coherent sheaf of rings",
     TT "sheaf(X,R)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the ring ", TT "R", ".  The variety ", TT "X", " must be ", TT "Spec R", " or ", TT "Proj R", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z]
	  X = Proj R
	  Y = Spec R
	  sheaf(X,R)
	  sheaf(Y,R)
     ///}

document {
     Key => (sheaf, Variety),
     Headline => "make a coherent sheaf",
     TT "sheaf(X)", " -- produce the structure sheaf of rings on the variety ", TT "X", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z]
	  X = Proj R
	  Y = Spec R
	  sheaf X
	  sheaf Y
     ///}

document {
     Key => (sheaf, Module),
     Headline => "make a coherent sheaf",
     TT "sheaf M", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", "."
     }

document {
     Key => (symbol ~, Module),
     Headline => "make a coherent sheaf",
     TT "M~", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", ".",
     PARA{},
     SeeAlso => "CoherentSheaf"
     }

document {
     Key => (sheaf, Ring), 
     Headline => "make the structure sheaf",
     TT "sheaf R", " -- produce the structure sheaf on the projective variety ", TT "Proj R", "."
     }

document {
     Key => (symbol ~, Ring),
     Headline => "make the structure sheaf",
     TT "R~", " -- produce the structure sheaf on the projective variety ", TT "Proj R", ".",
     EXAMPLE { "R = QQ[x,y,z];", "R~", "variety oo" }
     }

document {
     Key => (module, CoherentSheaf),
     Headline => "get the module defining a coherent sheaf",
     TT "module F", " -- produce the module from which the coherent sheaf ", TT "F", " was defined.",
     PARA{},
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X(3)",
	  "module F",
	  "degrees oo",
	  }
     }

document {
     Key => (symbol ++, CoherentSheaf, CoherentSheaf),
     Headline => "direct sum of coherent sheaves",
     TT "F ++ G", " -- direct sum of coherent sheaves."
     }

document {
     Key => (symbol **, CoherentSheaf, CoherentSheaf),
     Headline => "tensor product of coherent sheaves",
     TT "F ** G", " -- tensor product of coherent sheaves."
     }

document {
     Key => {(symbol SPACE, CoherentSheaf, ZZ),
	  (symbol SPACE, SheafOfRings, ZZ)},
     Headline => "canonical twist of a coherent sheaf",
     Usage => "F(n)",
     Inputs => {"F" => {"or a ", ofClass SheafOfRings, ", on a projective variety"}, "n"},
     Outputs => {
	  CoherentSheaf => "the twist of F on a projective variety by
         the n-th power of the hyperplane line bundle.",
	 },
     PARA{},
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X",
	  "G = F(3)",
	  "module G",
	  "degrees oo",
	  }
     }

document { 
     Key => {(symbol /, CoherentSheaf, CoherentSheaf),
	  (symbol /, CoherentSheaf, Ideal)
	  },
     Headline => "quotient of coherent sheaves",
     Usage => "F / G",
     Inputs => {
	  "F",
	  "G" => {"or ", ofClass Ideal}
	  },
     Outputs => {
	  CoherentSheaf => {"the quotient sheaf ", TT "F/G"}
	  },
     "We compute the cohomology of two sheaves supported on an elliptic curve.",
     EXAMPLE lines ///
     	  X = Proj(QQ[x,y,z])
	  I = ideal(y^2*z-x*(x-z)*(x-11*z))
	  N = (sheaf module I)/(sheaf module I^2)
	  G = OO_X^1/I
	  HH^1(G)
	  HH^1(N)
	  ///,
     SeeAlso => {Proj, Spec, sheaf, (cohomology,ZZ,CoherentSheaf), OO}
     }

document {
     Key => (exteriorPower, ZZ, CoherentSheaf),
     TT "exteriorPower(i,F)", " -- calculate the ", TT "i", "-th exterior power of a coherent sheaf
     ", TT "F", ".",
     }

document {
     Key => {(symbol >=, ZZ),(symbol >=,InfiniteNumber)},
     Usage => "(>= d)",
     Inputs => { "d" },
     Outputs => { { "a special object of class ", TT "LowerBound", " used to represent the set of natural numbers at least as large as ", TT "d" } }
     }

document {
     Key => {(symbol >, ZZ),(symbol >,InfiniteNumber)},
     Usage => "(> d)",
     Inputs => { "d" },
     Outputs => { { "a special object of class ", TT "LowerBound", " used to represent the set of natural numbers larger than ", TT "d" } }
     }

document {
     Key => {(OO,Variety), OO},
     Headline => "the structure sheaf",
     Usage => "OO_X",
     Inputs => { "X" => "a variety" },
     Outputs => { { "the structure sheaf of ", TT "X", "." } },
     EXAMPLE lines ///
         R = QQ[x,y,z]/(y^2*z-x*(x-z)*(x-37*z));
	 X = Proj R
	 OO_X
	 HH^1(OO_X)
	 HH^0(OO_X(3))
     ///,
     SeeAlso => {CoherentSheaf, cohomology}
     }

document {
     Key => {(instances, Type),instances},
     Usage => "instances X",
     Inputs => { "X" },
     Outputs => {{"a hashtable listing global symbols whose values are instances of type ", TT "X"}},
     EXAMPLE lines ///
     	  20!
	  instances ZZ
	  ///
     }

document { Key => Core,
     Headline => "the core part of Macaulay 2",
     PARA {
     	  "This package contains the core functionality of Macaulay 2, without the documentation, 
     	  which is in the package ", TO "Macaulay2", ".  It doesn't get installed in the usual way,
	  and we hope to rename to ", TO "Macaulay2", " in the future."
	  }
     }

document { Key => toRR,
     Headline => "convert to high-precision real number",
     Usage => "toRR(prec,x)",
     Inputs => { 
	  "prec" => ZZ => {"the number of bits of precision desired"},
	  "x" => {ofClass{RR,ZZ,QQ}}
	  },
     Outputs => {RR => {"the result of converting ", TT "x", " to a high-precision real number"}},
     EXAMPLE lines ///
	  toRR(200,1/7)
	  precision oo
     ///}

document {
     Key => {toCC,
 	  (toCC, ZZ, ZZ), (toCC, ZZ, QQ), (toCC, ZZ, RR), (toCC, ZZ, CC),
 	  (toCC, RR, RR), (toCC, ZZ, ZZ, ZZ), (toCC, ZZ, ZZ, QQ), 
	  (toCC, ZZ, QQ, ZZ), (toCC, ZZ), (toCC, ZZ, QQ, QQ), (toCC, QQ), 
	  (toCC, ZZ, RR, ZZ), (toCC, ZZ, ZZ, RR), (toCC, ZZ, RR, QQ), 
	  (toCC, ZZ, QQ, RR), (toCC, RR), (toCC, CC), (toCC, ZZ, RR, RR)
	  },
     Headline => "convert to high-precision complex number",
     SYNOPSIS (
	  Usage => "toCC(prec,x,y)\ntoCC(prec,x)",
	  Inputs => { 
	       "prec" => ZZ => {"the number of bits of precision desired"},
	       "x" => {ofClass{ZZ,QQ,RR}},
	       "y" => {ofClass{ZZ,QQ,RR}}
	       },
	  Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		    ", TT "y", " is omitted, the imaginary part is zero."}},
	  EXAMPLE lines ///
	  toCC(200,7)
	  toCC(100,7,3.)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "toCC(x,y)\ntoCC x",
	  Inputs => { 
	       "x" => RR,
	       "y" => RR
	       },
	  Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		    ", TT "y", " is omitted, the imaginary part is zero.  The precision of the result is
		    the minimum precision of the arguments."}},
	  EXAMPLE lines ///
	  toCC(3.,4.)
	  toCC(3.p100,4.p200)
	  ///
	  )
     }

document { Key => InexactNumber,
     PARA {
	  "This type of number is intended to serve as a parent class for those types of numbers
	  that are inexactly represented in the computer."
	  }
     }

document { 
     Key => { Constant,
	  (symbol /,Constant,Constant),
	  (symbol /,Constant,InexactNumber),
	  (symbol /,Constant,Number),
	  (symbol /,InexactNumber,Constant),
	  (symbol /,Number,Constant),
	  (symbol ==,Constant,Constant),
	  (symbol ==,Constant,InexactNumber),
	  (symbol ==,InexactNumber,Constant),
	  (symbol ^,Constant,Constant),
	  (symbol ^,Constant,InexactNumber),
	  (symbol ^,Constant,Number),
	  (symbol ^,InexactNumber,Constant),
	  (symbol ^,Number,Constant)
	  },
     PARA {
	  "A constant is a symbolic entity that can be approximated by a real or complex
	  number to any desired accuracy.  It is converted to a numeric value of the 
	  correct precision, when necessary."
	  },
     EXAMPLE lines ///
     	  pi
     	  +pi
     	  numeric_100 pi
	  2. * pi
	  2p100 * pi
	  exp(2*pi*ii/17)
     ///,
     SeeAlso => { numeric, "defaultPrecision" }
     }

document { Key => InexactField,
     PARA {
	  "An inexact field is one whose elements are real or complex numbers."
	  },
     EXAMPLE lines ///
     numeric_100 pi
     ring oo
     class oo
     parent oo
     ///
     }

document { Key => InexactFieldFamily,
     PARA {
	  "This type is intended to serve as a parent class for those types of rings of numbers
	  that have settable precision."
	  }
     }

document { Key => RealField,
     PARA {
	  "A real number ring is a ring whose elements are real numbers of variable precision."
	  }
     }

document { Key => ComplexField,
     PARA {
	  "A complex number ring is a ring whose elements are complex numbers of variable precision."
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
