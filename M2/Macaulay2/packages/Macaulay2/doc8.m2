--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => {(symbol /, Module, Module),
	  (symbol /, Module, Ideal),
	  (symbol /, Module, List),
	  (symbol /, Module, Sequence),
	  (symbol /, Module, Vector),
	  (symbol /, Module, RingElement)},
     Headline => "make a quotient module",
     Usage => "M/N",
     Inputs => {
	  "M",
	  "N" => Nothing => {
	       ofClass Module, ", ",
	       ofClass Ideal, ", ",
	       ofClass List, ", ",
	       ofClass Sequence, ", ",
	       ofClass RingElement, ", or ",
	       ofClass Vector}
	  },
     Outputs => {
	  Module => "The quotient module M/N of M"
	  },
     "If N is an ideal, ring element, or list or sequence of 
     ring elements (in the ring of M), 
     then the quotient is
     by the submodule N*M of M.",
     PARA{},
     "If N is a submodule of M, or a list or sequence of submodules, or a vector, then the quotient
     is by these elements or submodules.",
     EXAMPLE lines ///
     	  R = ZZ/173[a..d]
	  M = ker matrix{{a^3-a*c*d,a*b*c-b^3,a*b*d-b*c^2}}
	  M/a == M/(a*M)
	  M/M_0
	  M/(R*M_0 + b*M)
	  M/(M_0,a*M_1+M_2)
     	  presentation oo
	  ///,
      SeeAlso => {"subquotient modules", presentation}
     }

document {
     Key => (symbol /, Ideal, Ideal),
     Headline => "make a quotient module",
     Usage => "I/J",
     Inputs => { "I", "J" => {"in the same ring as ", TT "I"}},
     Outputs => {
	  Module => {"The quotient module ", TT "(I+J)/J"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a,b,c]
	  I = ideal vars R
	  M = I / I^2
	  ///,
     "There is a diffference between typing I/J and (I+J)/J
     in Macaulay2, although conceptually they are the same module.
     The former has as its generating set the generators of I,
     while the latter has as its (redundant) generators 
     the generators of I and J.  Generally, the former method is preferable.",
     EXAMPLE lines ///     
	  gens M
	  N = (I + I^2)/I^2
	  gens N
     ///,
     SeeAlso => {"subquotient modules", generators}
     }


document {
     Key => {(symbol ^,Module,Array),
       (symbol ^,ChainComplex,Array)},
     Headline => "projection onto some factors of a direct sum module or chain complex",
     Usage => "M^[i,j,...,k]",
     Inputs => {"M" => {"or ", ofClass ChainComplex},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass Matrix, ", or ", ofClass ChainComplexMap}
	  },
     PARA{},
     "The module ", TT "M", " should be a direct sum, and the result is the map
     obtained by projection onto the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.",
     PARA{},
     EXAMPLE lines ///
	  M = ZZ^2 ++ ZZ^3
      	  M^[0]
      	  M^[1]
      	  M^[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  M = (a => image vars R) ++ (b => coker vars R)
	  M^[a]
	  isWellDefined oo
	  M^[b]
	  isWellDefined oo
	  isWellDefined(M^{2})
	  ///,
     PARA{},
     "This works the same way for chain complexes.",
     EXAMPLE lines ///
	  C = res coker vars R
	  D = (a=>C) ++ (b=>C)
	  D^[a]
	  ///,
     SeeAlso => {directSum, (symbol ^,Matrix,Array), (symbol _,Module,Array),(symbol ^,Module,List)}
     }

document {
     Key => (symbol _,Module,Array),
     Headline => "get inclusion map into direct sum",
     TT "M_[i,j,k]", " -- get inclusion map of blocks from a module ", TT "M", ".",
     PARA{},
     "The module ", TT "M", " should be a direct sum, and the result is the matrix
     obtained by inclusion from the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA{},
     EXAMPLE {
	  "M = ZZ^2 ++ ZZ^3",
      	  "M_[0]",
      	  "M_[1]",
      	  "M_[1,0]",
	  },
     SeeAlso => {submatrix, (symbol _,Matrix,Array), (symbol ^,Module,Array),(symbol _,Module,List)}
     }
document {
     Key => (symbol ^, Module, List),
     Headline => "projection map from a free module",
     TT "M^{i,j,k,...}", " -- provides the projection map from a free module
     ", TT "M", " to the free module corresponding to the basis vectors whose
     index numbers are listed.",
     PARA{},
     EXAMPLE "(ZZ^5)^{2,3}",
     SeeAlso => {"_", Module, List}
     }

document {
     Key => (symbol _, Module, List),
     Headline => "map from free module to some generators",
     TT "M_{i,j,k,...}", " -- provides a map from a free module to the module
     ", TT "M", " which sends the basis vectors to the generators of ", TT "M", "
     whose index numbers are listed.",
     PARA{},
     EXAMPLE "(ZZ^5)^{2,3}",
     SeeAlso => {"^", Module, List}
     }

document {
     Key => (symbol _, Ideal, List),
     Headline => "map from free module to some generators",
     Usage => "I_{i,j,k,...}",
     Inputs => {
	  "I",
	  { TT "{i,j,k,...}", ", a list of integers" }
	  },
     Outputs => {
	  "f" => { "a map from a free module to the module ", TT "module I", "
	       which sends the basis vectors to the generators of ", TT "I", "
     	       whose index numbers are listed."
	       }
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal vars R",
	  "f = I_{0,2}",
	  "image f"
	  },
     SeeAlso => { (module, Ideal) }
     }

document {
     Key => (basis,List,Module),
     Headline => "basis of the part of a module of a certain degree",
     Usage => "basis(i,M)",
     Inputs => {
	  "i" => "a list of integers to serve as a degree or multidegree",
	  "M" => "a module"
	  },
     Outputs => {
	  {
	       "a map from a free module to ", TT "M", " which sends the
	       basis elements to a basis, over the ground field, of the
	       degree ", TT "i", " part of ", TT "M"
	       }
	  },
     "The degree ", TT "i", " is a multi-degree, represented as a list of 
     integers.  If the number of degrees is 1, then ", TT "i", " may
     be provided as an integer.",
     SeeAlso => {
	  (basis,Module),
	  "bases of parts of modules"
	  }
     }

document {
     Key => (basis,Module),
     Headline => "basis of a module",
     Usage => "basis M",
     Inputs => {
	  "M" => "a module"
	  },
     Outputs => {
	  {
	       "a map from a free module to ", TT "M", " which sends the
	       basis elements to a basis, over the ground field, of ", TT "M", "."
	       }
	  },
     SeeAlso => {
	  (basis,List,Module),
	  "bases of parts of modules"
	  }
     }

document {
     Key => (basis,Ring),
     Headline => "basis of a ring",
     Usage => "basis R",
     Inputs => {
	  "R" => "a ring"
	  },
     Outputs => {
	  {
	       "a map from a free module to ", TT "R", " which sends the
	       basis elements to a basis, over the ground field, of ", TT "R"
	       }
	  },
     EXAMPLE {
	  "R = QQ[x,y]/(x^3,y^2);",
	  "basis R"
	  }
     }


-- a) free modules, bases and monomials
-- b) monomial orders: def
-- c) kinds in M2
-- d) Schreyer orders
--    examples of use

document { 
     Key => "Monomial orders in free modules",
     "In Macaulay 2, each free module F = R^s over a ring R has a basis
     of unit column vectors F_0, F_1, ..., F_(s-1).  The monomials of F
     are the elements m*F_i, where m is a monomial of the ring R.
     In Macaulay 2, orders on the monomials of F are used for computing Groebner bases and
     syzygies, and also to determine the initial, or lead term of elements of F.",
     PARA{},
     "The ring R comes equipped with a total order on the monomials of R.
     A total order on the monomials of F is called ", EM "compatible", " (with the order
     on R), if m*F_i > n*F_i (in F) whenever m > n (in R). There are many types of
     compatible orders, but several stand out: term over position up, term over position down,
     position up over term, position down over term, and Schreyer orders.",
     PARA{},
     "term over position up:   m*F_i > n*F_j iff m>n or m==n and i>j",
     PARA{},
     "term over position down: m*F_i > n*F_j iff m>n or m==n and i<j",
     PARA{},
     "position up over term:   m*F_i > n*F_j iff i>j or i==j and m>n",
     PARA{},
     "position down over term: m*F_i > n*F_j iff i>j or i==j and m>n",
     PARA{},
     "Induced monomial orders are another class of important orders on F.",
     "Given a free module G and a set of monomials phi_0, ..., phi_(s-1) of G,
     and a monomial order on the monomials of G, the induced order, or, Schreyer
     order is defined by:
     m*F_i > n*F_j (in F) iff m*phi_i > n*phi_j (in G), or m*phi_i and n*phi_j
     are scalar multiples of each other, and i>j.
     ",
     PARA{},
     "In Macaulay 2, free modules come equipped with a compatible order.  The default
     order is: m*F_i > n*F_j if m > n (in R), or m == n, and i > j. 
     This is called Position=>Up.  In the following example, the lead term is a*F_1,
     since a > b.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "F = R^3",
	  "f = b*F_0 + a*F_1",
	  "leadTerm f"
	  },
     "This is the same as giving the monomial order as:",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Up}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)"
	  },
     "Giving Position=>Down instead switches the test above to i < j.  In this case the 
     monomial order on F is:
     m*F_i > n*F_j if m>n or m==n and i<j.",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Down}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)"
	  },
     "If one gives Position=>Up or Position=>Down earlier, then the position will be 
     taken into account earlier. For example",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 2, Position => Down, GRevLex => 2}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)",
	  "leadTerm(b*F_0 + c^4*F_1)",	  
	  "leadTerm(c*F_0 + d^2*F_1)"	  
	  },
     "If one wants Position over Term (POT), place the Position element first",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {Position => Down}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)",
	  "leadTerm(b*F_0 + c^4*F_1)",	  
	  "leadTerm(c*F_0 + d^2*F_1)"	  
	  },
     SeeAlso => {
	  "Schreyer order"
	  }
     }
     
document {
     Key => "Schreyer order",
     Headline => "induced monomial order on a free module",
     "The Schreyer order is a monomial order on a free module which is particularly
     efficient for computing Groebner bases and syzygies.  The size of Groebner bases
     of submodules using such orders is often much much smaller than if a position over term
     or term over position order would be used.  We call these Schreyer orders, after
     Frank Olaf-Schreyer, who used them to give an algorithm for syzygies, and who also 
     recognized many of their beneficial properties.  See Schreyer.... for more 
     information.",
     PARA{},
     "Let F be a free module, with basis e_0, e_1, ..., e_r (unit column vectors),
     and suppose that a e_i, and b e_j are monomials in F (that is, a and b are monomials in
     the base ring, and e_i and e_j are elements of the basis of F).  Suppose also that 
     m0, m1, ..., mr are monomials of a free module G (which is itself endowed with a monomial
     order).",
     PARA{},
     "Then, a e_i > b e_j if a mi > b mj, in the order on G, or, if a mi and b mj are 
     scalar multiples of each other, if i > j.",
     PARA{},
     "In Macaulay 2, free modules with a Schreyer order on them can be created using ", 
     TO (schreyerOrder,Matrix), ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "m = matrix{{a,b,c,d}};",
	  "m1 = schreyerOrder m",
	  "F = source m1",
	  "g = syz m1",
	  "leadTerm g"
	  },
     "In Macaulay 2, free modules are displayed without any indication of whether they are
     endowed with a Schreyer order or not.  To determine whether one is, use ", 
     TO (schreyerOrder,Module), ".  If the result is the zero matrix, then the monomial order
     associated with this free module is not a Schreyer order.  Instead it is the 
     order determined directly from the ring.",
     EXAMPLE {
	  "schreyerOrder target m",
	  "schreyerOrder source g"
	  },
     "Over quotient rings, the multiplication a mi and b mj are over the ambient polynomial
     ring, not the quotient.",
     PARA{},
     "It is fine for the free module G above to be endowed with a Schreyer order too.",
     PARA{},
     "The only places that Schreyer orders are considered is in computation of Groebner bases,
     syzygies, and free resolutions, and with the ", TO leadTerm, " routine.",
     SeeAlso => {
	  leadTerm,
	  (schreyerOrder,Matrix),
	  (schreyerOrder,Module),
	  gb,
	  syz,
	  resolution
	  }
     }

document {
     Key => (schreyerOrder,Matrix),
     Headline => "create a matrix with the same entries whose source free module has a Schreyer monomial order",
     Usage => "schreyerOrder m",
     Inputs => {
	  "m" => "G <-- F between free modules"
	  },
     Outputs => {
	  {
	       "the same matrix as ", TT "m", ", except that its source free module
	       is endowed with a Schreyer, or induced, monomial order"
	       }
	  },
     "Given a matrix m : F --> G, the Schreyer order on the monomials
     of F is given by: If a e_i and b e_j are monomials of F, i.e. a and b are 
     monomials in the ring, and
     e_i and e_j are unit column vectors of F, then a e_i > b e_j iff
     either leadterm(m)(a e_i) > leadterm(m)(b e_j) or they are k-multiples of
     the same monomial in G, and i > j.",
     PARA{},
     "If the base ring is a quotient ring, we think of leadterm(m) as a matrix
     over the ambient polynomial ring",
     "Given a matrix m with define a monomial order ",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "m = matrix{{a,b,c,d}};",
	  "f = schreyerOrder m",
	  "g = syz f",
	  "leadTerm g"
	  },
     SeeAlso => {
	  "Schreyer monomial orders",
	  leadTerm,
	  (schreyerOrder,Module)
	  }
     }

     
-----------------------------------------------------------------------------

TEST "
R=ZZ/101[a..f]
assert( degrees( R^{1} ++ R^{2} ) == {{-1}, {-2}} )
assert( degrees (R^{1,2} ** R^{3,5}) == {{-4}, {-6}, {-5}, {-7}} )
assert( numgens R^6 == 6 )
assert( rank R^6 == 6 )
f = vars R
M = cokernel (transpose f * f)
assert ( rank M == 5 )
assert ( rank kernel f == 5 )
assert ( rank cokernel f == 0 )
assert(R^{0,0} == R^2)
assert(R^{0,0} != R^{0,1})
"
document {
     Key => GroebnerBasis,
     Headline => "the class of all Groebner bases",
     "A Groebner basis in Macaulay 2 consists of a Groebner basis
     computation, and several associated matrices. Normally you don't
     need to refer to these objects directly, as many operations on
     matrices and modules create them, and refer to them.  For more
     information, see ", TO "Groebner bases", "."
     }

document {
     Key => returnCode,
     TT "returnCode", " --  a key for a ", TO "GroebnerBasis", " under which is
     stored the return code from the engine for the computation."
     }

document {
     Key => symbol gbTrace,
     Headline => "provide tracing output during various computations in the 	 engine.",
     TT "gbTrace = n", " -- set the tracing level for the ", TO "engine", " to
     level ", TT "n", ".  Meaningful values for the user ", TT "n", " are
     0, 1, 2, and 3.",
     PARA{},
     "The notations used in tracing are :",
     UL {
	  "g       - a generator reduced to something nonzero and has been added to the basis.",
	  "m       - an S-pair reduced to something nonzero and has been added to the basis.",
	  "z       - an S-pair reduced to zero, and a syzygy has been recorded.",
	  "u       - an S-pair reduced to zero, but the syzygy need not be recorded.",
	  "o       - an S-pair or generator reduced to zero, but no new syzygy occurred.",
	  "r       - an S-pair has been removed.",
	  "{2}     - beginning to reduce the S-pairs of degree 2.",
	  "(7)     - 7 more S-pairs need to be reduced.",
	  "(8,9)   - there are 8 S-pairs to do in this degree, and 9 more in higher degrees.",
	  ".       - a minor has been computed, or something has happened while computing a resolution.",
	  }
     }
     

document {
     Key => StopBeforeComputation,
     Headline => "initialize but do not begin the computation",
     TT "StopBeforeComputation", " -- an option used by certain functions to cause
     the computation to be initialized but not begun."
     }


document {
     Key => DegreeLimit,
     Headline => "compute up to a certain degree",
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     various functions which specifies that the computation should halt after dealing 
     with degree n."
     }


document {
     Key => BasisElementLimit, 
     Headline => "stop when this number of basis elements is obtained",
     TT "BasisElementLimit", " -- keyword for an optional argument used with
     ", TO "gb", ", ", TO "pushForward", ", ", TO "pushForward1", ", 
     and ", TO "syz", ", which can be used to specify that the computation should
     stop after a certain number of Groebner basis elements have been discovered.",
     SeeAlso => "Groebner bases"
     }

document {
     Key => SyzygyLimit,
     Headline => "stop when this number of syzygies is obtained",
     TT "SyzygyLimit", " -- keyword for an optional argument which specifies
     that the computation should stop after a certain number of syzygies 
     have computed.",
     }


document {
     Key => PairLimit,
     Headline => "stop when this number of pairs is handled",
     TT "PairLimit", " -- keyword for an optional argument used with
     certain functions which specifies that the
     computation should be stopped after a certain number of S-pairs
     have been reduced."
     }


document {
     Key => CodimensionLimit,
     Headline => "stop when this codimension is reached",
     TT "CodimensionLimit => n", " -- keyword for an optional argument used with
     certain functions which specifies that the computation should stop when
     the codimension of the zero set of the ideal (or submodule) generated
     by the leading terms of the Groebner basis elements found so far reaches 
     a certain limit.",
     PARA{},
     "This option has not been implemented yet.",
     PARA{},
     "Eventually the codimension of the ideal of leading terms is the
     codimension of the original ideal.",
     UL {
	  TO [gb,CodimensionLimit],
	  TO [syz,CodimensionLimit],
	  }
     }


document {
     Key => StopWithMinimalGenerators,
     Headline => "stop when minimal generators have been determined",
     TT "StopWithMinimalGenerators", " -- an option used with certain
     functions to specify that the computation should stop as soon as a
     complete list of minimal generators for the submodule or ideal has been
     determined.",
     PARA{},
     UL {
	  TO [gb,StopWithMinimalGenerators],
	  TO [pushForward,StopWithMinimalGenerators],
	  TO [pushForward1,StopWithMinimalGenerators],
	  TO [syz,StopWithMinimalGenerators],
	  }
     }


document {
     Key => Strategy,
     Headline => "specify a computational strategy",
     TT "Strategy => v", " -- an optional argument used with various routines 
     to suggest a strategy for efficient computation."
     }


document {
     Key => Sort,
     TT "Sort", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "Indicates that the Groebner basis should be sorted by lead term; usually
     this is a bad idea.  Normally the basis is sorted only by degree. The
     running time can change either for the good or bad."
     }

document {
     Key => Primary,
     TT "Primary", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "This is a new Groebner basis algorithm, that works in the homogeneous and
     inhomogeneous cases, and is often faster than the default algorithms.  This 
     feature is currently under development."
     }

document {
     Key => Homogeneous,
     TT "Homogeneous", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "This is an alternate Groebner basis algorithm which can be used if the submodule
     is homogeneous, and the ring is a (quotient of) a polynomial ring over a field."
     }

document {
     Key => Inhomogeneous,
     TT "Inhomogeneous", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "This is the default Groebner basis algorithm used if the submodule is
     inhomogeneous, and the ring is a (quotient of) a polynomial ring over a field."
     }

document {
     Key => LongPolynomial,
     TT "LongPolynomial", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "Indicates that during computation of a Groebner basis, the reduction
     routine will be replaced by one which will handle long polynomials more
     efficiently using \"geobuckets\", which accomodate the terms in buckets
     of geometrically increasing length.  This method was first used
     successfully by Thomas Yan, graduate student in CS at Cornell."
     }

document {
     Key => Syzygies, 
     Headline => "whether to collect syzygies",
     TT "Syzygies", " -- keyword for an optional argument used with
     ", TO "gb", " which indicates whether the syzygies should be
     computed.",
     PARA{},
     "It's also an option for ", TO "syz", ", but ", TO "syz", " overrides 
     any value provided by the user with the value ", TT "true", "."
     }

document {
     Key => ChangeMatrix,
     Headline => "whether to produce the change of basis matrix",
     TT "ChangeMatrix", " -- a keyword for optional arguments to certain functions
     which concern a change of basis matrix."
     }

     
document {
     Key => SyzygyRows, 
     Headline => "the number rows of the syzygy matrix to collect",
     TT "SyzygyRows", " -- keyword for an optional argument used with
     ", TO "gb", " and ", TO "syz", ", which specifies how many rows of 
     the syzygy matrix to retain.",
     PARA{},
     "This option is for internal use only."
     }

document {
     Key => getChangeMatrix,
     Headline => "get the change of basis matrix",
     TT "getChangeMatrix G", " -- for a Groebner basis G, return the change of
     basis matrix from the Groebner basis to another generating set, 
     usually a minimal, or original, generating set.",
     PARA{},
     "The option ", TO "ChangeMatrix", " can be used with ", TO "gb", " 
     to enable the computation of the change of basis matrix."
     }

document {
     Key => MinimalMatrix,
     Headline => "set the matrix of minimal generators",
     TT "MinimalMatrix => g", " -- an option for ", TO "forceGB", " which
     specifies that the columns of g are minimal generators for the submodule
     generated by the Groebner basis."
     }

document {
     Key => SyzygyMatrix,
     Headline => "set the syzygy matrix",
     TT "SyzygyMatrix => h", " -- an option for ", TO "forceGB", " which
     specifies that the columns of h are the syzygies for the Groebner basis."
     }
    

TEST "
R = ZZ/103[a..c]
C = resolution cokernel vars R
assert(regularity C === 0)
R = ZZ/101[a .. r]
M = cokernel genericMatrix(R,a,3,6)
time C = resolution M
assert(regularity C === 2)
f = symmetricPower(2,vars R)
assert(f%a + a * (f//a) == f)
"

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (3,toList (t_1 .. t_9))			  -- 3 by 3
n = matrix pack (3,toList (u_1 .. u_9))			  -- 3 by 3
j = flatten (m * n - n * m)
k = flatten (m * n - n * m)
G = gb j
jj = generators G
assert( numgens source jj == 26 )
T = (degreesRing S)_0
assert( poincare cokernel j == 1-8*T^2+2*T^3+31*T^4-32*T^5-25*T^6+58*T^7-32*T^8+4*T^9+T^10 )
v = apply(7, i -> numgens source generators gb(k,DegreeLimit => i) )
assert (v  === {0, 0, 8, 20, 25, 26, 26} )
"

document {
     Key => [syz,StopWithMinimalGenerators],
     Headline => "stop when minimal generators have been determined",
     TT "StopWithMinimalGenerators => true", " -- an optional argument used 
     with ", TO "syz", " that specifies that the computation should stop as soon as a
     complete list of minimal generators for the submodule or ideal has been
     determined.",
     PARA{},
     "The value provided is simply passed on to ", TO "gb", ": see 
     ", TO [gb,StopWithMinimalGenerators], " for details."
     }

document {
     Key => [syz,Strategy],
     Headline => "specify the strategy used to compute the Groebner basis",
     TT "syz(f,Strategy => v)", " -- an option for ", TO "syz", " which can
     be used to specify the strategy to be used in the computation.",
     PARA{},
     "The value of the option is simply passed to ", TO "gb", ", so see the
     documentation there for details."
     }

document {
     Key => [syz,CodimensionLimit],
     Headline => "stop when this codimension is reached",
     TT "CodimensionLimit => n", " -- keyword for an optional argument used with
     ", TO "syz", " which specifies that the computation should stop when
     the codimension of the zero set of the ideal (or submodule) generated
     by the leading terms of the Groebner basis elements found so far reaches 
     a certain limit.",
     PARA{},
     "This option has not been implemented yet.",
     PARA{},
     "Eventually the codimension of the ideal of leading terms is the
     codimension of the original ideal.",
     }

document {
     Key => syz,
     Headline => "the syzygy matrix"
     }

document {
     Key => (syz, GroebnerBasis),
     Headline => "retrieve the syzygy matrix",
     Usage => "syz G",
     Inputs => {
	  "G" => {"the Groebner basis of a matrix ", TT "h"}
	  },
     Outputs => {
	  {"the matrix of syzygies among the columns of ", TT "h"}
	  },
     "Warning: the result may be zero if syzygies were not to be retained 
     during the calculation, or if the computation was not continued to a
     high enough degree.",
     }

document {
     Key => (syz, Matrix),
     Headline => "compute the syzygy matrix",
     Usage => "syz h",
     Inputs => {
	  "h" => {"a matrix"}
	  },
     Outputs => {
	  {"the matrix of minimal generators for the syzygies among the columns of ", TT "h"}
	  }
     }

document {
     Key => [syz,StopBeforeComputation],
     Headline => "whether to stop the computation immediately",
     TT "StopBeforeComputation => true", " -- an optional argument used with ", TO "gb", ".",
     PARA{},
     "Tells whether not to start the computation, with the default value
     being ", TT "false", ".  This can be useful when you want to obtain
     the partially computed Groebner basis contained in an interrupted
     computation."
     }

document {
     Key => [syz,ChangeMatrix],
     Headline => "whether to produce the change of basis matrix",
     TT "ChangeMatrix => true", " -- an optional argument for ", TO "syz", " which
     specifies whether to compute the change of basis matrix."
     }

document {
     Key => modulo,
     Headline => "find the pre-image of a map (low level version)",
     "modulo(f,g) - given homomorphisms ", TT "f", " and ", TT "g", " of free 
     modules with the same target, produces a homomorphism of free modules whose 
     target is the source of ", TT "f", ", and whose image is the pre-image (under
     ", TT "f", ") of the image of ", TT "g", ".",
     PARA{},
     "If ", TT "f", " is null, then it's taken to be the identity.  If ", TT "g", " is null, it's
     taken to be zero."
     }

document { 
     Key => {(symbol //, Matrix, Matrix),
       (symbol //, RingElement, MonomialIdeal),
       (symbol //, RingElement, GroebnerBasis),
       (symbol //, RingElement, RingElement),
       (symbol //, Matrix, MonomialIdeal),
       (symbol //, Matrix, GroebnerBasis),
       (symbol //, Matrix, RingElement),
       (symbol //, RingElement, Matrix),
       (quotient, Matrix, Matrix),
       (quotient, Matrix, GroebnerBasis)},
     Headline => "factor a map through another",
     Usage => "f//g",
     Inputs => {
	  "f" => {"between free modules F --> H, or ",
	     ofClass RingElement},
	  "g" => {"between free module G --> H, ",
	       ofClass RingElement, ", ", 
	       ofClass MonomialIdeal, ", or ",
	       ofClass GroebnerBasis}
	  },
     Outputs => {
	  Matrix => "a matrix h : F --> G"
	  },
     "If f is a matrix, and g is a matrix or Groebner basis, then quotient(f,g) is an alternate 
     notation for f//g.",
     PARA{},
     "If either f or g is a ring element, then it is taken to be the identity matrix on H.  If both are ring elements,
     then the result is also a ring element.  If g is a
     MonomialIdeal, then it is taken to be the matrix of generators of g.  Finally, if g is a GroebnerBasis
     object, then the Groebner basis as so far computed is used.  In these latter two cases, no Groebner bases 
     will be computed.",
     PARA{},
     "The resulting matrix h is such that ", TT "f - g*h", " is the reduction of ", TT "f", " modulo a Groebner basis 
     for the image of ", TT "g", ".",
     PARA{},
     "If the remainder ", TT "f - g*h", " is zero, then the quotient ", TT "f//g", "
     satisfies the equation ", TT "f = g * (f//g)", ".",
     PARA{},
     "One common use is the following.  If an ideal contains 1, then write 1 in terms
     of the generators of the ideal.",
     EXAMPLE lines ///
     	  A = ZZ/101[x,y,z]
	  F = x^4 - y*z*(1-x)^2 - z - y^3
	  I = ideal(F,diff(x,F),diff(y,F),diff(z,F))
	  1 % I
     ///,
     "So we see that 1 is in the ideal.  Now let us find the representation of 1 in terms
     of the four generators of ", TT "I", ".",
     EXAMPLE lines ///
	  g = gens I
	  f = matrix{{1_A}}
	  h = f // g
	  g * (f//g)
     ///,
     "We may also find h directly.",
     EXAMPLE "1 // (gens I)",
     PARA{},
     "One may also use ", TT "//", " to compute the inverse of an invertible matrix.",
     EXAMPLE lines ///
     	  M = matrix{{1,x,y},{x,0,y},{1,2,3}}
	  M = substitute(M, frac A)
	  det M
	  Minv = id_(target M) // M
	  M * Minv
     ///,
     SeeAlso => {(symbol %, Matrix, Matrix), generators, diff, substitute}
     }

TEST "
R = ZZ/101[a..d]
A = image matrix {{a}}
B = image matrix {{b}}
f = inducedMap((A+B)/A, B/intersect(A,B))
assert isIsomorphism f
g = f^-1
assert( f^-1 === g )			  -- check caching of inverses
assert( f*g == 1 )
assert( g*f == 1 )
assert isWellDefined f
assert isWellDefined g
assert not isWellDefined map(R^1,cokernel matrix {{a}})
"

document {
     Key => complement,
     Headline => "find the minimal generators for cokernel of a matrix (low level form)",
     TT "complement f", " -- for a matrix ", TT "f", ", return a map ", TT "g", " with the same
     target whose columns are minimal generators for the cokernel of ", TT "f", ".",
     PARA{},
     "The map ", TT "f", " must be homogeneous."
     }

-----------------------------------------------------------------------------

TEST "
S = ZZ/107[vars ( 0 .. 5 ) ]

g = matrix {{a*b*c - d*e*f, a*d^2 - e^3, a*e^2 - b*c*e}}
k = syz g
assert( numgens source k === 4 )

t = (a + b + c)^4 
u = (a + b + c) * b^3
v = a * t + b * u
w = c * t - d * u
x = b * t + f * u

h = matrix {{t,u,v,w,x}}
h1 = mingens image h

so = m -> m_(sortColumns m)

assert ( so h1 == so matrix {{
	       a^4+4*a^3*b+6*a^2*b^2-3*b^4+4*a^3*c+12*a^2*b*c+12*a*b^2*c+6*a^2*c^2
	       +12*a*b*c^2+6*b^2*c^2+4*a*c^3+4*b*c^3+c^4,
	       a*b^3+b^4+b^3*c
	       }} )
"

document {
     Key => homogenize,
     Headline => "homogenize with respect to a variable",
     TT "homogenize(m,v)", " -- homogenize the ring element, vector,
     matrix, or module ", TT "m", " using the variable ", TT "v", " in the ring of ", TT "m", ".",
     BR{},     
     TT "homogenize(m,v,w)", " -- homogenize ", TT "m", " using the variable ", TT "v", ",
     so that the result is homogeneous with respect to the given list ", TT "w", " of
     integers provided as weights for the variables.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z,Degrees => {1,2,3}]",
      	  "f = 1 + y + z^2",
      	  "homogenize(f,x)",
      	  "homogenize(f,x,{1,0,-1})",
	  },
     PARA{},
     "The weights that may be used are limited (roughly) to the range -2^30 .. 2^30.",
     PARA{},
     Caveat => {
	  "If the homogenization overflows the monomial, this is not
     	  reported as an error."
	  }
     }

TEST "
R = ZZ/101[a..d,t]
f = a^2-d^3*b-1
assert(homogenize(f,t) == a^2*t^2 - d^3*b - t^4)
assert(homogenize(f,t,{1,2,3,4,2}) == a^2*t^6 - d^3*b - t^7)
assert(homogenize(f,b,{1,1,0,-1,1}) == a^2 - d^3*b^5 - b^2)

m = map(R^{1,-1}, , {{a,b},{c,d-1}})
assert(homogenize(m,t) == map(R^{1,-1}, , {{a*t^2, b*t^2}, {c, d-t}}))
assert(homogenize(m,t,{-1,-1,-1,-1,1}) - map(R^{1,-1}, , {{a*t^2, b*t^3}, {c, d*t-1}}) == 0)

v = m_0
F = module v
assert(homogenize(v,t) == a*t^2 * F_0 + c * F_1)
assert(homogenize(v,t,{-1,-1,-1,-1,1}) == a*t^2 * F_0 + c * F_1)

-- now check to make sure that all is ok over quotient rings
R = ZZ/101[a..d]/(a^2-b^2, a*b)
use R
f = c^2 - 1 + b^2 - b
assert(homogenize(f,a) == c^2)
"

document {
     Key => Ascending,
     TT "Ascending", " -- a symbol used as a value for optional
     arguments ", TO "DegreeOrder", " and ", TO "MonomialOrder", "."
     }

document {
     Key => Descending,
     Headline => "specify descending order",
     TT "Descending", " -- a symbol used as a value for optional
     arguments ", TO "DegreeOrder", " and ", TO "MonomialOrder", "."
     }

document {
     Key => DegreeOrder,
     Headline => "sort primarily by degree",
     TT "DegreeOrder", " -- an optional argument for use with certain
     functions, used to specify sort order."
     }

document {
     Key => selectInSubring,
     Headline => "select columns in a subring",
     TT "selectInSubring(i,m)", " -- Form the submatrix of the matrix 'm' consisting of those
     columns which lie in the subring generated by all but the first 'i' parts of the
     monomial order.",
     PARA{},
     "We say that a monomial ordering has n 'parts' if the variables are partitioned
     into n subsets in such a way that for each j, any monomial smaller than a
     monomial that involves variables only from the last j parts, also involves
     variables only from the last j parts.  Such monomial orderings include
     product orderings, lexicographic orderings, and elimination orderings.",
     PARA{},
     "For example, consider the lexicographic ordering of four variables, where
     the parts are the singletons ", TT "{a}, {b}, {c}, {d}", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d,MonomialOrder=>Lex]",
      	  "m = matrix{{b^2-c^2, a^2 - b^2, c*d}}",
      	  "selectInSubring(1,m)",
      	  "selectInSubring(2,m)",
      	  "selectInSubring(3,m)",
	  },
     PARA{},
     Caveat => {
	  "This routine doesn't do what one would expect for graded orders
     	  such as ", TT "GLex", ".  There, the first part of the monomial 
	  order is the degree, which is usually not zero.  This routine 
	  should detect and correct this."
     },
     SeeAlso => "monomial orderings"
     }

document {
     Key => divideByVariable,
     Headline => "divide all columns by a (power of a) variable",
     TT "divideByVariable(m,v)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible.",
     BR{},
     TT "divideByVariable(m,v,d)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible, but divide by no more than v^d.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "m = matrix{{a*b, a^2*c}, {a*b^2, a^4*d}}",
      	  "divideByVariable(m,a)",
      	  "divideByVariable(m,a,1)",
	  },
     Caveat => "You can only divide by a variable, not a monomial,
     and you have little control on what power will be divided.  This routine is mostly
     used by the saturation commands as a fast internal way of dividing.",
     PARA{},
     "We may eliminate this routine."
     }

document {
     Key => newCoordinateSystem,
     Headline => "change variables",
     TT "newCoordinateSystem(S,m)", " -- takes a one-rowed matrix ", TT "m", " of
     independent linear forms over a ring ", TT "R", " and returns a list 
     ", TT "{f,g}", ", where ", TT "f", " is a ring map given by some linear change 
     of coordinates from ", TT "R", " to ", TT "S", " which sends the last variables 
     of ", TT"R", " to the forms in ", TT "m", ", and ", TT "g", " is the inverse 
     of ", TT "f", ".",
     PARA{},
     "The ring ", TT "S", " should have the same number of variables as 
     ", TT "S", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "S = ZZ/101[p..s]",
      	  "newCoordinateSystem(S,matrix{{a+2*b,3*c-d}})"
	  },
     }

document {
     Key => PrimitiveElement,
     Headline => "specify a primitive element",
     TT "PrimitiveElement => g", " -- an option used with ", TO "GF", ".",
     PARA{},
     "The value can be a ring element providing a primitive element, or the
     symbol ", TO "FindOne", " (the default) which specifies that
     ", TO "GF", " should search for a primitive element."
     }

document {
     Key => FindOne,
     Headline => "find a primitive element",
     TT "FindOne", " -- a value for the option ", TO "PrimitiveElement", "
     to ", TO "GF", " which specifies that ", TO "GF", " should search 
     for a primitive element."
     }

document {
     Key => Variable,
     Headline => "specify a name for a variable",
     -- it is also used with integralClosure, but we should automate that
     TT "Variable => x", " -- an option used with ", TO "GF", ", to specify
     a symbol to be used as a name for the generator of the Galois field."
     }

document {
     Key => GaloisField, Headline => "the class of all Galois fields" }
document {
     Key => GF, Headline => "make a finite field" }
document {
     Key => (GF,Ring), Headline => "make a finite field from a ring",
     TT "GF R", " -- make a Galois field from a quotient ring R which happens
     to be isomorphic to a finite field.",
     EXAMPLE {
	  "k = GF(ZZ/2[t]/(t^3+t+1))",
	  "t+t^2"
	  }
     }
document {
     Key => (GF,ZZ,ZZ), Headline => "make a finite field of a given prime power order",
     TT "GF(p,n)", " -- make a Galois field with ", TT "p^n", " elements, where 
     ", TT "p", " is a prime.",
     EXAMPLE {
	  "k = GF(2,3,Variable=>x)",
	  "x+x^2"
	  }
     }
document {
     Key => (GF,ZZ), Headline => "make a finite field of a given order",
     TT "GF(q)", " -- make a Galois field with ", TT "q", " elements, where 
     ", TT "q", " is a power of a prime.",
     EXAMPLE {
	  "k = GF(8)",
	  "x = k_0",
	  "x+x^2"
	  }
     }

document {
     Key => isPrimitive, Headline => "whether an element is a primitive element of a finite field",
     TT "isPrimitive(f)", " -- Given an element ", TT "f", " in a quotient of a polynomial ring ",
     TT "R", " over a finite field ", TT "K", "which is itself a finite field,
      with the ring being finite dimensional over the field,
     determine if ", TT "f", " generates the multiplicative group of this field.",
     EXAMPLE { "R = ZZ/5[t]/(t^2+t+1);", "isPrimitive t", "isPrimitive (t-1)" }
     }

TEST "
R = ZZ/5[t]/(t^2+t+1)
assert (not isPrimitive t)
assert isPrimitive (t-1)
assert (not isPrimitive 0_R)
"

document {
     Key => order,
     Headline => "a key used internally ",
     TT "order", " -- used as a key inside finite fields under which is
     stored the number of elements in the field.  Intended for internal use only",
     PARA{},
     SeeAlso => "GaloisField"
     }
document {
     Key => toField,
     Headline => "declare that a ring is a field",
     Usage => "toField R",
     Inputs => {
	  "R" => "a ring"
	  },
     Consequences => {
	  { "The ring ", TT "R", " is declared to be a field." }
	  },
     "The declaration is accomplished by setting ", TT "R.isField", " to be ", TT "true", ",
     and, in case the ring is a ring handled by the engine, informing the
     engine.  Polynomial rings over rings declared to be fields support
     Groebner basis operations.",
     PARA{},
     "If the engine eventually discovers that some nonzero element of ", TT "R", "
     is not a unit, an error will be signalled.  The user may then use
     ", TO "getNonUnit", " to obtain a non-invertible element of ", TT "R", ".
     If a ring is probably a field, it can be used as a field until a
     contradiction is found, and this may be a good way of discovering
     whether a ring is a field."
     }

document {
     Key => getNonUnit, Headline => "retrieve a previously discovered non-unit",
     Usage => "getNonUnit R",
     Inputs => {
	  "R" => "a ring in which division by a non-unit has been attempted"
	  },
     Outputs => {
	  "the non-unit"
	  },
     "Warning: this function does not work yet for divisions attempted in the course
     of computing a Groebner basis or resolution.",
     SeeAlso => { "toField" }
     }




document {
     Key => modifyRing,
     Headline => "make a copy of a ring, with some features changed",
     TT "modifyRing(R,options)", " -- yields a ring similar to R, with 
     certain features changed.",
     PARA{},
     "Bug: doesn't work yet."
     }

TEST "
    R = ZZ/101[a..d,Degrees=>{1,2,3,4}]
    S = modifyRing(R,Degrees=>{1,1,1,1})
    S1 = modifyRing(R,MonomialOrder=>Eliminate 2,Degrees=>{1,1,1,1})
"

document {
     Key => (symbol **, Ring, Ring), Headline => "tensor product",
     "For complete documentation, see ", TO "tensor", "."
     }



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
