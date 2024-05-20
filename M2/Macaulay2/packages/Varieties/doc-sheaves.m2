-- TODO: hilbertSeries and hilbertFunction used to have this in the documentation:
--"For a projective varieties and coherent sheaves, the functionality is not yet implemented."
-- TODO: what would this mean?

undocumented {
    SheafExpression,
    -- TODO: document some of these
    (symbol ^,  CoherentSheaf, Array),
    (symbol _,  CoherentSheaf, Array),
    (symbol ?,  CoherentSheaf, CoherentSheaf),
    (symbol ==, CoherentSheaf, CoherentSheaf),
    (symbol ==, CoherentSheaf, ZZ),
    (symbol ==, ZZ, CoherentSheaf),
    }

document {
    Key => "coherent sheaves",
    "The main reason to implement algebraic varieties is support the
    computation of sheaf cohomology of coherent sheaves, which doesn't
    have an immediate description in terms of graded modules.",
    PARA{},
    "In this example, we use ", TO "cotangentSheaf", " to produce
    the cotangent sheaf on a K3 surface and compute its sheaf
    cohomology.",
    EXAMPLE {
	"R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);",
	"X = Proj R",
	"Omega = cotangentSheaf X",
	"HH^1(Omega)",
	},
    "Use the function ", TO "sheaf", " to convert a graded module to
    a coherent sheaf, and ", TO "module", " to get the graded module
    back again.",
    EXAMPLE {
	"F = sheaf coker matrix {{a,b}}",
	"module F",
	},
    SeeAlso => {
	(cohomology, ZZ, CoherentSheaf),
	(cohomology, ZZ, SumOfTwists)
	}
    }

undocumented {
    (degree, CoherentSheaf), -- TODO: why undocumented?
    (describe, CoherentSheaf),
    }

-----------------------------------------------------------------------------
-- Types and basic constructors and methods that return a sheaf
-----------------------------------------------------------------------------

document { Key => SheafOfRings,
    SeeAlso => { Variety, OO },
    Headline => "the class of sheaves of rings",
    EXAMPLE lines ///
      X = Proj(QQ[x..z])
      OO_X
      OO_X^6
    ///
    }

document { Key => CoherentSheaf, Headline => "the class of all coherent sheaves" }

document {
    Key => SumOfTwists,
    Headline => "the class of all sums of twists",
    "This class is used internally as an abstract representation of a graded module as an infinite direct sum of twists of a coherent sheaf.",
    EXAMPLE lines ///
	  R = QQ[x,y,z]
	  X = Proj R
	  OO_X(*)
	  peek oo
	  OO_X(>=2)
	  peek oo
	  Ext^0(OO_X^1, OO_X^1)
	  Ext^0(OO_X^1, OO_X^1(*))
    ///
    }

-- TODO: perhaps combine this with SumOfTwists?
document {
    Key => {
	(symbol (*), CoherentSheaf),
	(symbol (*), SheafOfRings)
	},
    Headline => "sum of twists",
    Usage => "F(*)",
    Inputs => {"F" => {" or a ", ofClass SheafOfRings}},
    Outputs => {{"a symbolic representation of the graded object consisting of the twists ", TT "F(n)", ", for all integers ", TT "n"}},
    EXAMPLE lines ///
	  R = QQ[x,y,z];
	  X = Proj R
	  Ext^0(OO_X^1, OO_X^1)
	  Ext^0(OO_X^1, OO_X^1(*))
	  Ext^0(OO_X^1, OO_X(*))
    ///
    }

document {
     Key => {OO, (symbol _, OO, Variety)},
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

document { Key => sheaf, Headline => "make a coherent sheaf" }
document {
     Key => (sheaf, Variety),
     Headline => "make a coherent sheaf",
     Usage => "sheaf X",
     Inputs => {"X"},
     Outputs => {{ "the structure sheaf of rings on the variety ", TT "X" }},
     EXAMPLE lines ///
       R = QQ[x,y,z]
       X = Proj R
       Y = Spec R
       sheaf X
       sheaf Y
     ///
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
    Key => {
	(symbol ^, SheafOfRings, List),
	(symbol ^, SheafOfRings, ZZ)},
    Headline => "make a graded free coherent sheaf",
    Usage => "M = R^{i,j,k,...}",
    Inputs => {"R",
	Nothing => {TT "{i,j,k, ...}", ", ", ofClass List, ", of integers or lists of integers"}},
    Outputs => {
	CoherentSheaf => {
	    {", a graded free coherent sheaf whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}}},
    EXAMPLE lines ///
	  R = QQ[a..d]/(a*b*c*d)
	  X = Proj R
	  OO_X^{-1,-2,3}
	  ///,
    PARA{},
    "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
    they represent multi-degrees, as in ", TO "graded and multigraded polynomial rings", ".",
    EXAMPLE lines ///
	  Y = Proj (QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}])
	  OO_Y^{{1,2},{-1,3}}
	  degrees oo
	  ///,
    SeeAlso => {OO, Proj, degrees, "graded and multigraded polynomial rings"}}

document {
    Key => {(sheaf, Variety, Module), (sheaf, Variety, Ideal)},
    Headline => "make a coherent sheaf",
    Usage => "sheaf(X,M)",
    Inputs => {"X","M"},
    Outputs => {{ "the coherent sheaf on the variety ", TT "X", " corresponding to the module ", TT "M" }},
    PARA{
	"If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is
	the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module."
	}
    }

document {
    Key => {(sheaf, Module), (sheaf, Ideal), (symbol ~, Module), (symbol ~, Ideal)},
    Headline => "make a coherent sheaf",
    Usage => "sheaf M\nM~",
    Inputs => {"M" => "homogeneous" },
    Outputs => {{ "the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M" }},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R
      M = R^{1,2,3}
      sheaf M
      M~
    ///
    }

document {
    Key => {(sheaf, Ring),(symbol ~, Ring)},
    Headline => "make a coherent sheaf of rings",
    Usage => "sheaf R\nR~",
    Inputs => {"R"},
    Outputs => {{"the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M"}},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R
      sheaf R
      R~
    ///
    }

-- TODO: perhaps combine these nodes in SumOfTwists?
document {
    Key => {
	LowerBound,
	(symbol SPACE, CoherentSheaf, LowerBound),
	(symbol SPACE, SheafOfRings, LowerBound)
	},
    Headline => "the class of lower bound objects",
    "This is a type of list that represents a lower bound.  The single element of the list is an integer, and the object represents the condition
    that some other integer, such as the index in a direct sum, should be at least as large.",
    EXAMPLE {
	"LowerBound {4}",
	">= 4",
	"> 4"
	}}
document {
    Key => {(symbol >=, ZZ), (symbol >=, InfiniteNumber)},
    Usage => "(>= d)",
    Inputs => { "d" },
    Outputs => {{"a special object of class ", TT "LowerBound", " used to represent the set of natural numbers at least as large as ", TT "d"}}
    }
document {
    Key => {(symbol >, ZZ), (symbol >, InfiniteNumber)},
    Usage => "(> d)",
    Inputs => { "d" },
    Outputs => { { "a special object of class ", TT "LowerBound", " used to represent the set of natural numbers larger than ", TT "d" } }
    }

doc ///
Node
  Key
    canonicalBundle
   (canonicalBundle, ProjectiveVariety)
   [canonicalBundle, MinimalGenerators]
   [canonicalBundle, Strategy]
  Headline
    the canonical bundle of a projective variety

Node
  Key
    tangentSheaf
   (tangentSheaf, ProjectiveVariety)
   [tangentSheaf, MinimalGenerators]
   [tangentSheaf, Strategy]
  Headline
    tangent sheaf of a projective variety
  Usage
    tangentSheaf X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the tangent sheaf of the projective variety $X$.
    Text
      Tangent sheaf of the projective plane:
    Example
      P2 = Proj QQ[a,b,c]
      TP = tangentSheaf P2
      HH^0(TP(-1))
      HH^1(TP(-3))
    Text
      Tangent sheaf of a plane nodal and cuspidal curve:
    Example
      N = Proj QQ[a,b,c]/(b^2*c-a^2*(a+c))
      TN = tangentSheaf N
      HH^0(TN), HH^1(TN)
      C = Proj QQ[a,b,c]/(b^2*c-a^3)
      TC = tangentSheaf C
      HH^0(TC), HH^1(TC)
      --- TODO: update this once we can compute the kernel
      --- of HH^0(phi) for a sheaf map phi: F -> G
  SeeAlso
    cotangentSheaf
    ProjectiveVariety

Node
  Key
    cotangentSheaf
   (cotangentSheaf, ProjectiveVariety)
   [cotangentSheaf, MinimalGenerators]
   [cotangentSheaf, Strategy]
  Headline
    cotangent sheaf of a projective variety
  Usage
    cotangentSheaf X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the cotangent sheaf of the projective variety $X$
    Text
      As an example we verify Gauss-Bonnet's theorem on a plane quartic curve:
    Example
      X = Proj QQ[x,y,z]/(x^4+y^4+z^4)
      genus X
      omega = cotangentSheaf X
      degree omega
  SeeAlso
    tangentSheaf
    (cotangentSheaf, ZZ, ProjectiveVariety)
    ProjectiveVariety

Node
  Key
    (cotangentSheaf, ZZ, ProjectiveVariety)
  Headline
    exterior powers of the cotangent sheaf of a projective variety
  Usage
    cotangentSheaf(p, X)
  Inputs
    p:ZZ
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This function computes the $p$-th exterior power of the @TO2 {cotangentSheaf, "cotangent sheaf"}@
      of a projective variety $X$, usually denoted $\Omega_X^p$.
    Text
      As an example we compute $h^{1,1}$ on a K3 surface (a quartic in projective threespace):
    Example
      K3 = Proj QQ[x_0..x_3]/(x_0^4+x_1^4+x_2^4+x_3^4-11*x_0*x_1*x_2*x_3)
      omega1 = cotangentSheaf(1, K3);
      HH^1(omega1)
    Text
      As a second example we compute @TO2 {(hh, Sequence, ProjectiveVariety), "Hodge numbers"}@
      of the Fermat quintic in projective fourspace:
    Example
      FermatQuintic = Proj QQ[x_0..x_4]/(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5)
      omega1 = cotangentSheaf(1, FermatQuintic);
      HH^1(omega1)
      HH^2(omega1)
      omega2 = cotangentSheaf(2, FermatQuintic);
      HH^1(omega2)
      HH^2(omega2)
  SeeAlso
    cotangentSheaf
    ProjectiveVariety
    (hh, Sequence, ProjectiveVariety)
///

-----------------------------------------------------------------------------
-- Basic methods for sheaves
-----------------------------------------------------------------------------

doc ///
Node
  Key
    (variety, CoherentSheaf)
    (variety, SheafOfRings)
    (variety, SumOfTwists)
    (variety, SheafMap)
  Headline
    the underlying variety over which a coherent sheaf or morphism is defined.
  Usage
    variety F
  Inputs
    F:{CoherentSheaf,SheafOfRings,SumOfTwists,SheafMap}
  Outputs
    :Variety
  Description
    Example
      X = Proj QQ[x,y,z]
      variety OO_X
      variety OO_X(3)
      variety id_(OO_X(3))
///

doc ///
Node
  Key
    (ring, CoherentSheaf)
    (ring, SheafOfRings)
    (ring, SumOfTwists)
    (ring, SheafMap)
  Headline
    the coordinate ring of the underlying variety
///

-- TODO: combine these two
document {
    Key => (module, CoherentSheaf),
    Headline => "get the module defining a coherent sheaf",
    Usage => "module F",
    Inputs => {"F"},
    Outputs => {{"the module from which the coherent sheaf ", TT "F", " was defined"}},
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      F = OO_X(3)
      module F
      degrees oo
    ///,
    SeeAlso => { OO, degrees, Proj }
    }
document {
    Key => (module, SheafOfRings),
    SeeAlso => { Variety, OO },
    Usage => "module F",
    Inputs => { "F" },
    Outputs => { { "the module corresponding to ", TT "F" }},
    EXAMPLE lines ///
      R = QQ[x..z]
      X = Proj R
      OO_X^6
      module oo
    ///
    }
-- TODO: should (module, SumOfTwists) return HH^0(F) or module F?

doc ///
Node
  Key
    (isWellDefined, CoherentSheaf)
  Headline
    whether a coherent sheaf is well-defined
  Usage
    isWellDefined F
  Inputs
    F:CoherentSheaf
  Outputs
    :Boolean
--  Description
--    Text
--    Example
  SeeAlso
    (isWellDefined, Variety)
--    (isWellDefined, Module)

Node
  Key
    isLocallyFree
   (isLocallyFree, SumOfTwists)
   (isLocallyFree, SheafOfRings)
   (isLocallyFree, CoherentSheaf)
  Headline
    whether a coherent sheaf is locally free
  Usage
    isLocallyFree F
  Inputs
    F:{SumOfTwists,SheafOfRings,CoherentSheaf}
  Outputs
    :Boolean
  Description
    Text
      This method determines whether a coherent sheaf $\mathcal F$ on $X$ is locally free;
      i.e. there is an open cover of $X$ such that $\mathcal F(U)$ for each patch $U\subset X$ is a free $\mathcal O_X(U)$-module.
    Example
      X = Proj QQ[x,y,z]
      isLocallyFree OO_X
      isLocallyFree OO_X^{1,2,3}
      F = cotangentSheaf X
      isLocallyFree F
    Text
      Internally, the algorithm uses a computation of @TO2 {fittingIdeal, "fitting ideals"}@.
  SourceCode
    (isLocallyFree, CoherentSheaf)
  SeeAlso
    fittingIdeal
    isFreeModule
///

document {
    Key => {(codim, CoherentSheaf), [(codim, CoherentSheaf), Generic]},
    Headline => "codimension of the support of a coherent sheaf on a projective variety",
    Usage => "codim F",
    Inputs => {"F" => {"a coherent sheaf over a ", TO "ProjectiveVariety", TT " X"}},
    Outputs => {ZZ},
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

doc ///
Node
  Key
    (rank, CoherentSheaf)
  Headline
    rank of a coherent sheaf
  Usage
    rank F
  Inputs
    F:CoherentSheaf
  Outputs
    :ZZ
--  Description
--    Text
--    Example
  SeeAlso
    (rank, Module)
    (degree, Module)
    (degree, Ring)

Node
  Key
    (degrees, CoherentSheaf)
  Headline
    degrees of a coherent sheaf
  Usage
    degrees F
  Inputs
    F:CoherentSheaf
  Outputs
    :List
--  Description
--    Text
--    Example
  SeeAlso
    (degrees, Module)
///

document {
    Key => (numgens, CoherentSheaf),
    Headline => "the number of generators of the underlying module",
    Usage => "numgens F",
    Inputs => {"F"},
    Outputs => { ZZ => {"number of generators of the underlying module ", TT "M", " of ", TT "F"} },
    "In Macaulay2, each coherent sheaf comes equipped with a module over
    the coordinate ring.  In the homogeneous case, this is not
    necessarily the number of generators of the sum of twists ",
    TT "H^0(F(d))", ", summed over all d, which in fact could be infinitely
    generated.",
    EXAMPLE {
	"R = QQ[a..d]/(a^3+b^3+c^3+d^3)",
	"X = Proj R;",
	"T' = cotangentSheaf X",
	"numgens T'",
	"module T'"
	},
    SeeAlso => {(module, CoherentSheaf), tangentSheaf}
    }

-* TODO: does this still make sense? compare with (betti, Module)
Node
  Key
   (betti, CoherentSheaf)
  Heading
    Betti diagram showing the degrees of generators and relations of a homogeneous module or coherent sheaf
  Usage
    betti M
  Inputs
    M:{Module,CoherentSheaf}
    Weights=>List
  Outputs
    :BettiTally
      showing the zero-th, first graded, and total Betti numbers of $M$.
  Description
    Text
      Note that the Betti numbers are not minimized.
    Example
      S = ZZ/10007[x,y];
      betti coker matrix{{x^3, x*y^2}, {y*x^2, y^3}}
      betti coker map(S^{0,-1}, , matrix{{x^2, y}, {y^3, x^2}})
*-

-- TODO: (super, CoherentSheaf)
-- TODO: (cover, CoherentSheaf)
-- TODO: (ambient, CoherentSheaf)

document {
    Key => (hilbertPolynomial, CoherentSheaf),
    Headline => "compute the Hilbert polynomial of the coherent sheaf",
    Usage => "hilbertPolynomial S",
    Inputs => {"S"},
    Outputs => {ProjectiveHilbertPolynomial => "unless the option Projective is false"},
    "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"), " of a coherent sheaf.",
    EXAMPLE {
	"R = ZZ/101[x_0..x_2];",
	"V = Proj R;",
	"S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	"h = hilbertPolynomial S",
	"hilbertPolynomial(S, Projective=>false)"
	}
    }

document {
    Key => (pdim, CoherentSheaf),
    Headline => "calculate the projective dimension",
    Usage => "pdim S",
    Inputs => {"S" => CoherentSheaf},
    Outputs => {ZZ => "the projective dimension"},
    EXAMPLE {
	"V = Proj(ZZ/101[x_0..x_2]);",
	"S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	"pdim S"}
    }

doc ///
Node
  Key
    (minimalPresentation, CoherentSheaf)
    (prune,               CoherentSheaf)
  Headline
    minimal presentation of a coherent sheaf
///
-* TODO: this is a piece of the documentation of (prune, Module)
-- (minimalPresentation, CoherentSheaf), (prune, CoherentSheaf)
     "This function also works when ", TT "M", " is ", ofClass{GradedModule,ChainComplex,CoherentSheaf}, ",
     by acting on the modules and maps within it.",
     EXAMPLE lines ///
	  I = ideal(a^2,b^3,c^4,d^7)
     	  X = Proj R
	  J = (module I)~
	  minimalPresentation J
     ///,
*-

-----------------------------------------------------------------------------
-- Arithmetic operations
-----------------------------------------------------------------------------

document {
    Key => {(symbol SPACE, CoherentSheaf, ZZ), (symbol SPACE, SheafOfRings, ZZ)},
    Headline => "canonical twist of a coherent sheaf",
    Usage => "F(n)",
    Inputs => {"F" => {"or ", ofClass SheafOfRings, ", on a projective variety"}, "n"},
    Outputs => { CoherentSheaf => "the twist of F on a projective variety by the n-th power of the hyperplane line bundle." },
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      F = OO_X
      G = F(3)
      module G
      degrees oo
    ///
    }
-- TODO: (symbol SPACE, CoherentSheaf, Sequence)
-- TODO: also for SumOfTwists?

document {
    Key => (dual, CoherentSheaf),
    Headline => "dual coherent sheaf",
    TT "dual M", " -- the dual of a coherent sheaf."
    }

-- TODO: should this move up, next to SheafOfRings ^ List?
document {
    Key => {
	(symbol ^, CoherentSheaf, ZZ),
	(symbol ^, CoherentSheaf, List)},
    Headline => "direct sum",
    Usage => "F^n",
    Inputs => {"F" => {", or a ", ofClass SheafOfRings}, "n"},
    Outputs => {CoherentSheaf => {"the direct sum of ", TT "n", " copies of ", TT "F"},},
    EXAMPLE lines ///
      R = QQ[a..d]/(a*d-b*c)
      Q = Proj R
      OO_Q^5
      IL = sheaf module ideal(a,b)
      IL^3
    ///,
    SeeAlso => {Proj, sheaf}
    }

-- TODO: (directSum, CoherentSheaf)?
document {
    Key => (symbol ++, CoherentSheaf, CoherentSheaf),
    Headline => "direct sum of coherent sheaves",
    Usage => "F ++ G",
    Inputs => {"F","G"},
    Outputs => {{"the direct sum of ", TT "F", " and ", TT "G"}},
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      OO_X(3) ++ OO_X(4)
      module oo
    ///
    }

document {
    Key => (symbol **, CoherentSheaf, CoherentSheaf),
    Headline => "tensor produce of coherent sheaves",
    Usage => "F ** G",
    Inputs => {"F","G"},
    Outputs => {{"the tensor product of ", TT "F", " and ", TT "G"}},
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      OO_X(-3) ++ OO_X(4)
      oo ** oo
    ///
    }

document {
    Key => (symbol ^**, CoherentSheaf, ZZ),
    Headline => "tensor power",
    Usage => "M^**i",
    Inputs => {"M" , "i" },
    Outputs => {CoherentSheaf => { "the ", TT "i", "-th tensor power of ", TT "M"}},
    "The second symmetric power of the canonical sheaf of the
    rational quartic:",
    EXAMPLE lines ///
      R = QQ[a..d];
      I = monomialCurveIdeal(R,{1,3,4})
      X = variety I
      KX = sheaf(Ext^1(I,R^{-4}) ** ring X)
      K2 = KX^**2
      prune K2
    ///,
    "Notice that the resulting sheaf is not always presented in the most
    economical manner.  Use ", TO prune, " to improve the presentation.",
    SeeAlso => {monomialCurveIdeal, Ext, variety, sheaf, prune}
    }

document {
    Key => {(symbol /, CoherentSheaf, CoherentSheaf), (symbol /, CoherentSheaf, Ideal)},
    Headline => "quotient of coherent sheaves",
    Usage => "F / G",
    Inputs => { "F", "G" => {"or ", ofClass Ideal} },
    Outputs => { CoherentSheaf => {"the quotient sheaf ", TT "F/G"} },
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

-- TODO: (symbol *, Ideal, CoherentSheaf)

document {
    Key => (exteriorPower, ZZ, CoherentSheaf),
    Usage => "exteriorPower(i,F)",
    Inputs => {"i","F"},
    Outputs => {{ "the ", TT "i", "-th exterior power of ", TT "F"}}
    }

-- TODO: (symmetricPower, ZZ, CoherentSheaf)

doc ///
Node
  Key
   (annihilator, CoherentSheaf)
  Headline
    the annihilator ideal
  Usage
    ann F
    annihilator F
  Inputs
    F:{CoherentSheaf}
    Strategy=>Symbol
      either @TT "Quotient"@ or @TT "Intersection"@; see @TO [annihilator, Strategy]@
  Outputs
    :Ideal
      the annihilator ideal
  Description
    Text
      @stacksProject("0H2G", "The annihilator of a sheaf of modules")@ $\mathcal F$
      is the ideal corresponding to the kernel of the map of sheaves
      $$ \mathcal O_X \to \mathcal Hom_{\mathcal O_X}(\mathcal F, \mathcal F).b $$

      You may use @TT "ann"@ as a synonym for @TT "annihilator"@.
  SeeAlso
    (annihilator, Module)
    (isSupportedInZeroLocus, Ideal, Module)
///
