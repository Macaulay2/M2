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
    Key => {(sheaf, Module), (sheaf, Ideal), (symbol^~, Module), (symbol^~, Ideal)},
    Headline => "make a coherent sheaf",
    Usage => "sheaf M\nM^~",
    Inputs => {"M" => "homogeneous" },
    Outputs => {{ "the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M" }},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R
      M = R^{1,2,3}
      sheaf M
      M^~
    ///
    }

document {
    Key => {(sheaf, Ring),(symbol^~, Ring)},
    Headline => "make a coherent sheaf of rings",
    Usage => "sheaf R\nR^~",
    Inputs => {"R"},
    Outputs => {{"the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M"}},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R
      sheaf R
      R^~
    ///
    }

-*
doc ///
  Key
    sheaf
    (sheaf, Module)
    (sheaf, Ideal)
    (symbol ~, Module)
    (symbol ~, Ideal)
  Headline
    the coherent sheaf associated to a finitely presented graded module over a graded ring
  Usage
    sheaf M
  Inputs
    M:Module
      or @TO Ideal@, a finitely presented graded module over the ring associated to some projective variety
  Outputs
    :CoherentSheaf
      $\mathcal F$, the coherent sheaf on projective space associated to the module $M$
  Description
   Text
     Given a finitely presented graded module over a homogeneous ring $R$, one may always form a coherent
     sheaf on Proj$(R)$. This method formally constructs such an object in Macaulay2. The main difference
     between such sheaves in the affine versus projective case is that any truncation of a module will determine
     the same sheaf on Proj$(R)$. To obtain the simplest (from a readability perspective) use the @TO (prune, CoherentSheaf)@
     method.
   Text
     In the following example, we demonstrate the fact that Macaulay2 will consider different truncations
     of a module as representing the same sheaf.
   Example
     X = Proj (Q = QQ[x..z])
     F = sheaf ker vars Q
     isWellDefined F
     F' = sheaf truncate(3, module F)
     module F == module F'
     F == F'
     prune F'
   Text
     As demonstrated in the above, in order to reobtain the module representing the sheaf, use the @TO module@ command.
     Since such sheaves are determined only up to truncation, any module with finite length should determine
     the 0 sheaf:
   Example
     G = sheaf coker symmetricPower(4, vars Q)
     G == 0
   Text
     This method also works when the input is an ideal instead of a sheaf:
   Example
     m = ideal vars Q;
     H = sheaf m^4
     prune H --it "untruncates" the representing module
  Caveat
    Note that in Macaulay2, a coherent sheaf is completely determined by the data of its underlying module, and
    this data is stored rather primitively. It does not keep track of any kind of patching data on associated affine
    coverings since, computationally, such data is unnecessary.
  SeeAlso
    (sheaf, Matrix)
    (cohomology, ZZ, CoherentSheaf)
    (cohomology, ZZ, SheafMap)
///
*-

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
    Key => {(symbol >=, ZZ), (symbol >=, InfiniteNumber), (symbol >=, List)},
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
  Usage
    canonicalBundle X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it. The default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the canonical bundle of a projective variety $X$. Recall that the canonical
      bundle of a normal variety is the reflexive hull (aka the double dual) of the top exterior power of the cotangent sheaf $\Omega_X$ on a variety.
    Text
      An example --the example should be Serre duality and arithmetic genus computation
    Example
      Q = QQ[x_1..x_4];
      X = Proj Q
      omega = canonicalBundle X
      for i to 3 list HH^i (tangentSheaf X)(-1)
      for i to 3 list HH^i (dual( (tangentSheaf X)(-1)) ** omega)
      --I = ideal(x_2*x_3-x_1*x_4,x_3^3-x_2*x_4^2,x_1*x_3^2-x_2^2*x_4,x_2^3-x_1^2*x_3
      --ideal(x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_2^2-x_1*x_3)
      Y = Proj(Q/(x_1^5+x_2^5+x_3^5+x_4^5))
      isSmooth Y
      omega' = canonicalBundle Y
      C = cotangentSheaf(2,Y)
      for i to 2 list HH^i (C)
      for i to 2 list HH^i ((dual C) ** omega') --dual
    Text
      We can use this to see the difference between the top exterior power of the cotangent bundle of Z and its reflexive hull.
    Example
      Z = Proj(Q/(x_1^2*x_2 - x_3^2*x_4))
      isSmooth Z
      for i to 2 list HH^i canonicalBundle Z
      for i to 2 list HH^i det cotangentSheaf Z
    Text
      Recall that the arithmetic genus is given by the number of global sections of the canonical bundle.
      Projective space has genus 0 in general, and smooth elliptic curves have genus 1. We verify the elliptic curve
      case in an example:
    Example
      R = QQ[x..z]/(y^3 - y*z^2 - x^3)
      E = Proj R
      for i to 1 list HH^i canonicalBundle E
  Caveat
      This method does not check that the input variety $X$ is normal, but rather always returns the reflexive hull of the top exterior power of the cotangent sheaf.
  SeeAlso
    cotangentSheaf
    
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
      whether to @TO prune@ the result before returning it. The default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the tangent sheaf of the projective variety $X$. It is computed by taking
      the dual of the @TO cotangentSheaf@.
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
    idealSheaf
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
      This method computes the cotangent sheaf of the projective variety $X$. The method used is to
      take the middle homology of the sequence associated to the Jacobian: --finish
    Text
      As an example we verify the Gauss-Bonnet theorem on a plane quartic curve:
    Example
      X = Proj QQ[x,y,z]/(x^4+y^4+z^4)
      genus X
      omega = cotangentSheaf X
      degree omega
  SeeAlso
    idealSheaf
    tangentSheaf
    cotangentSurjection
--    (cotangentSheaf, ZZ, ProjectiveVariety)
    ProjectiveVariety

Node
  Key
    idealSheaf
   (idealSheaf, ProjectiveVariety)
  Headline
    ideal sheaf of a projective variety
  Usage
    idealSheaf X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it. Default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the ideal sheaf of the projective variety $X$
    Text
      As an example, consider the projective variety defined by the equation $x^4 + y^4 + z^4 = 0$. The ideal sheaf of this variety is computed.
    Example
      X = Proj QQ[x,y,z]/(x^4+y^4+z^4)
      I = idealSheaf X
      rho = inducedMap((ambient I)/I, ambient I) --induced inclusion of ideal sheaf into structure sheaf of ambient ring
      for i to 2 list HH^i(image rho)
  SeeAlso
    cotangentSheaf
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
    variety
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
    (minimalPresentation, SheafMap)
    (prune,               SheafMap)
  Headline
    minimal presentation of a coherent sheaf or sheaf map
  Usage
    prune F
  Inputs
    F:CoherentSheaf
  Outputs
    :CoherentSheaf
      a sheaf represented by the depth 2-ification of the original representative module
  Description
    Text
      Given a sheaf $\mathcal F$ represented by some module $M$, there are always isomorphisms
      $$H^i (X , \mathcal{F}) = H^i_{\mathfrak{m}} (M)_0 \quad \text{for} \ i > 0,$$
      where $\mathfrak{m}$ denotes the irrelevant ideal. When $i = 0$ the best one can say in general
      is that there is an exact sequence
      $$0 \to H^0_{\mathfrak{m}} (M) \to \bigoplus_{d \geq 0} H^0 (X , \mathfrak{F}(d)) \to M \to H^1_{\mathfrak{m}} (M) \to 0.$$
      Thus, the prune command for a coherent sheaf computes a graded module $M'$ representing the same
      sheaf $\mathca{F}$, but such that there is an honest isomorphism
      $$M' \cong \bigoplus_{d \geq 0} H^0 (X , \mathcal{F} (d)).$$
      In other words, the prune command computes a module with depth at least 2 that represents the same sheaf $\mathcal{F}$.
    Example
      Q = QQ[x..z]; --KELLER: do example
    Text
      The pruning isomorphism of sheaves is cached with the key {\tt pruningMap}. It is an isomorphism of sheaves
      that is essentially never represented by an isomorphism of the underlying modules (at least in interesting cases).
    Example
      Q = QQ[x..z] -- DO example of accessing the pruning map
    Text
      Pruning also applies in a functorial way to morphisms of sheaves. 
    Text
      The method for computing the pruned representative proceeds with a few steps: first, the representative module
      $M$ is replaced by $M / H^0_{\mathfrak{m}} (M)$, which kills any torsion. Next, since $M$ is now torsion-free, we have
      an equality $\operatorname{Hom}_S (N , M) = 0$ for any module $N$ with finite length. Thus, we can choose
      a large formal Frobenius power $\mathfrak{m}^{[m]} = (x_1^m , \dots , x_n^m)$ of the maximal ideal so that $\operatorname{Hom}_S (\mathfrak{m}^{[m]} , M)$
      has depth at least $2$. Applying the functor $\operatorname{Hom}_S ( - , M)$ to the short exact sequence
      $$0 \to \mathfrak{m}^{[m]} \to S \to S / \mathfrak{m}^{[m]} \to 0$$
      yields a short exact sequence
      $$0 \to \operatorname{Hom}_S (\mathfrak{m}^{[m]} , M) \to M \to \operatorname{Ext}^1_S (S / \mathfrak{m}^{[m]} , M) \to 0.$$
      The induced map $\operatorname{Hom}_S (\mathfrak{m}^{[m]} , M) \to M$ is not an isomorphism of modules,
      but sheafifies to an isomorphism since the cokernel has finite length. This induced sheaf map is the pruning map,
      and the new representative is the sheaf associated to the module $\operatorname{Hom}_R (\mathfrak{m}^{[m]} , M)$.
    Text
      For morphisms, the above process can be made compatible with maps without much difficulty, since one only
      needs to conjugate the map by the pruning maps to get an induced map on the pruned representatives.
  Caveat
    Since the pruning operation for sheaves tends to be much more involved, one should be careful
    about the fact that pruning sheaves at every step of a computation may slow cause significant slowdowns.
  SeeAlso
    prune
///
-* TODO: this is a piece of the documentation of (prune, Module)
-- (minimalPresentation, CoherentSheaf), (prune, CoherentSheaf)
     "This function also works when ", TT "M", " is ", ofClass{GradedModule,ChainComplex,CoherentSheaf}, ",
     by acting on the modules and maps within it.",
     EXAMPLE lines ///
	  I = ideal(a^2,b^3,c^4,d^7)
     	  X = Proj R
	  J = I^~
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
--this should be updated to mention the caching of components that now occurs
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


--Should the output of this be of type CoherentSheaf instead?
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
