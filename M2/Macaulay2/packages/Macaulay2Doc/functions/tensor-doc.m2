--- status: TODO
--- author(s):
--- notes:

-- TODO:
-- (tensor, Matrix, Matrix)
-- (tensor, Module, Module)

L := {
    (tensor, QuotientRing,   QuotientRing),
    (tensor, PolynomialRing, PolynomialRing),
    (tensor, QuotientRing,   PolynomialRing),
    (tensor, PolynomialRing, QuotientRing)
    }
undocumented L
undocumented flatten table(L, {
	DegreeLift, DegreeMap, DegreeRank, Degrees, DegreeGroup, Global, Heft,
	Constants, Join, Local, MonomialOrder, MonomialSize, VariableBaseName,
	Inverses, Variables, SkewCommutative, Weights, WeylAlgebra}, (m,o) -> [m, o])

doc ///
Node
  Key
    (tensor, Monoid, Monoid)
   [(tensor, Monoid, Monoid), DegreeLift]
   [(tensor, Monoid, Monoid), DegreeMap]
   [(tensor, Monoid, Monoid), DegreeRank]
   [(tensor, Monoid, Monoid), DegreeGroup]
   [(tensor, Monoid, Monoid), Degrees]
   [(tensor, Monoid, Monoid), Global]
   [(tensor, Monoid, Monoid), Heft]
   [(tensor, Monoid, Monoid), Inverses]
   [(tensor, Monoid, Monoid), Constants]
   [(tensor, Monoid, Monoid), Join]
   [(tensor, Monoid, Monoid), Local]
   [(tensor, Monoid, Monoid), MonomialOrder]
   [(tensor, Monoid, Monoid), MonomialSize]
   [(tensor, Monoid, Monoid), VariableBaseName]
   [(tensor, Monoid, Monoid), Variables]
   [(tensor, Monoid, Monoid), SkewCommutative]
   [(tensor, Monoid, Monoid), Weights]
   [(tensor, Monoid, Monoid), WeylAlgebra]
    (tensor, Ring, Ring)
   [(tensor, Ring, Ring), DegreeLift]
   [(tensor, Ring, Ring), DegreeMap]
   [(tensor, Ring, Ring), DegreeRank]
   [(tensor, Ring, Ring), DegreeGroup]
   [(tensor, Ring, Ring), Degrees]
   [(tensor, Ring, Ring), Global]
   [(tensor, Ring, Ring), Heft]
   [(tensor, Ring, Ring), Inverses]
   [(tensor, Ring, Ring), Constants]
   [(tensor, Ring, Ring), Join]
   [(tensor, Ring, Ring), Local]
   [(tensor, Ring, Ring), MonomialOrder]
   [(tensor, Ring, Ring), MonomialSize]
   [(tensor, Ring, Ring), VariableBaseName]
   [(tensor, Ring, Ring), Variables]
   [(tensor, Ring, Ring), SkewCommutative]
   [(tensor, Ring, Ring), Weights]
   [(tensor, Ring, Ring), WeylAlgebra]
    (symbol**, Ring, Ring)
    (symbol^**, Ring, ZZ)
    (symbol**, Monoid, Monoid)
    (symbol^**, Monoid, ZZ)
  Headline
    tensor product of monoids
  Usage
    C = A ** B
    C = tensor(A, B, ...)
  Inputs
    A:{Ring,Monoid}
    B:{Ring,Monoid}
    DegreeRank       => ZZ      -- see @TO [monoid,DegreeRank]@
    Degrees          => List    -- see @TO [monoid,Degrees]@
    Inverses         => Boolean -- see @TO [monoid,Inverses]@
    Global           => Boolean -- see @TO [monoid,Global]@
    Local            => Boolean -- see @TO [monoid,Local]@
    MonomialOrder    => List    -- see @TO [monoid,MonomialOrder]@
    MonomialSize     => ZZ      -- see @TO [monoid,MonomialSize]@
    Variables        => List    -- see @TO [monoid,Variables]@
    VariableBaseName => Symbol  -- see @TO [monoid,VariableBaseName]@
    Heft             => List    -- see @TO [monoid,Heft]@
    Join             => Boolean -- overrides the corresponding option in {\tt A}; see @TO [monoid,Join]@
    DegreeMap        => Boolean -- overrides the corresponding option in {\tt A}; see @TO [monoid,DegreeMap]@
    DegreeLift       => Boolean -- overrides the corresponding option in {\tt A}; see @TO [monoid,DegreeLift]@
    Constants        => Boolean -- ignored by this routine
    SkewCommutative  => Boolean -- ignored by this routine
    WeylAlgebra      => List    -- ignored by this routine
    Weights          => List    -- ignored by this routine
  Outputs
    C:{Ring,Monoid}
      tensor product of the monoids or rings
  Description
    Text
      This is the same as {\tt A ** B} except that options are allowed,
      see @TO (symbol **, Monoid, Monoid)@ and @TO (symbol **, Ring, Ring)@.
      This method allows many of the options available for monoids, see @TO monoid@ for details.
      This method essentially combines the variables of {\tt A} and {\tt B} into one monoid or ring.
    Example
      kk = ZZ/101
      A = kk[a,b]
      B = kk[c,d,e]
    Text
      The simplest version is to simply use @TO symbol**@:
    Example
      describe(A**B)
    Text
      If you wish to change the variable names:
    Example
      describe tensor(A, B, VariableBaseName => p)
      describe tensor(A, B, Variables => {a1,a2,b1,b2,b3})
    Text
      The tensor product of two singly graded rings is bigraded.
      Sometimes you want a singly graded ring. Here is one way to get it:
    Example
      describe (C = tensor(A, B, DegreeRank => 1, Degrees => {5:1}))
      degreeLength C
      degreesRing C
    Text
      Packing monomials into smaller space is more efficient, but less flexible.
      The default is 32 bits, so if you want to pack them into 8 bit exponents, use:
    Example
      describe tensor(A, B, MonomialSize => 8)
    Text
      The default monomial order for tensor products is a product order.
      Sometimes other orders are more desirable, e.g. GRevLex, or an elimination order:
    Example
      describe (C = tensor(A, B, MonomialOrder => Eliminate numgens A))
      describe (C = tensor(A, B, MonomialOrder => GRevLex))
    Text
      If you tensor two skew-commutative rings, (or one skew commutative ring with a commutative polynomial ring),
      then all of the skew-commuting variables skew commute with each other:
    Example
      As = kk[a, b, SkewCommutative => true]
      D  = kk[c, d, e, SkewCommutative => true]
      E = tensor(As, D)
      describe E
      c * a
    Text
      Similarly, tensoring two Weyl algebras (or one and a polynomial ring) produces
      a Weyl algebra with both sets of non-commuting pairs.
    Example
      E = kk[x, Dx, WeylAlgebra => {x => Dx}]
      tensor(E, E, Variables => {x, Dx, y, Dy})
      describe oo
    Text
      Two polynomial rings must have the same coefficient ring, otherwise an error is issued.
      Currently, there is no way to specify other rings over which to define the tensor product.
    Example
      A = ZZ/101[a, b]
      B = A[x, y]
      C = tensor(B, B, Variables => {x1, y1, x2, y2})
      describe C
    Text
      The flat monoid with the all variables visible, including those from the base ring,
      can be obtained as follows.
    Example
      C.FlatMonoid
  Caveat
    Not all of the options for monoid are useful here. Some are silently ignored.
  SeeAlso
    describe
    degreesRing
    degreeLength
    symbol**
    FlatMonoid

Node
  Key
    (tensor, RingMap, Matrix)
    (tensor, RingMap, Module)
    (symbol**, RingMap, Matrix)
    (symbol**, RingMap, Module)
  Headline
    tensor product via a ring map
  Usage
    f ** M
    tensor(f, M)
  Inputs
    f:RingMap
      from $R \to S$
    M:{Matrix,Module}
      over the source ring $R$ of {\tt f}
  Outputs
    :{Matrix,Module}
      the same type as {\tt M}, but over the target ring $S$ of {\tt f}
  Description
    Example
      R = QQ[a..d]
      S = QQ[s,t]
      F = map(S,R,{s^4,s^3*t,s*t^3,t^4})
      m = matrix{{a,b,c,d}}
      F ** m
      F ** image m
  SeeAlso
    (symbol SPACE, RingMap, Module)
    (symbol SPACE, RingMap, Module)
///

doc ///
Node
  Key
    (tensor, Module, Module)
    (symbol**, Module, Module)
  Headline
    tensor product of modules
  Usage
    M ** N
    tensor(M, N)
  Inputs
    M:Module
    N:Module
  Outputs
     :Module
       the tensor product of $M$ and $N$
  Description
    Text
      If $M$ has generators $m_1, $m_2, \dots, $m_r$, and $N$ has generators $n_1, n_2, \dots, n_s$,
      then $M \otimes N$ has generators $m_i\otimes n_j$ for $0<i\leq r$ and $0<j\leq s$.
    Example
      R = ZZ[a..d];
      M = image matrix {{a,b}}
      N = image matrix {{c,d}}
      M ** N
      N ** M
    Text
      Use @TO trim@ or @TO minimalPresentation@ if a more compact presentation is desired.
    Text
      Use @TO flip@ to produce the isomorphism $M \otimes N \to N \otimes M$.
    Text
      To recover the factors from the tensor product, use the function @TO formation@.
  SeeAlso
    flip
    (tensor, Module, Matrix)
    (tensor, Matrix, Matrix)
    formation

Node
  Key
    (tensor, Matrix, Module)
    (tensor, Module, Matrix)
    (symbol **, Matrix, Module)
    (symbol **, Module, Matrix)
  Headline
    tensor product
  Usage
    f ** M
    M ** f
    tensor(f, M)
    tensor(M, f)
  Inputs
    f:Matrix
    M:Module
  Outputs
     :Matrix
       formed by tensoring $f$ with the identity map of $M$
  Description
    Example
      R = ZZ/101[x,y];
      R^2 ** vars R
      (vars R) ** R^2
    Text
      When $N$ is a free module of rank 1 the net effect of the operation is to shift the degrees of $f$.
    Example
      R = ZZ/101[t];
      f = matrix {{t}}
      degrees source f
      degrees source (f ** R^{-3})
  SeeAlso
    adjoint
    (tensor, Module, Module)
    (tensor, Matrix, Matrix)
  Subnodes
    flip
    tensorAssociativity

Node
  Key
    (tensor, Matrix, Matrix)
    (symbol**, Matrix, Matrix)
  Headline
    tensor product
  Usage
    f ** g
    tensor(f, g)
  Inputs
    f:Matrix
    g:Matrix
  Outputs
     :Matrix
       the tensor product of maps $f$ and $g$
  Description
    Text
      Other names for the tensor product include: the outer product, or the Kronecker product of two matrices.
    Example
      R = ZZ[a..d];
      f = matrix {{a,b}}
      g = transpose matrix {{c,d}}
      f ** g
  SeeAlso
    flip
    (tensor, Module, Module)
    (tensor, Matrix, Module)
    --(tensor, Vector, Vector)

Node
  Key
    --(tensor, Vector, Vector)
    (symbol**, Vector, Vector)
  Headline
    tensor product
  Usage
    v ** w
    tensor(v, w)
  Inputs
    v:Vector
    w:Vector
  Outputs
     :Vector
       the tensor product of $v$ and $w$
  Description
    Text
      If $v$ is in the module $M$ and $w$ is in the module $N$, then $v\otimes w$ is in the module $M\otimes N$.
    Example
      R = ZZ[a..d];
      F = R^3
      G = coker vars R
      v = (a-37)*F_1
      v ** G_0
  SeeAlso
    flip
    (tensor, Module, Module)
    (tensor, Matrix, Module)
    (tensor, Matrix, Matrix)

Node
  Key
    flip
   (flip, Module, Module)
  Headline
    isomorphism map of commutativity of tensor product
  Usage
    flip(F, G)
  Inputs
    F:Module
    G:Module
  Outputs
     :Matrix
       the matrix representing the natural isomorphism $F \otimes G \to G \otimes F$
  Description
    Example
      R = QQ[x,y];
      F = R^{1,2,3}
      G = R^{10,20,30}
      f = flip(F,G)
      isHomogeneous f
      target f
      source f
      target f === G**F
      source f === F**G
      u = x * F_0
      v = y * G_1
      u ** v
      v ** u
      f * (u ** v)
      f * (u ** v) === v ** u

Node
  Key
    --(tensor, Module, Ring)
    --(tensor, Ideal,  Ring)
    --(tensor, Ring, Ideal)
    --(tensor, Ring, Module)
    (symbol **, Module, Ring)
    (symbol **, Ideal,  Ring)
    (symbol **, Ring, Ideal)
    (symbol **, Ring, Module)
  Headline
    tensor product
  Usage
    M ** R
    R ** M
    tensor(M, R)
    tensor(R, M)
  Inputs
    M:{Module,Ideal}
    R:Ring
  Outputs
     :Module
       over $R$, obtained by forming the tensor product of the module $M$ with $R$
  Description
    Text
      If the ring of $M$ is a base ring of $R$ then the matrix presenting
      the module will be simply promoted (see @TO promote@).
      Otherwise, a ring map from the ring of @TT "M"@ to @TT "R"@ will be
      constructed by examining the names of the variables, as described in @TO (map, Ring, Ring)@.
    Example
      R = ZZ/101[x,y];
      M = coker vars R
      M ** R[t]

Node
  Key
    --(tensor, Matrix, Ring)
    --(tensor, Ring, Matrix)
    (symbol **, Matrix, Ring)
    (symbol **, Ring, Matrix)
  Headline
    tensor product
  Usage
    f ** R
    R ** f
  Inputs
    f:Matrix
    R:Ring
  Outputs
     :Matrix
       over $R$, obtained by forming the tensor product of the map $f$ with $R$
  Description
    Text
      The ring of $f$ should be a base ring of $R$.  The degree of the map is preserved.
    Example
      R = ZZ[a..c];
      S = R/(a+b+c);
      f = vars R
      f ** S
///

document {
    Key => (symbol ^**, Module, ZZ),
    Headline => "tensor power",
    Usage => "M^**i",
    Inputs => { "M", "i" },
    Outputs => {Module => { "the ", TT "i", "-th tensor power of ", TT "M"}},
    "The second symmetric power of the canonical module of the rational quartic:",
    EXAMPLE lines ///
         R = QQ[a..d];
         I = monomialCurveIdeal(R,{1,3,4})
	 M = Ext^1(I,R^{-4})
	 M^**2
	 ///
    }

document {
    Key => {
	 tensorAssociativity,
	(tensorAssociativity, Module, Module, Module),
    },
    Headline => "associativity isomorphisms for tensor products",
    TT "tensorAssociativity(A,B,C)", " -- produces the isomorphism from
    A**(B**C) to (A**B)**C.",
    PARA{},
    "Currently implemented for modules and chain complexes.",
    SeeAlso => {"OldChainComplexes :: ChainComplex", "Module"}
}
