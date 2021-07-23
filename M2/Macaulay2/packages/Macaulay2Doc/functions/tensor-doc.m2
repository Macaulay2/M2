--- status: TODO
--- author(s):
--- notes:

-- TODO:
-- (tensor, Matrix, Matrix)
-- (tensor, Module, Module)

undocumented {
    (tensor, QuotientRing,   QuotientRing),
    (tensor, PolynomialRing, PolynomialRing),
    (tensor, QuotientRing,   PolynomialRing),
    (tensor, PolynomialRing, QuotientRing),
    }

doc ///
Node
  Key
    (tensor, Monoid, Monoid)
   [(tensor, Monoid, Monoid), DegreeLift]
   [(tensor, Monoid, Monoid), DegreeMap]
   [(tensor, Monoid, Monoid), DegreeRank]
   [(tensor, Monoid, Monoid), Degrees]
   [(tensor, Monoid, Monoid), Global]
   [(tensor, Monoid, Monoid), Heft]
   [(tensor, Monoid, Monoid), Inverses]
   [(tensor, Monoid, Monoid), Join]
   [(tensor, Monoid, Monoid), Local]
   [(tensor, Monoid, Monoid), MonomialOrder]
   [(tensor, Monoid, Monoid), MonomialSize]
   [(tensor, Monoid, Monoid), VariableBaseName]
   [(tensor, Monoid, Monoid), Variables]
   [(tensor, Monoid, Monoid), SkewCommutative]
   [(tensor, Monoid, Monoid), Weights]
   [(tensor, Monoid, Monoid), WeylAlgebra]
    (tensor, Ring,   Ring)
    (symbol**, Monoid, Monoid)
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
    SkewCommutative  => Boolean -- ignored by this routine
    Weights          => List    -- ignored by this routine
    WeylAlgebra      => List    -- ignored by this routine
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

Node
  Key
    (symbol**, List, List)
  Headline
    Cartesian product of two lists
  Description
    Example
      {1, 2} ** {10, 20, 30}
///
