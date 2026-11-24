--- status: Draft
--- author(s): MES
--- notes: 

undocumented{(numgens, InexactField)}

doc ///
  Key
    numgens
  Headline
    the number of generators
///
doc ///
  Key
    (numgens,Module)
  Headline
    number of generators of a module
  Usage
    numgens M
  Inputs
    M:
  Outputs
    :ZZ
      number of generators of M
  Description
    Text
      In Macaulay2, each module comes equipped with a matrix of generators. It
      is the number of columns of this matrix which is returned. If the module
      is graded, this may or may not be the number of minimal generators.
    Example
      R = QQ[a..d];
      M = ker vars R
      generators M
      numgens M
    Text
      The number of generators of a free module is its rank.
    Example
      numgens R^10
  SeeAlso
    generators
    trim
    prune
    mingens
    ker
    vars
///
doc ///
  Key
    (numgens, Monoid)
    -- TODO: combine with (numgens, Ring) below
  Headline
    number of generators of a monoid
  Usage
    numgens M
  Inputs
    M:
  Outputs
    :ZZ
      number of generators of M
  Description
    Example
      M = monoid[x_1..x_10];
      numgens M
  SeeAlso
    monoid
///
doc ///
  Key
    (numgens,Ideal)
  Headline
    number of generators of an ideal
  Usage
    numgens I
  Inputs
    I:
  Outputs
    :ZZ
      number of generators of I
  Description
    Text
      In Macaulay2, each ideal comes equipped with a matrix of generators. It
      is the number of columns of this matrix that is returned. If the ideal is
      homogeneous, this may or may not be the number of minimal generators.
    Example
      R = QQ[a..d];
      I = ideal(a^2-b*d, a^2-b*d, c^2, d^2);
      numgens I
    Text
      In order to find a more efficient set of generators, use @TO mingens@ or
      @TO trim@.
    Example
      mingens I
      numgens trim I
  SeeAlso
    mingens
    trim
    generators
///
doc ///
  Key
    (numgens,Ring)
    (numgens, EngineRing)
    (numgens, FractionField)
    (numgens, MonomialIdeal)
    (numgens, PolynomialRing)
    (numgens, QuotientRing)
  Headline
    number of generators of a polynomial ring
  Usage
    numgens R
  Inputs
    R:
  Outputs
    :ZZ
      number of generators of R over the coefficient ring
  Description
    Text
      If the ring @TT "R"@ is a fraction ring or a (quotient of a) polynomial
      ring, the number returned is the number of generators of @TT "R"@ over
      the coefficient ring. In all other cases, the number of generators is
      zero.
    Example
      numgens ZZ
      A = ZZ[a,b,c];
      numgens A
      KA = frac A
      numgens KA
    Text
      If the ring is polynomial ring over another polynomial ring, then only
      the outermost variables are counted.
    Example
      B = A[x,y];
      numgens B
      C = KA[x,y];
      numgens C
    Text
      In this case, use the @TO "CoefficientRing"@ option to @TO generators@ to
      obtain the complete set of generators.
    Example
      g = generators(B, CoefficientRing=>ZZ)
      #g
    Text
      Galois fields created using @TO GF@ have zero generators, but their
      underlying polynomial ring has one generators.
    Example
      K = GF(9,Variable=>a)
      numgens K
      R = ambient K
      numgens R
  SeeAlso
    generators
    minPres
    GF
    ambient
///
