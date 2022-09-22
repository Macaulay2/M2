--- status: Rewritten August 2022
--- author(s): Mahrud
--- notes: functions below are all defined in monoids.m2

undocumented {
    (monoid, PolynomialRing),
    (monoid, FractionField),
    (monoid, QuotientRing),
    }

-- TODO: document Constants => Boolean, whether to use rawTowerRing when making a monoid ring
doc ///
Node
  Key
     monoid
    (monoid, Array)
    (monoid, List)
  Headline
    make or retrieve a monoid
  Usage
    monoid[a,b,c,...]
    monoid{a,b,c,...}
  Inputs
    TT "[a,b,c,...]":Array
      listing generators of the monoid, as well as optional arguments.
    TT "{a,b,c,...}":List
      same as above, but equivalent to also passing @TT "Local => true"@.
    Variables        => List    -- see @TO [monoid, Variables]@
    VariableBaseName => Symbol  -- see @TO [monoid, VariableBaseName]@
    Global           => Boolean -- see @TO [monoid, Global]@
    Local            => Boolean -- see @TO [monoid, Local]@
    Inverses         => Boolean -- see @TO [monoid, Inverses]@
    Weights          => List    -- see @TO [monoid, Weights]@
    Degrees          => List    -- see @TO [monoid, Degrees]@
    DegreeMap        => Boolean -- see @TO [monoid, DegreeMap]@
    DegreeLift       => Boolean -- see @TO [monoid, DegreeLift]@
    DegreeRank       => ZZ      -- see @TO [monoid, DegreeRank]@
    Heft             => List    -- see @TO [monoid, Heft]@
    Join             => Boolean -- see @TO [monoid, Join]@
    MonomialOrder    => List    -- see @TO [monoid, MonomialOrder]@
    MonomialSize     => ZZ      -- see @TO [monoid, MonomialSize]@
    SkewCommutative  => Boolean -- see @TO [monoid, SkewCommutative]@
    WeylAlgebra      => List    -- see @TO [monoid, WeylAlgebra]@
  Outputs
    :Monoid
  Description
    Text
      The function @TO monoid@ is called whenever a polynomial ring is created, see
      @TO (symbol SPACE, Ring, Array)@, or when a local polynomial ring is made, see
      @TO (symbol SPACE, Ring, List)@. Some of the options provided when making a
      monoid don't take effect until the monoid is made into a polynomial ring.

      Let's make a free ordered commutative monoid on the variables @TT "a,b,c"@,
      with degrees 2, 3, and 4, respectively.
    Example
      M = monoid[a,b,c, Degrees => {2,3,4}]
      degrees M
      M_0 * M_1^6
    Text
      Call @TO use@ to assign the variables their values in the monoid.
    Example
      monoid[x,y,z]
      x
      use ooo
      x * y^6
    Text
      The options used when the monoid was created can be recovered with @TO options@.
    Example
      options M
      describe M
      toExternalString M
    Text
      The variables listed may be symbols or indexed variables. The values assigned to
      these variables are the corresponding monoid generators. The function
      @TO baseName@ may be used to recover the original symbol or indexed variable.

      The @TO [monoid, Heft]@ option is used, for instance, by @TO (Ext, Module, Module)@.
    Example
      R = ZZ[x,y, Degrees => {-1,-2}, Heft => {-1}]
      degree \ gens R
      transpose vars R
    Text
      By default, (multi)degrees are concatenated when forming polynomial rings over
      polynomial rings, as can be seen by examining the corresponding flattened monoid,
      which displays information about all of the variables.
    Example
      QQ[x][y]
      oo.FlatMonoid
      QQ[x][y][z]
      oo.FlatMonoid
    Text
      That behavior can be overridden with the @TO [monoid, Join]@ option.
    Example
      QQ[x][y, Join => false]
      oo.FlatMonoid
  Subnodes
    :Optional arguments
    [monoid, Variables] -- and VariableBaseName
    [monoid, Local]     -- and Global
    [monoid, Inverses]
    [monoid, Weights]
    [monoid, Degrees]   -- and DegreeRank
    [monoid, DegreeMap] -- and DegreeLift
    [monoid, Heft]
    [monoid, Join]
    [monoid, MonomialOrder]
    [monoid, MonomialSize]
    [monoid, SkewCommutative]
    [monoid, WeylAlgebra]

Node
  Key
    [monoid, Variables]
    [monoid, VariableBaseName]
  Headline
    specify the names of the indeterminates
  Usage
    monoid[Variables => L]
    monoid[Variables => n, VariableBaseName => s]
  Description
    Text
      When given a list or sequence, @TT "Variables => L"@ specifies the variables to
      be used as generators of the monoid.
    Example
      QQ[Variables => vars(0..3)]
      QQ[Variables => x_(0,0)..x_(3,3)]
    Text
      When given a number, @TT "Variables => n"@ specifies number of
      @TO2(IndexedVariable, "indexed variables")@ to create with base name provided
      by @TT "VariableBaseName => s"@, where @TT "s"@ may be either a symbol or string.
      The default base name is @TT "p"@.
    Example
      QQ[Variables => 2]
      QQ[Variables => 3, VariableBaseName => v]
      QQ[Variables => 4, VariableBaseName => "e"]
      class baseName e_0
      class e
    Text
      This option is also useful when creating a new ring from an existing ring, creating
      a tensor product ring, or symmetric algebra.
    Example
      R = QQ[x, y, Degrees => {1, 2}]
      newRing(R, Variables => {a,b})
      degrees oo
      tensor(R, R, Variables => t_(0,0)..t_(1,1))
      degrees oo
      symmetricAlgebra(R^3, Variables => s_0..s_2)
  SeeAlso
    vars
    baseName
    IndexedVariable
    IndexedVariableTable
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, Local]
    [monoid, Global]
  Headline
    specify local or global monomial order
  Usage
    monoid[x,y,z, Local => P, Global => Q]
  Description
    Text
      The optional argument @TT "Global"@ specifies whether all variables are larger
      than 1 in the monomial order. The default value @TT "Global => true"@ means that
      rings in Macaulay2 are by default global rings. If set to true, and the option
      @TO2([monoid, Inverses], TT "Inverses => true")@ is not specified, an error is
      raised if any of the variables are not greater than 1 in the @TO2(MonomialOrder, "monomial ordering")@,
      as required by the standard theory of Gröbner bases.
    Example
      QQ[x,y, Weights => {-1,1}, Global => false]
      1 > x
      1 < y
      leadTerm matrix{{x+x^2, y+y^2}}
    Text
      The optional argument @TT "Local"@ specifies whether all variables are smaller than
      1 in the monomial order. Providing @TT "Local => true"@ induces a local monomial
      ordering and in particular implies @TT "Global => false"@. A shortcut for indicating
      a local monomial ordering is to use a @TO List@ to provide the variables.
    Example
      QQ[x,y, Local => true]
      1 > x
      1 > y
      QQ{x,y}
      leadTerm matrix{{x+x^2, y+y^2}}
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    "heft vectors"
    "monomial orderings"
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, Inverses]
  Headline
    allow negative exponents in monomials
  Usage
    monoid[X,Y, Inverses => P]
  Description
    Text
      The optional argument @TT "Inverses"@ specifies whether negative exponents will
      be allowed, making the monoid into a group. This option is mainly used by
      @TO degreesMonoid@ and @TO degreesRing@ functions to allow negative exponents
      in the monomials.
    Example
      S = QQ[x,y, Inverses => true, MonomialOrder => Lex]
      S_{1,-1} + S_{-1,1}
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, Weights]
  Headline
    specify weights of the variables
  Usage
    monoid[a,b,c, Weights => {w1, w2, w3}]
  Description
    Text
      This option specifies the weights of the variables in the monoid. The orderings
      by these weight vectors is prepended to the list of orderings provided by the
      @TO[monoid, MonomialOrder]@ option. The value must be a list integers when the
      degree length is one, or a list of lists of integers otherwise.
    Example
      R = QQ[x,y]
      leadTerm matrix {{x+y, x^2+y}}
      R = QQ[x,y, Weights => {1,2}]
      leadTerm matrix {{x+y, x^2+y}}
      R = QQ[x,y, Weights => {1,3}]
      leadTerm matrix {{x+y, x^2+y}}
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra
    leadTerm
    [monoid, MonomialOrder]

Node
  Key
    [monoid, Degrees]
    [monoid, DegreeRank]
  Headline
    specify the degrees of the variables
  Usage
    monoid[x,y,z, Degrees => {d1, d2, d3}]
    monoid[x,y,z, DegreeRank => r]
  Description
    Text
      The @TT "Degrees"@ option specifies the degrees of the variables in the monoid.
      If provided, the value must be a list or sequence with as many entries (after
      @TO2(splice, "splicing")@ and @TO2(flatten, "flattening")@) as there are variables.
      Each degree is an integers or a list of integers. Degrees provided as integers
      will be converted into multidegrees of length 1.
    Example
      QQ[x,y,z, Degrees => {2:1, 2}]
      degrees oo
      QQ[x,y, Degrees => {{1,0}, {0,1}}]
      degrees oo
    Text
      The @TT "DegreeRank"@ option specifies the degree length of the monoid.
      If provided, the value must be an integer. If the @TT "Degrees"@ option is not
      provided, the degrees of the variables are determined similar to this example.
    Example
      QQ[a..f, DegreeRank => 3]
      transpose matrix degrees oo
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra
Node
  Key
    [monoid, DegreeLift]
    [monoid, DegreeMap]
  Headline
    specify maps between degree groups
  Usage
    monoid[x,y, DegreeMap => p, DegreeLift => q]
  Description
    Text
      The @TT "DegreeMap"@ option speficies the degree map, particularly when @TT "Join => false"@
      is given. The degree map is a (linear) function from the multidegrees of the
      (future) coefficient ring to the multidegrees of the monoid ring (polynomial ring)
      made from it with the monoid created here, to be used in determining homogeneity
      and in determining degrees in tensor products. The default is the @TO identity@.

      If a degree map is provided, it will be used in computing tensor products.
    Example
      A = QQ[x];
      B = A[y,
	  Join => false,
	  DegreeMap => x -> 7*x]
      B.FlatMonoid
      degrees A^{-1,-2}
      degrees(A^{-1,-2} ** B)
    Text
      For certain applications, such as lifting matrices, a degree lift function can
      be provided using the @TT "DegreeLift"@ option. The degree lift is a (partial)
      inverse of the degree map, giving an error when lifting is not possible. If the
      degree map is the identity, then by default the identity map will be provided.
      -- TODO: check this:
    Example
      B = A[y,
	  Join => false,
	  DegreeMap => x -> 7*x,
	  DegreeLift => x -> apply(x, d -> d // 7)]
      m = matrix {{x_B}}
      degrees m
      lift(m, A)
      degrees oo
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, Heft]
  Headline
    specify a heft vector
  Usage
    monoid[a,b, Heft => {...}]
  Description
    Text
      The @TT "Heft"@ option can be used to provide a @TO2("heft vectors", "heft vector")@
      as a list of integers such that the dot product with the degree of each variable
      will be positive. This is used as a computational aid in certain routines.
      If no value for this option is specified, one will be computed automatically if
      possible, hence there is no need to provide one unless the time spent computing
      one is onerous; if no heft vector exists, certain computations will not be supported,
      and others may take more time.
    Example
      R = ZZ[x,y, Degrees => {-1,-2}, Heft => {-1}]
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, Join]
  Headline
    specify how to handle degrees in the coefficient ring
  Usage
    monoid[a,b,c, Join => P]
  Description
    Text
      The @TT "Join"@ option specifies whether the degrees in the new monoid ring
      will be obtained by joining the degrees in the coefficient with the degrees
      in the monoid.

      By default, (multi)degrees are concatenated when forming polynomial rings over
      polynomial rings, as can be seen by examining the corresponding flattened monoid,
      which displays information about all of the variables.
    Example
      QQ[x][y]
      oo.FlatMonoid
      QQ[x][y][z]
      oo.FlatMonoid
    Text
      That behavior can be overridden with the @TO [monoid, Join]@ option.
    Example
      QQ[x][y, Join => false]
      oo.FlatMonoid
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, MonomialOrder]
  Headline
    specify the monomial ordering
  Usage
    monoid[a,b,c, MonomialOrder => {...}]
  Description
    Text
      The @TT "MonomialOrder"@ option specifies the monomial ordering, see @TO "MonomialOrder"@.
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    MonomialOrder
    "monomial orderings"
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, MonomialSize]
  Headline
    specify the bit-length of monomial exponents in the ring
  Usage
    monoid[a,b,c, MonomialSize => n]
  Description
    Text
      The @TT "MonomialSize"@ option specifies the minimum number of bits to be used
      for storing each exponent in a monomial. The exponents are stored as signed
      binary numbers, so @TT "n"@ bits allows an exponent as large as $2^{n-1}-1$.
      Useful values are 8, 16, and 32.

Node
  Key
    [monoid, SkewCommutative]
  Headline
    specify Skew commuting variables in the ring
  Usage
    monoid[e_0..e_3, SkewCommutative => true]
    monoid[u,v,x,y,  SkewCommutative => {u,v}]
  Description
    Text
      The @TT "SkewCommutative"@ option specifies which variables will skew-commute
      when the monoid is used to create a ring. The value @TT "true"@ indicates that
      all of the variables skew-commute. Otherwise, the value of the option may be a
      list of symbols or indices corresponding to the skew-commuting variables.
    Example
      ZZ/101[e_0..e_3,       SkewCommutative => true]
      (e_0+e_1+e_2+e_3)^2
      ZZ/101[x,y,vars(0..4), SkewCommutative => vars(0..4)]
      c*b*a*d
      ZZ/101[x,y,vars(0..4), SkewCommutative => {2..6}]
      c*b*a*d
    Example
      R = ZZ[x,y,z, SkewCommutative => {x,y}]
      x*y
      y*x
      x*z-z*x
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    [monoid, WeylAlgebra]
  Headline
    specify differential operators in the ring
  Usage
    monoid[x,dx,y,dy,   WeylAlgebra => {x => dx, y => dy}]
    monoid[x,dx,y,dy,h, WeylAlgebra => {x => dx, y => dy, h}]
  Description
    Text
      The @TT "WeylAlgebra"@ option may be used to provide a list of variables
      and their corresponding differential operators in a Weyl algebra. For instance,
      an option of the form @TT "x => dx"@ specifies that @TT "dx"@ plays the role of
      the derivative with respect to @TT "x"@ in the resulting Weyl algebra.
    Example
      R = ZZ/101[x,dx,y,dy, WeylAlgebra => {x => dx, y => dy}]
      dx*x
      dx*x^10
      dx*y^10
    Text
      Note that only when the monoid is used to create a polynomial ring the Weyl
      algebra variables take effect. There are multiple acceptable ways to populate
      this option, but the differential variable always has to come to the right
      of the corresponding algebra variable.
    Example
--      QQ[a,b,x,dx,y,dy, WeylAlgebra => { {2,   3}, {4,   5} }]
--      QQ[a,b,x,dx,y,dy, WeylAlgebra => { {2 => 3}, {4 => 5} }]
      QQ[a,b,x,dx,y,dy, WeylAlgebra => {  x => dx,  y => dy }]
      QQ[a,b,x,dx,y,dy, WeylAlgebra => { (x, y) => (dx, dy) }]
    Text
      Lastly, if a single variable is provided at the end, it is used as a homogenizing
      variable.
    Example
      QQ[x,dx,y,dy,h, WeylAlgebra => { x => dx, y => dy, h }]
      dx*x^2
      dx*x*y
      dx*h^2
      isHomogeneous(dx*(x^2+x*y+h^2))
    Text
      This option may also be used when creating a new ring from an existing ring,
      creating a tensor product ring, or symmetric algebra.
  SeeAlso
    monoid
    newRing
    (tensor, Ring, Ring)
    symmetricAlgebra

Node
  Key
    (monoid, Ring)
  Usage
    monoid R
  Inputs
    R:Ring
  Outputs
    :Monoid
      the monoid of monomials in the polynomial ring @TT "R"@
  Description
    Text
      If @TT "R"@ is a quotient ring of a polynomial ring @TT "S"@, then the monoid of
      @TT "S"@ is returned.
    Example
      R = QQ[a..d, Weights=>{1,2,3,4}]
      M = monoid R
      use M
      class a
///
