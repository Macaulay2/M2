doc ///
Node
  Key
    "elementary arithmetic"
  Subnodes
    mod
    plus
    minus
    difference
    times
    power
    powermod
    lcm
    gcd
    gcdCoefficients
    factor

Node
  Key
    "commutative algebra"
  Subnodes
    "Gröbner bases"
    "normal forms"
    -- Mike wanted this: TO "Hilbert functions"
    "elimination of variables"
    -- Mike wanted this: TO "syzygies"
    -- Mike wanted this: TO "saturation"
    -- Mike wanted this: TO "fibers of a map between varieties"
    -- Mike wanted this: TO "solving systems of polynomial equations"
    --
    @TO "IntegralClosure :: IntegralClosure"@
    @TO "PrimaryDecomposition :: PrimaryDecomposition"@
    --
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    syz
    koszul
    eagonNorthcott
    regularity
    presentation
    "OldChainComplexes :: resolution"
    quotient
    quotient'
    quotientRemainder
    quotientRemainder'
    remainder
    remainder'
    pseudoRemainder
    rank
    dim
    codim
    pdim
    depth -- contains link to package Depth
    height -- TODO
    length
    complete
    trim
    prune
    numgens
    mingens
    minimize
    minimalPresentation
    tensor
    directSum
    directProduct
    monomialCurveIdeal
    isBorel
    isCommutative
    isAffineRing

Node
  Key
    "homological algebra"
  Subnodes
    kernel
    cokernel
    image
    coimage
    preimage
    pullback
    pushout
    extend
    "OldChainComplexes :: nullhomotopy"
    isExact
    isInjective
    isSurjective
    isIsomorphism
    inverse
    ScriptedFunctor
    id
    HH
    Tor
    Ext

Node
  Key
    "algebraic geometry"
  Subnodes
    singularLocus
    Grassmannian
    Schubert
    Fano
    isNormal
    isSmooth
    isVeryAmple
    randomKRationalPoint
    chi
    euler
    eulers
    genus
    genera

Node
  Key
    "combinatorics"
  Subnodes
    binomial
    compositions
    inversePermutation
    partitions
    random
    Set
    rays
    cone
    normalCone
    multidegree
///

document {
     Key => "A first course in commutative algebra",
     "This section includes tutorials showing how to do
     basic commutative algebra constructions in Macaulay2.
     This section is being written by Mike Stillman, for use
     with his Fall 2005 course: Math 634, Commutative algebra,
     at Cornell University.  This course covers basic commutative
     algebra, at the level of Atiyah-Macdonald, and Greuel-Pfister.",
     PARA{},
     "Macaulay2 examples corresponding to the Singular examples in the
     book by Greuel-Pfister may also be found here.",
     Subnodes => {
	  TO "Elementary uses of Groebner bases I. Math 634 Fall 2005",
	  TO "M2SingularBook"
	  }
     }
