document {
     Key => "mathematical tutorials",
     "In this section we present some tutorials that aim to introduce
     the user to some mathematical ways of using Macaulay2.  The tutorials
     are relatively independent of each other, and each one introduces the use
     of some features of Macaulay2 in a slow and leisurely way, assuming the
     reader is already familiar with the mathematical concepts involved.  
     ", TO "David Eisenbud", " joins us as a co-author of these tutorials.",
     Subnodes => {
	  TO "Tutorial: Elementary uses of Groebner bases",
	  TO "Tutorial: Canonical Embeddings of Plane Curves and Gonality",
	  TO "Tutorial: Fano varieties",
	  TO "Tutorial: Divisors",
	  }
     }

doc ///
Node
  Key
    "arithmetic functions"
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
    "commutative algebra functions"
  Subnodes
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    syz
    koszul
    eagonNorthcott
    regularity
    presentation
    resolution
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
    trim
    prune
    numgens
    mingens
    minimize
    minimalPresentation
    tensor
    directSum
    monomialCurveIdeal
    syzygyScheme
    isBorel
    isCommutative
    isAffineRing

Node
  Key
    "homological algebra functions"
  Subnodes
    kernel
    cokernel
    image
    coimage
    preimage
    pullback
    pushout
    extend
    nullhomotopy
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
    "algebraic geometry functions"
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
///

document {
     Key => "basic commutative algebra",
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
	  TO "modules in Macaulay2",
	  TO "M2SingularBook"
	  }
     }
