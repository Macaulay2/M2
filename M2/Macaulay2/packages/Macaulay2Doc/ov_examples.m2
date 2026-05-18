doc ///
Node
  Key
    "elementary arithmetic"
  Description
    Text
      Elementary arithmetic in Macaulay2 includes integer and modular arithmetic,
      factorization, gcd/lcm computations, and finite-field calculations.
    Tree
      :Selected Packages
        > @TOH "ConwayPolynomials :: ConwayPolynomials"@
        > @TOH "Cyclotomic :: Cyclotomic"@
        > @TOH "LLLBases :: LLLBases"@
        > @TOH "EllipticCurves :: EllipticCurves"@
        > @TOH "Units :: Units"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "gcd"
        > "lcm"
        > "factor"
        > "powermod"
        > "mod"
  SeeAlso
    "packages provided with Macaulay2"
    "ConwayPolynomials :: ConwayPolynomials"
    "Cyclotomic :: Cyclotomic"
    "LLLBases :: LLLBases"
    "EllipticCurves :: EllipticCurves"
    "Units :: Units"
  Subnodes
    mod
    plus
    minus
    difference
    times
    power
    powermod
    (sqrt, ZZ, ZZ)
    lcm
    gcd
    gcdCoefficients
    factor

Node
  Key
    "linear algebra"
  Description
    Text
      Linear algebra in Macaulay2 includes matrix construction, determinants and
      minors, row reduction, matrix decompositions, kernels and inverses, and
      related multilinear constructions.
    Tree
      :Selected Packages
        > @TOH "LLLBases :: LLLBases"@
        > @TOH "TensorComplexes :: TensorComplexes"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "matrix"
        > "determinants and minors"
        > "rank"
        > @TOH "kernel(Matrix)"@
        > @TOH "inverse(Matrix)"@
        > "transpose"
        > "reducedRowEchelonForm"
        > "LUdecomposition"
        > "QRDecomposition"
        > "SVD"
        > "smithNormalForm"
  SeeAlso
    "packages provided with Macaulay2"
    "LLLBases :: LLLBases"
    "TensorComplexes :: TensorComplexes"
  Subnodes
    @TO "LLLBases :: LLLBases"@
    @TO "TensorComplexes :: TensorComplexes"@
    matrix
    "determinants and minors"
    rank
    "kernel(Matrix)"
    "inverse(Matrix)"
    transpose
    reducedRowEchelonForm
    LUdecomposition
    QRDecomposition
    SVD
    smithNormalForm

Node
  Key
    "commutative algebra"
  Description
    Text
      Commutative algebra in Macaulay2 centers on ideals, modules, Groebner bases,
      syzygies, Hilbert invariants, elimination, and resolution techniques.
    Tree
      :Tutorials
        > "Tutorial: Modules in Macaulay2"
        > "Tutorial: Elementary uses of Gröbner bases"
        > "Teaching Materials"
      :Selected Packages
        > @TOH "IntegralClosure :: IntegralClosure"@
        > @TOH "PrimaryDecomposition :: PrimaryDecomposition"@
        > @TOH "ReesAlgebra :: ReesAlgebra"@
        > @TOH "MinimalPrimes :: MinimalPrimes"@
        > @TOH "Depth :: Depth"@
        > @TOH "Elimination :: Elimination"@
        > @TOH "Saturation :: Saturation"@
        > @TOH "Regularity :: Regularity"@
        > @TOH "LocalRings :: LocalRings"@
        > @TOH "MonomialAlgebras :: MonomialAlgebras"@
        > @TOH "NoetherNormalization :: NoetherNormalization"@
        > @TOH "TangentCone :: TangentCone"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "Gröbner bases"
        > "normal forms"
        > "elimination of variables"
        > "hilbertFunction"
        > "hilbertSeries"
        > "hilbertPolynomial"
        > @TOH "Complexes :: freeResolution"@
        > @TOH "MinimalPrimes :: radical"@
        > @TOH "MinimalPrimes :: minimalPrimes"@
        > @TOH "PrimaryDecomposition :: primaryDecomposition"@
        > "regularity"
    Text
      The reference links below are retained for direct access to lower-level commands.
  SeeAlso
    "packages provided with Macaulay2"
    "IntegralClosure :: IntegralClosure"
    "PrimaryDecomposition :: PrimaryDecomposition"
    "ReesAlgebra :: ReesAlgebra"
    "MinimalPrimes :: MinimalPrimes"
    "Depth :: Depth"
    "Elimination :: Elimination"
    "Saturation :: Saturation"
    "Regularity :: Regularity"
    "LocalRings :: LocalRings"
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
  Description
    Text
      Homological algebra in Macaulay2 covers complexes, maps of complexes,
      homology and cohomology functors, and derived constructions such as Tor and
      Ext.
    Tree
      :Tutorials
        > "Tutorial: Modules in Macaulay2"
      :Selected Packages
        > @TOH "Complexes :: Complexes"@
        > @TOH "OldChainComplexes :: OldChainComplexes"@
        > @TOH "ChainComplexExtras :: ChainComplexExtras"@
        > @TOH "DGAlgebras :: DGAlgebras"@
        > @TOH "TorAlgebra :: TorAlgebra"@
        > @TOH "SchurFunctors :: SchurFunctors"@
        > @TOH "TensorComplexes :: TensorComplexes"@
        > @TOH "MCMApproximations :: MCMApproximations"@
        > @TOH "KustinMiller :: KustinMiller"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "Tor"
        > "Ext"
        > "HH"
        > "isExact"
        > "kernel"
        > "cokernel"
        > "image"
  SeeAlso
    "packages provided with Macaulay2"
    "Complexes :: Complexes"
    "OldChainComplexes :: OldChainComplexes"
    "ChainComplexExtras :: ChainComplexExtras"
    "DGAlgebras :: DGAlgebras"
    "TorAlgebra :: TorAlgebra"
    "SchurFunctors :: SchurFunctors"
    "TensorComplexes :: TensorComplexes"
    "MCMApproximations :: MCMApproximations"
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
  Description
    Text
      Algebraic geometry in Macaulay2 includes computations with affine and
      projective varieties, toric and Schubert geometry, singularities,
      characteristic classes, and rational points.
    Tree
      :Tutorials
        > "Tutorial: Divisors"
        > "Tutorial: Canonical Embeddings of Plane Curves and Gonality"
        > "Tutorial: Fano varieties"
      :Selected Packages
        > @TOH "Varieties :: Varieties"@
        > @TOH "Schubert2 :: Schubert2"@
        > @TOH "NormalToricVarieties :: NormalToricVarieties"@
        > @TOH "CharacteristicClasses :: CharacteristicClasses"@
        > @TOH "HyperplaneArrangements :: HyperplaneArrangements"@
        > @TOH "Points :: Points"@
        > @TOH "RationalPoints :: RationalPoints"@
        > @TOH "PushForward :: PushForward"@
        > @TOH "ToricVectorBundles :: ToricVectorBundles"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "singularLocus"
        > "Grassmannian"
        > "Schubert"
        > "Fano"
        > "isNormal"
        > "isSmooth"
        > "genus"
    Text
      The reference links below remain available for direct access to core geometry commands.
  SeeAlso
    "packages provided with Macaulay2"
    "Varieties :: Varieties"
    "Schubert2 :: Schubert2"
    "NormalToricVarieties :: NormalToricVarieties"
    "CharacteristicClasses :: CharacteristicClasses"
    "HyperplaneArrangements :: HyperplaneArrangements"
    "RationalPoints :: RationalPoints"
    "ToricVectorBundles :: ToricVectorBundles"
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
    "Representation Theory"
  Description
    Text
      Representation theory in Macaulay2 currently centers on symmetric
      functions, Schur and Weyl functors, Lie algebras and their
      representations, highest-weight methods, group actions, invariant
      theory, and related character computations.
    Tree
      :Selected Packages
        > @TOH "SchurRings :: SchurRings"@
        > @TOH "SchurFunctors :: SchurFunctors"@
        > @TOH "PieriMaps::PieriMaps"@
        > @TOH "WeylGroups :: WeylGroups"@
        > @TOH "LieAlgebraRepresentations :: LieAlgebraRepresentations"@
        > @TOH "GradedLieAlgebras :: GradedLieAlgebras"@
        > @TOH "HighestWeights :: HighestWeights"@
        > @TOH "InvariantRing :: InvariantRing"@
        > @TOH "BettiCharacters :: BettiCharacters"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > @TOH "SchurFunctors :: schur"@
        > @TOH "SchurFunctors :: schurModule"@
        > @TOH "SchurRings :: plethysm"@
        > @TOH "SchurRings :: internalProduct"@
        > @TOH "LieAlgebraRepresentations :: simpleLieAlgebra"@
        > @TOH "HighestWeights :: setWeights"@
        > @TO2("HighestWeights :: HighestWeights", "highest-weight decompositions")@
        > @TO2("InvariantRing :: InvariantRing", "group actions and invariant rings")@
        > @TO2("BettiCharacters :: BettiCharacters", "characters on resolutions and graded modules")@
        > @TOH "WeylGroups :: rootSystem"@
        > @TOH "WeylGroups :: dynkinType"@
        > @TOH "WeylGroups :: cartanMatrix"@
    Text
      The reference links below remain available for direct access to package
      entry points and core constructions.
  SeeAlso
    "packages provided with Macaulay2"
    "SchurRings :: SchurRings"
    "SchurFunctors :: SchurFunctors"
    "PieriMaps::PieriMaps"
    "WeylGroups :: WeylGroups"
    "LieAlgebraRepresentations :: LieAlgebraRepresentations"
    "GradedLieAlgebras :: GradedLieAlgebras"
    "HighestWeights :: HighestWeights"
    "InvariantRing :: InvariantRing"
    "BettiCharacters :: BettiCharacters"
  Subnodes
    @TO "SchurRings :: SchurRings"@
    @TO "SchurFunctors :: SchurFunctors"@
    @TO "PieriMaps::PieriMaps"@
    @TO "WeylGroups :: WeylGroups"@
    @TO "LieAlgebraRepresentations :: LieAlgebraRepresentations"@
    @TO "GradedLieAlgebras :: GradedLieAlgebras"@
    @TO "HighestWeights :: HighestWeights"@
    @TO "InvariantRing :: InvariantRing"@
    @TO "BettiCharacters :: BettiCharacters"@
    "SchurFunctors :: schur"
    "SchurFunctors :: schurModule"
    "SchurRings :: plethysm"
    "SchurRings :: internalProduct"
    "LieAlgebraRepresentations :: simpleLieAlgebra"
    "HighestWeights :: setWeights"
    "WeylGroups :: rootSystem"
    "WeylGroups :: dynkinType"
    "WeylGroups :: cartanMatrix"

Node
  Key
    "combinatorics"
  Description
    Text
      Combinatorics in Macaulay2 spans graphs, posets, simplicial complexes,
      polyhedra, lattice polytopes, and combinatorial commutative algebra.
    Tree
      :Selected Packages
        > @TOH "Graphs :: Graphs"@
        > @TOH "Posets :: Posets"@
        > @TOH "Matroids :: Matroids"@
        > @TOH "EdgeIdeals :: EdgeIdeals"@
        > @TOH "SimplicialComplexes :: SimplicialComplexes"@
        > @TOH "Polyhedra :: Polyhedra"@
        > @TOH "LatticePolytopes :: LatticePolytopes"@
        > @TOH "StatePolytope :: StatePolytope"@
        > @TOH "Nauty :: Nauty"@
        > @TOH "NautyGraphs :: NautyGraphs"@
        > @TOH "FourTiTwo :: FourTiTwo"@
        > @TOH "Normaliz :: Normaliz"@
        > @TOH "SimplicialDecomposability :: SimplicialDecomposability"@
        > @TO2("packages provided with Macaulay2", "browse packages by subject area")@
      :Core topics
        > "partitions"
        > "compositions"
        > "binomial"
        > "Set"
        > "cone"
        > "rays"
        > "normalCone"
        > "multidegree"
  SeeAlso
    "packages provided with Macaulay2"
    "Graphs :: Graphs"
    "Posets :: Posets"
    "EdgeIdeals :: EdgeIdeals"
    "SimplicialComplexes :: SimplicialComplexes"
    "Polyhedra :: Polyhedra"
    "LatticePolytopes :: LatticePolytopes"
    "Nauty :: Nauty"
    "FourTiTwo :: FourTiTwo"
  Subnodes
    binomial
    compositions
    inversePermutation
    partitions
    random
    shuffle
    Set
    rays
    cone
    normalCone
    multidegree
///

document {
     Key => {"Teaching Materials", "A first course in commutative algebra"},
     "This section collects teaching materials and introductory tutorials
     for basic commutative algebra in Macaulay2, at roughly the level of
     Atiyah-Macdonald and Greuel-Pfister.",
     PARA{},
     "Use these materials when you want worked examples that connect
     commutative-algebra topics to first Macaulay2 commands, including
     Gröbner bases, ideals and modules, Hilbert invariants, elimination,
     and resolutions.",
     PARA{},
     "Macaulay2 examples corresponding to examples in the Greuel-Pfister
     text may also be found here.",
     Subnodes => {
	  TO2("Elementary uses of Groebner bases I", "Elementary uses of Groebner bases"),
	  TO "M2SingularBook"
	  }
     }
