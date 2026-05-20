doc ///
Node
  Key
    "elementary arithmetic"
  Description
    Text
      Elementary arithmetic in Macaulay2 includes integer and modular arithmetic,
      factorization, gcd/lcm computations, and finite-field calculations.
    Text
      Selected packages include @TO2("ConwayPolynomials :: ConwayPolynomials", "ConwayPolynomials")@,
      @TO2("Cyclotomic :: Cyclotomic", "Cyclotomic")@,
      @TO2("LLLBases :: LLLBases", "LLLBases")@,
      @TO2("EllipticCurves :: EllipticCurves", "EllipticCurves")@, and
      @TO2("Units :: Units", "Units")@. For a broader catalog, see
      @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "gcd"@, @TO "lcm"@, @TO "factor"@,
      @TO "powermod"@, and @TO "mod"@.
  SeeAlso
    "packages provided with Macaulay2"
    "ConwayPolynomials :: ConwayPolynomials"
    "Cyclotomic :: Cyclotomic"
    "LLLBases :: LLLBases"
    "EllipticCurves :: EllipticCurves"
    "Units :: Units"

Node
  Key
    "linear algebra"
  Description
    Text
      Linear algebra in Macaulay2 includes matrix construction, determinants and
      minors, row reduction, matrix decompositions, kernels and inverses, and
      related multilinear constructions.
    Text
      Selected packages include @TO2("LLLBases :: LLLBases", "LLLBases")@ and
      @TO2("TensorComplexes :: TensorComplexes", "TensorComplexes")@. For a broader
      catalog, see @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "matrix"@, @TO "determinants and minors"@,
      @TO "rank"@, @TO "kernel(Matrix)"@, @TO "inverse(Matrix)"@,
      @TO "transpose"@, @TO "reducedRowEchelonForm"@, @TO "LUdecomposition"@,
      @TO "QRDecomposition"@, @TO "SVD"@, and @TO "smithNormalForm"@.
  SeeAlso
    "packages provided with Macaulay2"
    "LLLBases :: LLLBases"
    "TensorComplexes :: TensorComplexes"

Node
  Key
    "commutative algebra"
  Description
    Text
      Commutative algebra in Macaulay2 centers on ideals, modules, Groebner bases,
      syzygies, Hilbert invariants, elimination, and resolution techniques.
    Text
      Tutorials include @TO2("Tutorials::Tutorial: Modules in Macaulay2", "Tutorial: Modules in Macaulay2")@,
      @TO2("Tutorials::Tutorial: Elementary uses of Gröbner bases", "Tutorial: Elementary uses of Gröbner bases")@,
      and @TO2("Tutorials::Teaching Materials", "Teaching Materials")@.
    Text
      Selected packages include @TO2("IntegralClosure :: IntegralClosure", "IntegralClosure")@,
      @TO2("PrimaryDecomposition :: PrimaryDecomposition", "PrimaryDecomposition")@,
      @TO2("ReesAlgebra :: ReesAlgebra", "ReesAlgebra")@,
      @TO2("MinimalPrimes :: MinimalPrimes", "MinimalPrimes")@,
      @TO2("Depth :: Depth", "Depth")@,
      @TO2("Elimination :: Elimination", "Elimination")@,
      @TO2("Saturation :: Saturation", "Saturation")@,
      @TO2("Regularity :: Regularity", "Regularity")@,
      @TO2("LocalRings :: LocalRings", "LocalRings")@,
      @TO2("MonomialAlgebras :: MonomialAlgebras", "MonomialAlgebras")@,
      @TO2("NoetherNormalization :: NoetherNormalization", "NoetherNormalization")@, and
      @TO2("TangentCone :: TangentCone", "TangentCone")@. For a broader catalog, see
      @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "Gröbner bases"@, @TO "normal forms"@,
      @TO "elimination of variables"@, @TO "hilbertFunction"@,
      @TO "hilbertSeries"@, @TO "hilbertPolynomial"@,
      @TO "Complexes :: freeResolution"@, @TO "MinimalPrimes :: radical"@,
      @TO "MinimalPrimes :: minimalPrimes"@,
      @TO "PrimaryDecomposition :: primaryDecomposition"@, and
      @TO "regularity"@.
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

Node
  Key
    "homological algebra"
  Description
    Text
      Homological algebra in Macaulay2 covers complexes, maps of complexes,
      homology and cohomology functors, and derived constructions such as Tor and
      Ext.
    Text
      A good starting tutorial is
      @TO2("Tutorials::Tutorial: Modules in Macaulay2", "Tutorial: Modules in Macaulay2")@.
    Text
      Selected packages include @TO2("Complexes :: Complexes", "Complexes")@,
      @TO2("OldChainComplexes :: OldChainComplexes", "OldChainComplexes")@,
      @TO2("ChainComplexExtras :: ChainComplexExtras", "ChainComplexExtras")@,
      @TO2("DGAlgebras :: DGAlgebras", "DGAlgebras")@,
      @TO2("TorAlgebra :: TorAlgebra", "TorAlgebra")@,
      @TO2("SchurFunctors :: SchurFunctors", "SchurFunctors")@,
      @TO2("TensorComplexes :: TensorComplexes", "TensorComplexes")@,
      @TO2("MCMApproximations :: MCMApproximations", "MCMApproximations")@, and
      @TO2("KustinMiller :: KustinMiller", "KustinMiller")@. For a broader catalog, see
      @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "Tor"@, @TO "Ext"@, @TO "HH"@,
      @TO "isExact"@, @TO "kernel"@, @TO "cokernel"@, and @TO "image"@.
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

Node
  Key
    "algebraic geometry"
  Description
    Text
      Algebraic geometry in Macaulay2 includes computations with affine and
      projective varieties, toric and Schubert geometry, singularities,
      characteristic classes, and rational points.
    Text
      Tutorials include @TO2("Tutorials::Tutorial: Divisors", "Tutorial: Divisors")@,
      @TO2("Tutorials::Tutorial: Canonical Embeddings of Plane Curves and Gonality", "Tutorial: Canonical Embeddings of Plane Curves and Gonality")@,
      and @TO2("Tutorials::Tutorial: Fano varieties", "Tutorial: Fano varieties")@.
    Text
      Selected packages include @TO2("Varieties :: Varieties", "Varieties")@,
      @TO2("Schubert2 :: Schubert2", "Schubert2")@,
      @TO2("NormalToricVarieties :: NormalToricVarieties", "NormalToricVarieties")@,
      @TO2("CharacteristicClasses :: CharacteristicClasses", "CharacteristicClasses")@,
      @TO2("HyperplaneArrangements :: HyperplaneArrangements", "HyperplaneArrangements")@,
      @TO2("Points :: Points", "Points")@,
      @TO2("RationalPoints :: RationalPoints", "RationalPoints")@,
      @TO2("PushForward :: PushForward", "PushForward")@, and
      @TO2("ToricVectorBundles :: ToricVectorBundles", "ToricVectorBundles")@.
      For a broader catalog, see
      @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "singularLocus"@, @TO "Grassmannian"@,
      @TO "Schubert"@, @TO "Fano"@, @TO "isNormal"@,
      @TO "isSmooth"@, and @TO "genus"@.
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

Node
  Key
    "Representation Theory"
  Description
    Text
      Representation theory in Macaulay2 currently centers on symmetric
      functions, Schur and Weyl functors, Lie algebras and their
      representations, highest-weight methods, group actions, invariant
      theory, and related character computations.
    Text
      Selected packages include @TO2("SchurRings :: SchurRings", "SchurRings")@,
      @TO2("SchurFunctors :: SchurFunctors", "SchurFunctors")@,
      @TO2("PieriMaps::PieriMaps", "PieriMaps")@,
      @TO2("WeylGroups :: WeylGroups", "WeylGroups")@,
      @TO2("LieAlgebraRepresentations :: LieAlgebraRepresentations", "LieAlgebraRepresentations")@,
      @TO2("GradedLieAlgebras :: GradedLieAlgebras", "GradedLieAlgebras")@,
      @TO2("HighestWeights :: HighestWeights", "HighestWeights")@,
      @TO2("InvariantRing :: InvariantRing", "InvariantRing")@, and
      @TO2("BettiCharacters :: BettiCharacters", "BettiCharacters")@. For a broader
      catalog, see @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "SchurFunctors :: schur"@,
      @TO "SchurFunctors :: schurModule"@,
      @TO "SchurRings :: plethysm"@,
      @TO "SchurRings :: internalProduct"@,
      @TO "LieAlgebraRepresentations :: simpleLieAlgebra"@,
      @TO "HighestWeights :: setWeights"@,
      @TO2("HighestWeights :: HighestWeights", "highest-weight decompositions")@,
      @TO2("InvariantRing :: InvariantRing", "group actions and invariant rings")@,
      @TO2("BettiCharacters :: BettiCharacters", "characters on resolutions and graded modules")@,
      @TO "WeylGroups :: rootSystem"@,
      @TO "WeylGroups :: dynkinType"@, and
      @TO "WeylGroups :: cartanMatrix"@.
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

Node
  Key
    "combinatorics"
  Description
    Text
      Combinatorics in Macaulay2 spans graphs, posets, simplicial complexes,
      polyhedra, lattice polytopes, and combinatorial commutative algebra.
    Text
      Selected packages include @TO2("Graphs :: Graphs", "Graphs")@,
      @TO2("Posets :: Posets", "Posets")@,
      @TO2("Matroids :: Matroids", "Matroids")@,
      @TO2("EdgeIdeals :: EdgeIdeals", "EdgeIdeals")@,
      @TO2("SimplicialComplexes :: SimplicialComplexes", "SimplicialComplexes")@,
      @TO2("Polyhedra :: Polyhedra", "Polyhedra")@,
      @TO2("LatticePolytopes :: LatticePolytopes", "LatticePolytopes")@,
      @TO2("StatePolytope :: StatePolytope", "StatePolytope")@,
      @TO2("Nauty :: Nauty", "Nauty")@,
      @TO2("NautyGraphs :: NautyGraphs", "NautyGraphs")@,
      @TO2("FourTiTwo :: FourTiTwo", "FourTiTwo")@,
      @TO2("Normaliz :: Normaliz", "Normaliz")@, and
      @TO2("SimplicialDecomposability :: SimplicialDecomposability", "SimplicialDecomposability")@.
      For a broader catalog, see
      @TO2("packages provided with Macaulay2", "packages provided with Macaulay2")@.
    Text
      Core topics include @TO "partitions"@, @TO "compositions"@,
      @TO "binomial"@, @TO "Set"@, @TO "cone"@, @TO "rays"@,
      @TO "normalCone"@, and @TO "multidegree"@.
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
///
