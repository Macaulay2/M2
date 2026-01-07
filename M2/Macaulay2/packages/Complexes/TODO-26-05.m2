What do we need to do?  Using Claude to help figure out this.

First: I removed: TateOnProducts, VirtualResolutions, LinearTruncations from the =distributed-packages file (temporarily).

The true leaf packages (importing OldChainComplexes but with no dependents) would be the remaining 21:
x  1. AdjunctionForSurfaces
x  2. AlgebraicSplines
x  3. BeginningMacaulay2
x  4. Benchmark
x  5. Bruns
  6. ChainComplexOperations
  7. HigherCIOperators
  8. HighestWeights
  9. Kronecker
  10. ModuleDeformations
  11. MonomialIntegerPrograms
  12. MultiGradedRationalMap
  13. NonminimalComplexes
  14. Parametrization
  15. RandomComplexes
  16. RandomCurvesOverVerySmallFiniteFields
  17. RandomMonomialIdeals
  18. ResolutionsOfStanleyReisnerRings
  19. TorAlgebra
  20. ToricTopology
  21. TSpreadIdeals

These should be easy to change, I hope.

  Transitive dependents that are now leaves:

  | Package                | Imports (OldChainComplexes-related)            |
  |------------------------|------------------------------------------------|
  | ConnectionMatrices     | Dmodules                                       |
  | Hadamard               | Points                                         |
  | HomotopyLieAlgebra     | DGAlgebras                                     |
  | MatrixFactorizations   | TensorComplexes                                |
  | MixedMultiplicity      | WeilDivisors, Depth                            |
  | MultiplierIdeals       | HyperplaneArrangements                         |
  | NonPrincipalTestIdeals | TestIdeals, FrobeniusThresholds, WeilDivisors  |
  | NumericalSemigroups    | FastMinors                                     |
  | PathSignatures         | NCAlgebra                                      |
  | QuaternaryQuartics     | GroebnerStrata                                 |
  | RandomCurves           | RandomSpaceCurves, RandomGenus14Curves         |
  | SectionRing            | WeilDivisors                                   |
  | SpectralSequences      | ChainComplexExtras                             |
  | TerraciniLoci          | CorrespondenceScrolls, FastMinors              |
  | Triplets               | BoijSoederberg                                 |
  | Valuations             | LocalRings                                     |
  | VirtualResolutions     | TateOnProducts, LinearTruncations, SpaceCurves |

To summarize the dependency structure we've mapped out:

 Layer 1 (21 true leaves) - Import OldChainComplexes, not imported by
   any OldChainComplexes-using package:
      - These are the best candidates to migrate first since no other
        OldChainComplexes-dependent package relies on them

  Layer 2 (17 packages) - Would become leaves after Layer 1 is migrated:
      - ConnectionMatrices, Hadamard, HomotopyLieAlgebra,
        MatrixFactorizations, MixedMultiplicity, MultiplierIdeals,
        NonPrincipalTestIdeals, NumericalSemigroups, PathSignatures,
        QuaternaryQuartics, RandomCurves, SectionRing,
        SpectralSequences, TerraciniLoci, Triplets, Valuations,
        VirtualResolutions

  Core packages - Deeply embedded, imported by many OldChainComplexes-using packages:
  - Depth, SVDComplexes, TateOnProducts, LinearTruncations, WeylAlgebras, TestIdeals, FastMinors, etc.

  This gives you a roadmap for incremental migration from
  OldChainComplexes to Complexes, starting with the leaf packages and
  working inward.


Packages that import Depth via PackageImports or PackageExports:

  | Package                      | Import Type    |
  |------------------------------|----------------|
  | GeometricDecomposability     | PackageImports |
  | MixedMultiplicity            | PackageImports |
  | Quasidegrees                 | PackageImports |
  | QuadraticIdealExamplesByRoos | PackageExports |
  | RandomMonomialIdeals         | PackageImports |
  | ResidualIntersections        | PackageExports |
  | SCMAlgebras                  | PackageExports |
  | TestIdeals                   | PackageExports |
  | VirtualResolutions           | PackageImports |

  Note: SymbolicPowers uses needsPackage "Depth" but not via PackageImports/PackageExports.

  Of these 9 packages:

      - RandomMonomialIdeals is one of the 21 leaf packages (imports
        OldChainComplexes, not imported by any OldChainComplexes-using
        package)

      - TestIdeals and VirtualResolutions are in the OldChainComplexes
        dependency tree

      - The others (GeometricDecomposability, MixedMultiplicity,
        Quasidegrees, QuadraticIdealExamplesByRoos,
        ResidualIntersections, SCMAlgebras) don't appear to use
        OldChainComplexes directly
  
