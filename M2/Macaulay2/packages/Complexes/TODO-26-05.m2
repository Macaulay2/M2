What do we need to do?  Using Claude to help figure out this.

Things we can do to match old behavior:
    1. change koszul to be koszulComplex
    2. use the shiftDegree function, added in commit d8629f8ed4b04
    3. allow res(Fastnonminimal => true)
    
First: I removed: TateOnProducts, VirtualResolutions, LinearTruncations from the =distributed-packages file (temporarily).

The true leaf packages (importing OldChainComplexes but with no dependents) would be the remaining 21:
x  1. AdjunctionForSurfaces
x  2. AlgebraicSplines
x  3. BeginningMacaulay2
x  4. Benchmark
x  5. Bruns
  6. ChainComplexOperations -- reverseComplex is causing a problem.
y 7. HigherCIOperators -- uses chainComplex, koszul, ChainComplex
y 8. HighestWeights -- seems ok now.
  9. Kronecker -- implements things for GradedModuleMap, uses chainComplex.
y 10. ModuleDeformations (y: not yet checked in)
y 11. MonomialIntegerPrograms -- probably ok. But hard to tell until we fix code.
y 12. MultiGradedRationalMap
  13. NonminimalComplexes
y 14. Parametrization -- but doesn't really work without Maple...  So can't tell if it is functional.
  15. RandomComplexes
y 16. RandomCurvesOverVerySmallFiniteFields -- BUT: no tests!
y 17. RandomMonomialIdeals (but: imports Depth too, which could be setting package back to OldChainComplexes...)
y 18. ResolutionsOfStanleyReisnerRings
  19. TorAlgebra -- doesn't work yet, need to get LocalRings switched over.
y 20. ToricTopology
y 21. TSpreadIdeals

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

At this point, the following still import or export OldChainComplexes      

 | Package                 | Import Type    |
  |-------------------------|----------------|
y | BernsteinSato           | PackageExports |
e | BoijSoederberg          | PackageExports | error: isPure is being used...
  | ChainComplexExtras      | PackageExports |
y | CorrespondenceScrolls   | PackageImports |
  | Depth                   | PackageImports |
y | DGAlgebras              | PackageExports | needs koszulComplex alot, but then works.
  | GroebnerStrata          | PackageExports |
y | HigherCIOperators       | PackageExports |
y | HyperplaneArrangements  | PackageImports |
y | Kronecker               | PackageExports |
y | LexIdeals               | PackageImports |
  | LinearTruncations       | PackageExports |
  | LocalRings              | PackageImports |
y | Matroids                | PackageImports |
  | MonomialIntegerPrograms | PackageImports |
e | NCAlgebra               | PackageExports | has error, since I don't have bergman...
  | NonminimalComplexes     | PackageExports |
y | Points                  | PackageImports |
  | PruneComplex            | PackageExports |
  | RandomComplexes         | PackageExports |
y | RandomGenus14Curves     | PackageImports |
y | RandomSpaceCurves       | PackageImports |
y | SpaceCurves             | PackageImports | (NO tests)
  | SVDComplexes            | PackageExports |
  | TateOnProducts          | PackageExports |
y | TensorComplexes         | PackageExports | need eagonNorthcott!
y | TestIdeals              | PackageImports |
y | WeylAlgebras            | PackageExports |    

w = working on it.
y = package installs and checks
x = checked in
e = has error in installing or checking.    

Packages still importing OldChainComplexes directly.
 | Package                 | Import Type    |
  |-------------------------|----------------|
y | BernsteinSato           | PackageExports |
  | ChainComplexExtras      | PackageExports |
y | Depth                   | PackageImports |
  | GroebnerStrata          | PackageExports |
y | Kronecker               | PackageExports | implements alot of GradedModule, GradedModuleMap functions BUT only for M2 ver <= 1.1 !!
  | LinearTruncations       | PackageExports |
  | LocalRings              | PackageImports |
  | MonomialIntegerPrograms | PackageImports | probably ok, but can't test MonomialIntegerPrograms
  | NonminimalComplexes     | PackageExports |
  | PruneComplex            | PackageExports |
  | RandomComplexes         | PackageExports |
  | SVDComplexes            | PackageExports |
  | TateOnProducts          | PackageExports |
y | TensorComplexes         | PackageExports | needs eagonNorthcott
y | WeylAlgebras            | PackageExports | done

ChainComplexExtras:
export "isMinimalChainComplex"

-- Want this functionality?
isMinimalChainComplex = C -> (
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    T :=true;
    scan(toList(1+min C..max C),
	i-> if 0 != red(C.dd_i) then T = false);
    T
    )

-- try out betti(C, Minimize=>true)
R = ZZ/101[a..d]
I = inverseSystem random(5, R)
M = comodule I
C = freeResolution(M, Strategy => Nonminimal)
betti(C, Minimize => true)
debug Complexes
peek C.cache
C.cache.Module.cache.ResolutionObject.RawComputation
peek oo


betti ChainComplex := opts -> C -> (
    R := ring C;
    if C.?Resolution and degreeLength R === 1 and heft R === {1} then return betti(C.Resolution, opts);
    if opts.Minimize then error "Minimize=>true is currently only supported for res(...,FastNonminimal=>true)";
    complete C;
    heftfn := heftfun heftvec(opts.Weights, heft R);
    new BettiTally from flatten apply(
	select(pairs C, (i,F) -> class i === ZZ),
	(i,F) -> (
	    if not isFreeModule F then error("betti: expected module at spot ", toString i, " in chain complex to be free");
	    apply(pairs tally degrees F, (d,n) -> (i,d,heftfn d) => n))))

minimalBettiOfResolution = method(Options => options betti)
minimalBettiOfResolution Complex := BettiTally => C -> (
    -- this version works only for rings of degree length 1
    -- currently if opts.Minimize is true, then an error is given
    -- unless Strategy => Nonminimal option was given for the free resolution.
    rawcomp := try (if C.cache.Nonminimal then C.cache.Module.cache.ResolutionObject.RawComputation) else null;
    if rawcomp === null then error "expected non-minimal resolution computation";
    B := rawBetti(rawcomp.RawComputation, if opts.Minimize then 4 else 0); -- the raw version takes no weight option
    betti(B, Weights => heftvec(opts.Weights, heft ring X))
    )

-- Strategy => 4.  Also want strategy 5...!
-- Using QQ.  
R = QQ[a..d]
I = ideal(1/2*a,3*b,5*c,7*d)
I = ideal(a,b,c,d)
M = comodule I
gbTrace=3
C = freeResolution(M, Strategy => Nonminimal)
-- all the non lead terms are of the form 1/0...
C.dd
