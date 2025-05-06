newPackage(
    "Complexes",
    Version => "0.999995",
    Date => "1 May 2023",
    Authors => {
        {   Name => "Gregory G. Smith", 
            Email => "ggsmith@mast.queensu.ca", 
            HomePage => "http://www.mast.queensu.ca/~ggsmith"
            },
        {   Name => "Mike Stillman", 
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"
            }},
    Headline => "beta testing new version of chain complexes",
    Keywords => {"Homological Algebra"},
    PackageImports => { "LLLBases" },
    PackageExports => { "Truncations" },
    AuxiliaryFiles => true
    )

export {
    "component",
    -- types
    "Complex",
    "ComplexMap",
    "GradedModule" => "Complex",
    "GradedModuleMap" => "ComplexMap",
    -- functions/methods
    "augmentationMap",
    "canonicalMap",
    "canonicalTruncation",
    "complex",
    "concentration",
    "connectingMap",
    "connectingExtMap",
    "connectingTorMap",
    "constantStrand",
    "cylinder",
    "epicResolutionMap",
    "freeResolution",
    "homotopyMap",
    "horseshoeResolution",
    "koszulComplex",
    "longExactSequence",
    "isComplexMorphism",
    "isFree", -- TODO: move to Core, use for freemodules too
    "isQuasiIsomorphism",
    "isNullHomotopic",
    "isNullHomotopyOf",
    "isShortExactSequence",
    "liftMapAlongQuasiIsomorphism",
--    "minimalBetti",
    "minimizingMap",
    "nullHomotopy",
    --"nullhomotopy" => "nullHomotopy",
    "naiveTruncation",
    "randomComplexMap",
    "res" => "freeResolution",
    "resolution" => "freeResolution",
    "resolutionMap",
    "tensorCommutativity",
    "torSymmetry",
    "yonedaExtension",
    "yonedaExtension'",
    "yonedaMap",
    "yonedaMap'",
    "yonedaProduct",
    -- Option names
    "FreeToExact", -- used in nullHomotopy
    "OverField",
    "OverZZ",
    "Homogenization",
    "Nonminimal",
    "Concentration",
    "Cycle",
    "Boundary",
    "InternalDegree",
    "UseTarget"
    }

importFrom_Core {
    "isPackageLoaded",
}

-- keys into the type `Complex`
protect modules

-- These are keys used in the various ResolutionObject's
protect SyzygyList
protect compute
protect SchreyerOrder
protect isComputable

--------------------------------------------------------------------
-- code to be migrated to M2 Core ----------------------------------
--------------------------------------------------------------------
tensorCommutativity = method()
tensorCommutativity(Module, Module) := Matrix => (M,N) -> (
    -- implement the isomorphism M ** N --> N ** M
    MN := M ** N;
    NM := N ** M;
    m := numgens source gens M;
    n := numgens source gens N;
    perm := flatten for i from 0 to m - 1 list
      for j from 0 to n - 1 list (
          -- (i,j) (in M**N) to m*i + j
          -- map to column (j,i) <--> n*j + i
          m*j+i
          );
    FMN := source gens MN;
    f := ((id)_FMN)_perm;
    map(NM, MN, f)
    )

homTensorAdjoint = method()
homTensorAdjoint(Module, Module, Module) := (L, M, N) -> (
    -- returns the natural map: Hom(L ** M, N) --> Hom(L, Hom(M, N))
    -- phi -> (ell |-> (m |-> phi(ell ** m)))
    )

--------------------------------------------------------------------
-- package code ----------------------------------------------------
--------------------------------------------------------------------
load "./Complexes/ChainComplex.m2"
load "./Complexes/FreeResolutions.m2"
load "./Complexes/ChainComplexMap.m2"
load "./Complexes/Tor.m2"
load "./Complexes/Ext.m2"

--------------------------------------------------------------------
-- interface code to legacy types ----------------------------------
--------------------------------------------------------------------
if isPackageLoaded "OldChainComplexes" then
  load "./OldChainComplexes/conversion.m2"

--------------------------------------------------------------------
-- package documentation -------------------------------------------
--------------------------------------------------------------------
beginDocumentation()

undocumented{
    (net, Complex),
    (net, ComplexMap),
    (texMath, Complex),
    (texMath, ComplexMap),
    (expression, ComplexMap),
    (component,Module,Thing),
    component
    }

load "./Complexes/ChainComplexDoc.m2"
load "./Complexes/ChainComplexMapDoc.m2"

--------------------------------------------------------------------
-- documentation for legacy type conversion ------------------------
--------------------------------------------------------------------
if isPackageLoaded "OldChainComplexes" then
  load "./OldChainComplexes/docs/conversion.m2"

--------------------------------------------------------------------
-- package tests ---------------------------------------------------
--------------------------------------------------------------------
load "./Complexes/ChainComplexTests.m2"
load "./Complexes/FreeResolutionTests.m2"

end------------------------------------------------------------

restart
uninstallPackage "Complexes"

restart
installPackage "Complexes"
check "Complexes"
viewHelp Complexes
restart
needsPackage "Complexes"

doc ///
    Key
    Headline
    Usage
    Inputs
    Outputs
    Description
        Text
        Example
    Caveat
    SeeAlso
///


S = ZZ/101[a..d]
K = freeResolution coker vars S
L = K ** K
-- would be nice if these were fast(er):
elapsedTime L**L;
elapsedTime (oo ** K)
elapsedTime (K ** ooo)
