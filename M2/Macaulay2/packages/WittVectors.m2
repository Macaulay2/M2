newPackage(
    "WittVectors",
    Version => "0.01",
    Date => "April 29, 2026",
    Authors => {
    	{Name => "Anne Fayolle", Email => "anne.fayolle@utah.edu", HomePage => "https://annefayolle.github.io/"},
	{Name => "Abhay Goel", Email => "u1421426@utah.edu", HomePage => "https://abhayg11.github.io/"},
	{Name => "Devlin Mallory", Email => "dmallory@bcamath.org", HomePage => "https://devlin-mallory.github.io/"},
	{Name => "Eamon Quinlan-Gallego", Email => "eamon.quinlan@utah.edu", HomePage => "https://eamonqg.github.io/"},
    	{Name => "Teppei Takamatsu", Email => "teppeitakamatsu.math@gmail.com", HomePage => "https://sites.google.com/view/teppei-takamatsu/home"}
    },
    PackageImports => {
        "TestIdeals",
        "Polyhedra",
        "PushForward",
        "Elimination",
    },
    Headline => "computations with Witt vectors, Frobenius lifts, and quasi-F-splittings",
    Keywords => {"Commutative Algebra"},
    DebuggingMode => false,
    AuxiliaryFiles => true
    )

-- TODO: the symbols `overring` (line below), `overringMap`, `wittGenerators`,
-- and `WittOverring` (exported in WittConversion.m2:10) are used only as
-- internal cache-key symbols (e.g. W.cache.overringMap).  They have no
-- doc nodes and no direct TEST coverage.  Following the pattern used for
-- `tuple`, `wittRings`, and `wittSub`, consider moving them to the
-- `protect` block below and dropping them from the export list.
export{
"witt",
"wittOverring",
"WittRingElement", 
"wittTupleToOverring",
"wittTupleToRing",
"wittRingToTuple",
"wittOverringToTuple",
"verschiebung",
"wittOverringIdeal",
"wittRingIdeal",
"unWitt",
"overringMap",
"WittPolynomialRing",
"WittRingMap",
"baseMap",
"WittQuotientRing",
"explicit",
"wittLength",
"WittIdeal",
"wittIdeal",
"wittGenerators",
"MaxHeight",
"Nontrivial",
"PerturbationTerm",
"findFrobeniusLiftConstraints",
"findFrobeniusLift",
"createEquations",
"fSplittingHeight",
"overring",
"wittFrobenius",
"makeCoefficientFieldPrime",
}

protect tuple
protect wittRings
protect wittSub

load "./WittVectors/WittConstructor.m2"
load "./WittVectors/Kernels.m2"
load "./WittVectors/WittConversion.m2"
load "./WittVectors/FrobeniusLiftings.m2"
load "./WittVectors/QuasiFSplittings.m2"
load "./WittVectors/tests.m2"

beginDocumentation()

load "./WittVectors/Documentation.m2"
