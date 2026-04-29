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
    Headline => "A Macaulay2 package for Witt vectors",
    Keywords => {"Witt Vectors"},
    DebuggingMode => false,
    AuxiliaryFiles => true
    )

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
"WittRing",
"wittSub",
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
"truncation",
"makeCoefficientFieldPrime",
}

protect tuple
protect wittRings

load "./WittVectors/WittConstructor.m2"
load "./WittVectors/Kernels.m2"
load "./WittVectors/WittConversion.m2"
load "./WittVectors/FrobeniusLiftings.m2"
load "./WittVectors/QuasiFSplittings.m2"
load "./WittVectors/tests.m2"

beginDocumentation()

load "./WittVectors/Documentation.m2"
