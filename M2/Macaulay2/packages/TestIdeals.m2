--*************************************************
--*************************************************
--This is the revised (and cleaned up) version
--of the TestIdeals.m2 package, which has been under
--continuous development since the Wake Forest
--Macaulay2 workshop of August 2012.
--TestIdeals.m2 and FThresholds.m2 broke off from
--the original package, called PosChar.m2
--*************************************************
--*************************************************

--version history
--0.2 first public version
--0.2a added AssumeDomain options to isFRegular and isFRational
--1.0 first complete version
--protect QGorensteinIndex;
--protect MaxCartierIndex;
--protect DepthOfSearch;
--protect FrobeniusPowerStrategy;

newPackage( "TestIdeals",
Version => "1.0",
Date => "8/17/2018, 2018",
Authors => {
     {Name => "Erin Bela",
     Email => "ebela@nd.edu"
     },
     {Name => "Alberto F. Boix",
     Email => "alberto.fernandezb@upf.edu"
     },
     {Name => "Juliette Bruce",
     Email => "juliette.bruce@math.wisc.edu",
     HomePage => "https://juliettebruce.github.io/"
     },
     {Name => "Drew Ellingson",
     Email => "drewtell@umich.edu"
     },
     {Name => "Daniel Hernandez",
     Email => "hernandez@ku.edu",
     HomePage => "https://hernandez.faculty.ku.edu"
     },
     {Name => "Zhibek Kadyrsizova",
     Email => "zhikadyr@umich.edu"
     },
     {Name => "Mordechai Katzman",
     Email => "m.katzman@sheffield.ac.uk",
     HomePage => "http://www.katzman.staff.shef.ac.uk/"
     },
     {Name => "Sara Malec",
     Email => "malec@hood.edu"
     },
     {Name => "Matthew Mastroeni",
     Email => "mastroe2@illinois.edu"
     },
     {Name => "Maral Mostafazadehfard",
     Email => "maralmostafazadehfard@gmail.com"
     },
     {Name => "Marcus Robinson",
     Email => "robinson@math.utah.edu"
     },
     {Name => "Karl Schwede",
     Email => "schwede@math.utah.edu",
     HomePage => "http://math.utah.edu/~schwede/"
     },
     {Name => "Dan Smolkin",
     Email => "smolkin@math.utah.edu",
     HomePage => "http://cohenmacaulay.life"
     },
     {Name => "Pedro Teixeira",
     Email => "pteixeir@knox.edu",
     HomePage => "http://www.knox.edu/academics/faculty/teixeira-pedro.html"
     },
     {Name=> "Emily Witt",
     Email => "witt@ku.edu",
     HomePage => "https://witt.faculty.ku.edu"
     }
},
Headline => "A package for calculations of singularities in positive characteristic",
DebuggingMode => true,
Reload => true,
AuxiliaryFiles=>true,
PackageExports=>{"Depth"}
)

export{
--BasicFunctions (BasicFunctions.m2)
    "adicExpansion",
    "adicDigit",
    "adicTruncation",
    "decomposeFraction",
    "floorLog",
    "multiplicativeOrder",
    "NoZeroC", --option to force certain behavior from a function

--ethRootFunctions (EthRoots.m2)
    "ascendIdeal",
    "ascendModule",
    "AscentCount",
    "FrobeniusRootStrategy",
    "frobeniusRoot",
    "MonomialBasis",
    "Substitution",

--Frobenius Powers (frobeniusPowers.m2)
    "fastExponentiation",
    "frobenius",
    "frobeniusPower",
    "FrobeniusPowerStrategy",
    "Naive",
    "Safe",

-- parameterTestIdeal.m2
    "AssumeCM", --an option for function, if true, then the function will do less work.
    "AssumeReduced", --an option telling functions to assume a ring is reduced.
    "AssumeNormal", --an option telling functions to assume a ring is normal.
    "AssumeDomain", --an option telling functions to assume a ring is a domain.
    "canonicalIdeal", --Karl (still needs more tests / documentation), this is based on Moty's old code.
    "frobeniusTraceOnCanonicalModule", --Karl (this is Moty's find u function, but it returns a list if Macaulay2 doesn't identify 1 element).
    "isCohenMacaulay", --Karl (added recently, if anyone has ideas to improve this...)
    "isFRational", --Karl (added recently).
    "IsLocal", --an option for isCohenMacaulay, isFRational, etc.
    "testModule", --Karl (this subsumes a bunch of older functions)
    "parameterTestIdeal",

-- Finjective.m2
    "HSLGModule", --produces the non-F-injective module, ie the submodule of the canonical module
    "isFInjective",
    "CanonicalStrategy", --how to check F-injectivity on the canonical module (Ext or Katzman)
    "Katzman", --an option for CanonicalStrategy

-- testIdeals.m2
    "QGorensteinGenerator", --Karl (this finds y such that I^{[p^e]} : I = (y) + I^{[p^e]}, if it exists) **Documented**
    "testElement", --Karl (my students Marcus and Dan did some improvements on this recently, it doesn't compute the whole Jacobian, it just looks at random minors until it finds a good one, it can be much much faster) **Documented**
    "MaxCartierIndex", --the cartier index limfindAllCompatibleIdealsit in the test ideal method
    "testIdeal", --Karl (the new version)
    "QGorensteinIndex", --if you already know the Q-Gorenstein index, you can pass it
    "DepthOfSearch",
    "isFRegular",
    "isFPure",
    "compatibleIdeals" ---MK
}


load "./TestIdeals/BasicFunctions.m2"

load "./TestIdeals/EthRoots.m2"

load "./TestIdeals/frobeniusPowers.m2"

load "./TestIdeals/compatiblySplit.m2"

load "./TestIdeals/parameterTestIdeal.m2"

load "./TestIdeals/Finjective.m2"

load "./TestIdeals/testIdeals.m2"

load "./TestIdeals/DivisorPatch.m2"

beginDocumentation()

load "./TestIdeals/BasicFunctionsDoc.m2"

load "./TestIdeals/frobeniusPowersDoc.m2"

load "./TestIdeals/TestIdealDoc.m2"

load "./TestIdeals/EthRootsDoc.m2"

load "./TestIdeals/compatiblySplitDoc.m2"

load "./TestIdeals/testIdealsDoc.m2"

load "./TestIdeals/parameterTestIdealDoc.m2"

load "./TestIdeals/FinjectiveDoc.m2"

-- TESTS

load "./TestIdeals/BasicFunctionsTest.m2"

load "./TestIdeals/EthRootsTest.m2"

load "./TestIdeals/frobeniusPowersTest.m2"

load "./TestIdeals/ParameterTestIdealTest.m2"

load "./TestIdeals/CompatiblySplitTest.m2"

load "./TestIdeals/testIdealTest.m2"

load "./TestIdeals/FinjectiveTest.m2"

end
