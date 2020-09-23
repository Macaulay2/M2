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
Version => "1.01",
Date => "3/22/2019, 2019",
Authors => {
     {Name => "Erin Bela",
     Email => "ebela@nd.edu"
     },
     {Name => "Alberto F. Boix",
     Email => "albertof.boix@gmail.com"
     },
     {Name => "Juliette Bruce",
     Email => "juliette.bruce@math.wisc.edu",
     HomePage => "https://juliettebruce.github.io/"
     },
     {Name => "Drew Ellingson",
     Email => "drewtell@umich.edu"
     },
     {Name => "Daniel Hernández",
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
     Email => "mmastro@okstate.edu",
     HomePage => "https://mnmastro.github.io/"
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
     HomePage => "https://www.knox.edu/academics/majors-and-minors/mathematics/faculty/teixeira-pedro"
     },
     {Name=> "Emily Witt",
     Email => "witt@ku.edu",
     HomePage => "https://witt.faculty.ku.edu"
     }
},
Headline => "singularities in positive characteristic",
Keywords => {"Commutative Algebra"},
AuxiliaryFiles=>true,
PackageExports=>{"Depth"},
Certification => {
     "journal name" => "The Journal of Software for Algebra and Geometry",
     "journal URI" => "http://j-sag.org/",
     "article title" => "The TestIdeals package for Macaulay2",
     "acceptance date" => "19 July 2019",
     "published article URI" => "https://msp.org/jsag/2019/9-2/p01.xhtml",
     "published article DOI" => "10.2140/jsag.2019.9.89",
     "published code URI" => "https://msp.org/jsag/2019/9-2/jsag-v9-n2-x01-TestIdeals.zip",
     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/TestIdeals.m2",
     "release at publication" => "fa98bdf2587243ef3e7e3042cac7cd9fc90fb6fc",	    -- git commit number in hex
     "version at publication" => "1.01",
     "volume number" => "9",
     "volume URI" => "https://msp.org/jsag/2019/9-2/"
     }
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
    "frobenius",
    "frobeniusPower",
    "FrobeniusPowerStrategy",
    "Naive",
    "Safe",
    "frobeniusPreimage",

-- parameterTestIdeal.m2
    "AssumeCM", --an option for function, if true, then the function will do less work.
    "AssumeReduced", --an option telling functions to assume a ring is reduced.
    "AssumeNormal", --an option telling functions to assume a ring is normal.
    "AssumeDomain", --an option telling functions to assume a ring is a domain.
    "canonicalIdeal", --Karl (still needs more tests / documentation), this is based on Moty's old code.
    "frobeniusTraceOnCanonicalModule", --Karl (this is Moty's find u function, but it returns a list if Macaulay2 doesn't identify 1 element).
    "isCohenMacaulay", --Karl (added recently, if anyone has ideas to improve this...)
    "isFRational", --Karl (added recently).
    "AtOrigin", --an option for isCohenMacaulay, isFRational, etc.
    "testModule", --Karl (this subsumes a bunch of older functions)
    "parameterTestIdeal",

-- Finjective.m2
    "FPureModule", --produces the non-F-injective module, ie the submodule of the canonical module
    "descendIdeal",
    "isFInjective",
    "CanonicalStrategy", --how to check F-injectivity on the canonical module (Ext or Katzman)
    "Katzman", --an option for CanonicalStrategy
    "CanonicalIdeal", --an option for passing the canonical ideal
    "GeneratorList", --an option for passing a list of u-generators (in Katzman's notation) for the canonical ideal or a canonical-like ideal
    "CurrentRing",--an option for passing a ring to work with

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
