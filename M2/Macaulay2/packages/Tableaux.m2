newPackage(
    "Tableaux",
    Version => "0.6",
    Date => "January 13, 2026",
    Authors => {
	{Name => "John Graf", Email => "jrgraf@udel.edu", HomePage => "https://j-graf.github.io/"}},
    Headline => "constructing Young tableaux",
    Keywords => {"Combinatorics"},
    AuxiliaryFiles => true,
    DebuggingMode => false,
    PackageImports => {"Permutations"}--,
    --Configuration => {"Convention" => "English", "TexPackage" => "aTableau"}
    )

export {"YoungTableau", "youngTableau",
        "youngDiagram", "ferrersDiagram", "drawInnerShape", "verticalNet", "horizontalNet",
        "skewShape", "shape", "outerShape", "innerShape",
        "standardize", "rowEntries", "columnEntries", "rowRange", "columnRange",
        "isSkew", "isWeaklyDecreasing", "isNonnegative", "isStandard", "isSemistandard",
        "toPosition", "toIndex", "positionList", "applyEntries", "applyPositions",
        "verticalConcatenate", "shift", "unshift",
        "boxContent", "hookLength", "rowStabilizer", "columnStabilizer", "isCorner",
        "readingWord"
        }

export {"MutableYoungTableau", "mutableYoungTableau"}

export {"Tabloid", "tabloid",
        "representative"}

export {"allSemistandardTableaux", "numSemistandardTableaux", "randomSemistandardTableau",
        "allStandardTableaux", "numStandardTableaux", "randomStandardTableau",
        "allTabloids", "numTabloids", "randomTabloid",
        "allSubPartitions", "toPartitionChain"}


    
-- Implementation of class YoungTableau

load "Tableaux/YoungTableaux.m2"



-- Implementation of subclass MutableYoungTableau

load "Tableaux/MutableYoungTableaux.m2"



-- TODO: Implementation of subclass Tabloid

load "Tableaux/Tabloids.m2"



-- Algorithms involving tableaux

load "Tableaux/algorithms.m2"



-- Documentation

beginDocumentation()

load "Tableaux/documentation.m2"



-- Tests

load "Tableaux/tests.m2"



end--
