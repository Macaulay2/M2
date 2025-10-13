newPackage(
    "Tableaux",
    Version => "0.5",
    Date => "July 22, 2025",
    Authors => {
	{Name => "John Graf", Email => "jrgraf@alumni.ncsu.edu", HomePage => "https://j-graf.github.io/"}},
    Headline => "constructing skew tableaux",
    Keywords => {"Combinatorics"},
    AuxiliaryFiles => true,
    DebuggingMode => false,
    PackageImports => {"Permutations"}--,
    --Configuration => {"Convention" => "English", "TexPackage" => "aTableau"}
    )

export {"SkewTableau", "skewTableau",
        "youngDiagram", "ferrersDiagram", "drawInnerShape",
        "skewShape", "standardize", "rowEntries", "columnEntries", "rowRange", "columnRange",
        "isWeaklyDecreasing", "isNonnegative",
        "toPosition", "toIndex", "positionList", "applyEntries", "applyPositions",
        "verticalConcatenate", "shift", "unshift",
        "boxContent", "hookLength",
        }

export {"YoungTableau", "youngTableau",
        "shape"}

export {"allSemistandardTableaux", "numSemistandardTableaux"}


    
-- Implementation of class SkewTableau

load "Tableaux/SkewTableaux.m2"



-- Implementation of subclass YoungTableau

load "Tableaux/YoungTableaux.m2"



-- TODO: Implementation of subclass Tabloid

-- load "Tableaux/Tabloids.m2"



-- Algorithms involving tableaux

load "Tableaux/algorithms.m2"



-- Documentation

beginDocumentation()

load "Tableaux/documentation.m2"



-- Tests

load "Tableaux/tests.m2"



end--
