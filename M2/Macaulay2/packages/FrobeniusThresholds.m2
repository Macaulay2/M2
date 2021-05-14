newPackage( "FrobeniusThresholds",
Version => "2.1",
Date => "June 15th, 2020",
Authors => {
     {Name => "Juliette Bruce",
     Email => "juliette.bruce@math.wisc.edu",
     HomePage => "https://juliettebruce.github.io/"
     },
     {Name => "Daniel Hernández",
     Email => "hernandez@ku.edu",
     HomePage => "https://hernandez.faculty.ku.edu"
     },
     {Name => "Karl Schwede",
      Email => "schwede@math.utah.edu",
     HomePage => "http://math.utah.edu/~schwede/"
     },
     {Name => "Dan Smolkin",
     Email => "smolkin@math.utah.edu",
     HomePage => "http://dan.smolk.in"
     },
     {Name => "Pedro Teixeira",
     Email => "pteixeir@knox.edu",
     HomePage => "https://www.knox.edu/academics/majors-and-minors/mathematics/faculty/teixeira-pedro"
     },
     {Name => "Emily Witt",
     Email => "witt@ku.edu",
     HomePage => "https://witt.faculty.ku.edu"
     }
},
Headline => "A package for calculations of F-thresholds",
DebuggingMode => true,
Reload => true,
AuxiliaryFiles => true,
PackageExports => {"TestIdeals"}
)

export{
    "Bounds",
    "compareFPT",
    "ContainmentTest",
    "FinalAttempt",
    "fpt",
    "FrobeniusPower",
    "FrobeniusRoot",
    "GlobalFrobeniusRoot",
    "GuessStrategy",
    "isFJumpingExponent",
    "isFPT",
    "isSimpleNormalCrossing",
    "frobeniusNu",
    "ReturnList",
    "Search",
    "StandardPower",
    "UseSpecialAlgorithms"
}

--*************************************************

load "./FrobeniusThresholds/DivisorPatch.m2" --some helper functions

load "./FrobeniusThresholds/BasicFunctions.m2"

load "./FrobeniusThresholds/MainFunctions.m2"

load "./FrobeniusThresholds/SpecialFThresholds.m2"

-- DOCUMENTATION

beginDocumentation()

load "./FrobeniusThresholds/FThresholdsDoc.m2"

load "./FrobeniusThresholds/MainFunctionsDoc.m2"

load "./FrobeniusThresholds/SpecialFThresholdsDoc.m2"

-- TESTS

load "./FrobeniusThresholds/MainFunctionsTest.m2"

load "./FrobeniusThresholds/SpecialFThresholdsTest.m2"
