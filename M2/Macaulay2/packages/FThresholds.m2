newPackage( "FThresholds",
Version => "1.0",
Date => "August 17th, 2018",
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
     Email => "zhibek.kadyrsizova@nu.edu.kz"
     },
     {Name => "Moty Katzman",
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
     HomePage => "http://dan.smolk.in"
     },
     {Name => "Pedro Teixeira",
     Email => "pteixeir@knox.edu",
     HomePage => "http://www.knox.edu/academics/faculty/teixeira-pedro.html"
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
    "Attempts",
--F-thresholds computations (MainFunctions.m2)
    "BinaryRecursive",
    "compareFPT",
    "ComputePreviousNus",
    "ContainmentTest",
    "criticalExponentApproximation",
    "fpt",
    "fptApproximation",
    "FRegularityCheck",
    "FrobeniusPower",
    "FrobeniusRoot",
    "ftApproximation",
    "isFJumpingExponent",
    "isFPT",
    "MaxChecks",
    "mu",
    "muList",
    "nu",
    "nuList",
    "Search",
    "StandardPower",
    "UseColonIdeals",
    "UseFSignature",
    "UseSpecialAlgorithms"
}


--loadPackage("TestIdeals", LoadDocumentation => true, Reload=>true);


--*************************************************

load "./FThresholds/DivisorPatch.m2" --some helper functions

load "./FThresholds/BasicFunctions.m2"

load "./FThresholds/MainFunctions.m2"

load "./FThresholds/SpecialFThresholds.m2"

beginDocumentation()

load "./FThresholds/FThresholdsDoc.m2"

load "./FThresholds/MainFunctionsDoc.m2"

load "./FThresholds/SpecialFThresholdsDoc.m2"

-- TESTS

load "./FThresholds/SpecialFThresholdsTest.m2"
