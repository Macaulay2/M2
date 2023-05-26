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
    Headline => "F-thresholds",
    Keywords => {"Commutative Algebra"},
    AuxiliaryFiles => true,
    PackageImports => {"MinimalPrimes"},
    PackageExports => {"TestIdeals"},
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "The FrobeniusThresholds package for Macaulay2",
	 "acceptance date" => "11 September 2020",
	 "published article URI" => "https://msp.org/jsag/2021/11-1/p04.xhtml",
	 "published article DOI" => "10.2140/jsag.2021.11.25",
	 "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x04-FrobeniusThresholds.m2",
	 "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/....m2",
	 "release at publication" => "eb4b9b426546bee5d4997afcc2b353421a30719d",	    -- git commit number in hex
	 "version at publication" => "2.1",
	 "volume number" => "11",
	 "volume URI" => "https://msp.org/jsag/2021/11-1/"
	 }
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
