newPackage(
  "TropicalToric",
	Version => "1.0",
	Date => "May 2022",
	Authors => {
   		{
        Name => "Alessio Borzì",
        Email => "Alessio.Borzi@warwick.ac.uk",
        HomePage=>"https://alessioborzi.github.io"
      }
  },
	Headline => "A package on tropical methods for toric intersection theory",
	Configuration => {},
  PackageExports => {
    "NormalToricVarieties",
    "Tropical",
    "Package$gfanInterface"
  },
  PackageImports => {
    "FourierMotzkin"
  },
	DebuggingMode => true,
	AuxiliaryFiles => true,
	CacheExampleOutput => true,
  OptionalComponentsPresent => true,
  UseCachedExampleOutput => true,
  Keywords => {"Tropical Geometry"},
  Certification => {
      "journal name" => "Journal of Software for Algebra and Geometry",
      "journal URI" => "https://msp.org/jsag/",
      "article title" => "Tropical computations for toric intersection theory in Macaulay2",
      "acceptance date" => "2023-09-14",
      "published article URI" => "https://msp.org/jsag/2024/14-1/p04.xhtml",
      "published article DOI" => "10.2140/jsag.2024.14.19",
      "published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x04-TropicalToric.zip",
      "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/TropicalToric.m2",
      "release at publication" => "9ef0931eee637fc1fd9f377b35e91f4c14309a7c",
      "version at publication" => "1.0",
      "volume number" => "14",
      "volume URI" => "https://msp.org/jsag/2024/14-1/"
      }
)

export{
  --types
  "ToricCycle",

  --functions / methods
  "toricCycle",
  "makeTransverse",
  "isTransverse",
  "degCycle",
  "toricDivisorFromCycle",
  "refineMultiplicity",
  "pushforwardMultiplicity",
  "poincareMatrix",
  "poincareDuality",
  "classFromTropical",
  "classWonderfulCompactification",
  "torusIntersection",
  "classFromTropicalCox",
  "polymakeConeContains",

  --symbols
  "PoincareMatrix"
}

protect maxRayList;
protect cycleSupport;
protect PicardBases;
protect RaysMatrix;
protect RaysMatrixRank;

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------

load "TropicalToric/ToricCycle.m2";
load "TropicalToric/ToricPullback.m2";
load "TropicalToric/TropicalToricCode.m2";
load "TropicalToric/polymakeContains.m2";

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------

beginDocumentation()

load "TropicalToric/ToricCycleDoc.m2";
load "TropicalToric/TropicalToricDoc.m2";
load "TropicalToric/polymakeContainsDoc.m2";

------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------

load "TropicalToric/ToricCycleTest.m2";
load "TropicalToric/TropicalToricTest.m2";

end
