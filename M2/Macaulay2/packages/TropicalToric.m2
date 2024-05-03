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
  UseCachedExampleOutput => true
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
