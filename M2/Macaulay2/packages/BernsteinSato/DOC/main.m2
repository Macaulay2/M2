doc ///
Node
  Key
    BernsteinSato
  Headline
    algorithms for b-functions, local cohomology, and intersection cohomology
  Description
    Code
     HEADER3 "B-functions:",
     UL {
	  TO {"bFunction", " -- b-function"}, 
	  TO {"globalBFunction", " -- global b-function"},
	  TO {"globalB", " -- global b-function and b-operator"},
	  TO {"globalBoperator", " -- global b-operator"},
	  TO {"generalB", " -- generalized Bernstein-Sato polynomial"},
	  TO {"localBFunction", " -- local b-function"},
	  TO {"paramBpoly", " -- Bernstein-Sato polynomials of a
	       polynomial with parametric coefficients"},
	  TO {"factorBFunction", " -- factors a univariate polynomial"},
	  TO {"bFunctionRoots", " -- gets roots of a b-function"},
	  TO {"getIntRoots", " -- gets integer roots of a b-function"},
	  {TO {"AnnFs"},  " -- annihilator ideal of ", EM "f", SUP "s" },
	  {TO {"AnnIFs"}, " -- annihilator ideal  of ", EM "f", SUP "s", 
	       " for an arbitrary D-module"}
	  },

     HEADER3 "Resolutions and Functors:",
     UL{TO {"Dresolution", " -- resolutions"}, 
	  TO {"Dlocalize", " -- localization"}, 
     	  TO {"WeylClosure", " -- Weyl closure"},
	  TO {"Ddual", " -- holonomic dual"}, 
	  TO {"Drestriction", " -- derived restriction"},
	  TO {"Dintegration", " -- derived integration"},
	  TO {"DHom", " -- homomorphisms between holonomic D-modules"},
	  TO {"DExt", " -- Ext between holonomic D-modules"},
	  TO {"PolyExt", 
	       " -- Ext between a holonomic D-module and a polynomial ring"},
	  TO {"RatExt", 
	       " -- rational Ext"}
	  },     

     HEADER3 "Applications:",
     UL{
	  TO {"localCohom", "-- local cohomology"},
	  TO {"deRham", " -- deRham cohomology"},
	  TO {"PolySols", " -- polynomial solutions of finite rank systems"},
	  TO {"RatSols", " -- rational solutions of finite rank systems"},
	  TO {"diffOps", " -- differential operators on affine varieties"},
	  {TO "populateCechComplexCC", ", ", TO "pruneCechComplexCC", 
	       " -- characteristic cycles of local cohomology"},
	  TO {"logCohomology", " -- logarithmic cohomology groups"},
	  TO {"lct", "-- log canonical threshold"},
	  {TO "multiplierIdeal", ", ", TO "isInMultiplierIdeal", ", ", TO "jumpingCoefficients", " -- multiplier ideals"},
	  TO "hasRationalSing"
	  }
///
