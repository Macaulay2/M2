document {
     Key => [localCohom,Strategy],
     Headline => "specify strategy for local cohomology",
     "This option together with ", TO "LocStrategy", " determines a strategy for ", 
     TT "localCohom(...Ideal...)", " and ", TT "localCohom(...Ideal, Module...)", ".",
     UL { 
	  {BOLD "Walther", " -- the algorithm of U. Walther that uses Cech complex."},
	  {BOLD "LocStrategy => null", 
	       " -- used only for ", TT "localCohom(...Ideal...)", 
	       ", localizations are done by straitforward computation of 
	       annihilators and b-polynomials as described in [1]."},
	  {BOLD "LocStrategy => OaTaWa", 
	       " -- localizations are done following Oaku-Takayama-Walther method."},
	  {BOLD "LocStrategy => Oaku", 
	       " -- localizations are done following Oaku's algorithm."},
	  {BOLD "OaTa", " -- restriction algorithm is used, 
	       which is due to T. Oaku and N. Takayama [2]"}   
	  },
          --Caveat => {"When WaltherOTW strategy is used the error 'Bad luck!' 
          --may appear. This means your are not a lucky individual...
	  --The glitch is due to the fact that the localizations are iterated 
	  --for this particular strategy; it was resolved for WaltherOaku, 
	  --a strategy that considers everyone lucky."
	  --},
     "For detailed description of the algorithms see",
     UL {
	  {BOLD "[1]", "U. Walther, ", 
	       EM "Algorithmic computation of local cohomology 
	       modules and the local cohomological dimension of algebraic 
	       varieties (JPAA (139), 1999.)"
	       },
	  {BOLD "[2]", "Oaku, Takayama", 
	       EM "Algorithms for D-modules..."
	       }
	  }    	      
     } 

document {
     Key => LocStrategy }
document {
     Key => [localCohom,LocStrategy],
     Headline => "specify localization strategy for local cohomology",
     "See ", TO [localCohom,Strategy]
     }
document {
     Key => Walther,
     Headline => "an option for localCohom=>Strategy",
     "see ", TO "localCohom"
     } 
document {
     Key => OaTa,
     Headline => "an option for localCohom=>Strategy",
     "see ", TO "localCohom"
     } 
document {
     Key => OaTaWa,
     Headline => "an option for localCohom => LocStrategy",
     SeeAlso => "localCohom"
     }

document {
     Key => localCohom,
     Headline => "local cohomology",
     "Local cohomology of a polynomial ring:",
     UL {
	  {TO (localCohom, Ideal)},
	  {TO (localCohom, List, Ideal)},
	  {TO (localCohom, ZZ, Ideal)}
	  },
     "Local cohomology of a holonomic module:",
     UL {
	  {TO (localCohom, Ideal, Module)},
	  {TO (localCohom, ZZ, Ideal, Module)},
	  {TO (localCohom, List, Ideal, Module)}
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }
document {
     Key => (localCohom, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "H = localCohom I", 
     Inputs => {
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       }
	  },
     Outputs => {
	  "H" => {
	       "each entry of ", TT "H", " has an integer key and 
	       contains the cohomology module in the corresponding degree."
	       }
	  },
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom I",
     	  "pruneLocalCohom h"
	  },
     Caveat => {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."},
     SeeAlso => {"pruneLocalCohom"}
     }  

document {
     Key => (localCohom, List, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "localCohom(l,I)",
     Inputs => { "l", "I" },
     Outputs => { { "the local cohomology of ", TT "I", " in the degrees specified by ", EM "l" } },
     "See ", TO (localCohom, Ideal), " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom({1,2}, I)",
     	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, ZZ, Ideal),
     Headline => "local cohomology of a polynomial ring",
     Usage => "localCohom(d,I)",
     Inputs => { "d", "I" },
     Outputs => {{ "the local cohomology of ", TT "I", " in degree ", EM "d" }},
     "See ", TO (localCohom, Ideal), " for the full description.",
     EXAMPLE { 
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom (2,I)",
     	  "Dprune h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "H = localCohom(I,M)", 
     Inputs => {
	  "I" => {
	       "an ideal of ", 
	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
	       },
	  "M" => {
	       "a holonomic module over Weyl algebra ", 
	       EM{"A", SUB "n", "(k)"}
	       }
	  },
     Outputs => {
	  "H" => {
	       "each entry of ", TT "H", " has an integer key and 
	       contains the cohomology module in the corresponding degree."
	       }
	  },
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
     	  "h = localCohom(I, W^1 / ideal{dX,dY,dZ})",
     	  "pruneLocalCohom h"
	  },
     Caveat => {"The modules returned are not simplified, 
     	  use ", TO "pruneLocalCohom", "."},
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, ZZ, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "localCohom(d,I,M)",
     Inputs => { "d", "I", "M" },
     Outputs => {{
	  "the local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degree ", EM "d", ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  }},
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom(2, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => (localCohom, List, Ideal, Module),
     Headline => "local cohomology of a D-module",
     Usage => "localCohom(l,I,M)",
     Inputs => { "l", "I", "M" },
     Outputs => {{
	  "the local cohomology ", 
	  EM {"H", SUB "I", "(M)"}, " in degrees listed in ", EM "l", 
	  ", where ", EM "I", 
	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
	  }},
     "See ", TO "localCohom(Ideal,Module)", " for the full description.",
     EXAMPLE {
	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
     	  "I = ideal (X*(Y-Z), X*Y*Z)",
	  "h = localCohom({1,2}, I, W^1 / ideal{dX,dY,dZ})",
	  "pruneLocalCohom h"
	  },
     SeeAlso => {"pruneLocalCohom"} 
     }

document {
     Key => {(pruneLocalCohom, HashTable), pruneLocalCohom},
     Headline => "prunes local cohomology modules",
     Usage => "pruneLocalCohom H",
     Inputs => {{"the output of ", TO "localCohom"}},
     Outputs => {HashTable},
     "This function applies ", TO "Dprune", " to all the keys of ", TT "H", ".", 
     SeeAlso => {"localCohom", "Dprune"} 
     }


document {
     Key => {(deRhamAll, RingElement), deRhamAll},
     Headline => "deRham complex for the complement of a hypersurface",
     Usage => "deRhamAll f",
     Inputs => {"f"},
     Outputs => {
	  HashTable => {"containing explicit cohomology classes 
	       in the deRham complex for the complement 
	       of the hypersurface ",  EM "{f = 0}", " and
     	       supplementary information"}	       
	  },
     "The routine deRhamAll can be used to compute cup product structures
     as in the paper 'The cup product structure for complements
     of affine varieties' by Walther(2000).",
     PARA{},
     "For a more basic functionality see ", TO "deRham", ".",
     EXAMPLE lines ///
	R = QQ[x,y]
     	f = x^2-y^3 
     	deRhamAll f
	///,
     SeeAlso => {"deRham", "Dlocalize", "Dintegration"}	
} 

document {
     Key => [deRham,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => [deRhamAll,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => {deRham, (deRham, ZZ, RingElement), (deRham, RingElement)},
     Headline => "deRham cohomology groups for the complement of a hypersurface",
     Usage => "M = deRham f, Mi = deRham(i,f)",
     Inputs => {
	  "i" => ZZ,
	  "f" => RingElement
	  },
     Outputs => {
	  "Mi" => Module => {"the i-th deRham cohomology group of the complement 
	  of the hypersurface ",  EM "{f = 0}"},
	  "M" => HashTable => {"containing the entries of the form ", TT "i=>Mi"}  
	  },
     "The algorithm used appears in the paper 'An algorithm for deRham 
     cohomology groups of the complement of an affine variety via D-module
     computation' by Oaku-Takayama(1999).  
     The method is to compute the localization of the polynomial ring 
     by f, then compute the derived integration of the localization.",
     EXAMPLE lines ///
	R = QQ[x,y]
     	f = x^2-y^3 
     	deRham f
	deRham(1,f)
	///,
     SeeAlso => {"deRhamAll", "Dlocalize", "Dintegration"}
     }
document {
     Key => TransferCycles,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => CohomologyGroups,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => PreCycles,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => LocalizeMap,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }
document {
     Key => OmegaRes,
     Headline => "a key in the hashtable created by deRham",
     SeeAlso => "deRham"
     }

document {
     Key => {(paramBpoly, RingElement, String), paramBpoly},
     Headline => "compute the list of all possible Bernstein-Sato polynomials 
     for a polynomial with parametric coefficients",
     Usage => "paramBpoly(f,filename)", 	  
     Inputs => {
     	  "f" => RingElement => {
	       "a polynomial in Weyl algebra ", EM "A_n(Q)"
	       },
	  "filename" => String => {"the base name for the output files"}
	  },
     Outputs => {
	  List => {"all possible Bernstein-Sato polynomials"}
	  },
--     Consequences => { "Creates  the file <filename.tex> that contains the list of 
--     the Bernstein-Sato polynomials and the corresponding constructible sets."
--	  },     
     EXAMPLE lines ///
	  A =  (QQ [a,b,c]) [x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	  paramBpoly(a*x^2 + b*x*y + c*y^2, "quadratic")
	  ///,
     Caveat => {
	  "A finite field should be used in place of ", EM "Q", 
	  " in order to speed up computations. This routine works only
	  on relatively small examples."
	  },
     SeeAlso => {"globalBFunction"}
     }  

document {
     Key => GroundField
     }
document {
     Key => [paramBpoly,GroundField],
     Headline => "characteristic for modular computation"
     }
