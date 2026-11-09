--- old format (commented out) ---
-- document {
--      Key => [localCohom,Strategy],
--      Headline => "specify strategy for local cohomology",
--      "There are two main strategies, Walther and OaTa. If the user selects Walther, which is the default, then ", TO "LocStrategy", " determines the localization strategy for ",
--      TT "localCohom(...Ideal...)", " and ", TT "localCohom(...Ideal, Module...)", ".",
--      UL {
-- 	  {BOLD "Walther", " -- the algorithm of U. Walther that uses Cech complex."},
-- 	  UL {
-- 	       {BOLD "LocStrategy => null",
-- 		    " -- used only for ", TT "localCohom(...Ideal...)",
-- 		    ", localizations are done by straightforward computation of
-- 		    annihilators and b-polynomials as described in [1]."},
-- 	       {BOLD "LocStrategy => OaTaWa",
-- 		    " -- localizations are done following Oaku-Takayama-Walther method [2]."},
-- 	       {BOLD "LocStrategy => Oaku",
-- 		    " -- localizations are done following Oaku's algorithm."},
-- 	  },
-- 	  {BOLD "OaTa", " -- restriction from the graph embedding is used,
-- 	       which is due to T. Oaku and N. Takayama [3]. See ", TO "Drestriction", "."}
-- 	  },
--           Caveat => {"localCohom(...Ideal, Module...) with the default strategy computes presentations for all the terms in the Cech complex regardless of the requested homological degrees. All strategies use the given generators of the ideal; the user is advised to call ", TO "mingens", " before calling localCohom."},
--      "For detailed description of the algorithms see",
--      UL {
-- 	  {BOLD "[1] ", "Walther, ",
-- 	       EM "Algorithmic computation of local cohomology
-- 	       modules and the local cohomological dimension of algebraic
-- 	       varieties (JPAA (139), 1999.)"
-- 	       },
-- 	  {BOLD "[2] ", "Oaku, Takayama, Walther, ",
-- 	       EM "A Localization Algorithm for D-modules (J. Symbolic Computation (29), 2000.)"
-- 	       },
-- 	  {BOLD "[3] ", "Oaku, Takayama, ",
-- 	       EM "Algorithms for D-modules -- restriction, tensor product, localization, and local cohomology groups (JPAA (156), 2001.)"
-- 	       }
-- 	  }
--      }

doc ///
  Key
    [localCohom, Strategy]
  Headline
    specify strategy for local cohomology
  Description
    Text
      There are two main strategies, Walther and OaTa. If the user selects Walther, which is the default, then @TO "LocStrategy"@ determines the localization strategy for
      @TT "localCohom(...Ideal...)"@ and @TT "localCohom(...Ideal, Module...)"@.

      @UL {
	  {TT "Walther", " -- the algorithm of U. Walther that uses Cech complex."},
	  UL {
	       {TT "LocStrategy => null",
		    " -- used only for ", TT "localCohom(...Ideal...)",
		    ", localizations are done by straightforward computation of
		    annihilators and b-polynomials as described in [", TO2 ("WeylAlgebras :: Works Cited", "Wal99"), "]."},
	       {TT "LocStrategy => OaTaWa",
		    " -- localizations are done following Oaku-Takayama-Walther method [", TO2 ("WeylAlgebras :: Works Cited", "OTW00"), "]."},
	       {TT "LocStrategy => Oaku",
		    " -- localizations are done following Oaku's algorithm."},
	  },
	  {TT "OaTa", " -- restriction from the graph embedding is used,
	       which is due to T. Oaku and N. Takayama [", TO2 ("WeylAlgebras :: Works Cited", "OT01"), "]. See ", TO "Drestriction", "."}
	  }@
  Caveat
    localCohom(...Ideal, Module...) with the default strategy computes presentations for all the terms in the Cech complex regardless of the requested homological degrees. All strategies use the given generators of the ideal; the user is advised to call @TO "mingens"@ before calling localCohom.
///

--- old format (commented out) ---
-- document {
--      Key => LocStrategy }

doc ///
  Key
    LocStrategy
///

--- old format (commented out) ---
-- document {
--      Key => [localCohom,LocStrategy],
--      Headline => "specify localization strategy for local cohomology",
--      "These strategies determine how presentations of localization in the Cech complex are calculated when selecting Walther's strategy. See ", TO [localCohom,Strategy]
--      }

doc ///
  Key
    [localCohom, LocStrategy]
  Headline
    specify localization strategy for local cohomology
  Description
    Text
      These strategies determine how presentations of localization in the Cech complex are calculated when selecting Walther's strategy. See @TO [localCohom,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => Walther,
--      Headline => "an option for localCohom=>Strategy",
--      "see ", TO "localCohom"
--      }

doc ///
  Key
    Walther
  Headline
    a value for the @TT "Strategy"@ option of @TO localCohom@
  Description
    Text
      See @TO "localCohom"@.
///

--- old format (commented out) ---
-- document {
--      Key => OaTa,
--      Headline => "an option for localCohom=>Strategy",
--      "see ", TO "localCohom"
--      }

doc ///
  Key
    OaTa
  Headline
    a value for the @TT "Strategy"@ option of @TO localCohom@
  Description
    Text
      See @TO "localCohom"@.
///

--- old format (commented out) ---
-- document {
--      Key => OaTaWa,
--      Headline => "an option for localCohom => LocStrategy",
--      SeeAlso => "localCohom"
--      }

doc ///
  Key
    OaTaWa
  Headline
    a value for the @TT "LocStrategy"@ option of @TO localCohom@
  SeeAlso
    localCohom
///

--- old format (commented out) ---
-- document {
--      Key => localCohom,
--      Headline => "local cohomology",
--      "Local cohomology of a polynomial ring:",
--      UL {
-- 	  {TO (localCohom, Ideal)},
-- 	  {TO (localCohom, List, Ideal)},
-- 	  {TO (localCohom, ZZ, Ideal)}
-- 	  },
--      "Local cohomology of a holonomic module:",
--      UL {
-- 	  {TO (localCohom, Ideal, Module)},
-- 	  {TO (localCohom, ZZ, Ideal, Module)},
-- 	  {TO (localCohom, List, Ideal, Module)}
-- 	  },
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    localCohom
  Headline
    local cohomology
  Description
    Text
      Local cohomology of a polynomial ring:

      @UL {
	  {TO (localCohom, Ideal)},
	  {TO (localCohom, List, Ideal)},
	  {TO (localCohom, ZZ, Ideal)}
	  }@

      Local cohomology of a holonomic module:

      @UL {
	  {TO (localCohom, Ideal, Module)},
	  {TO (localCohom, ZZ, Ideal, Module)},
	  {TO (localCohom, List, Ideal, Module)}
	  }@
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, Ideal),
--      Headline => "local cohomology of a polynomial ring",
--      Usage => "H = localCohom I",
--      Inputs => {
-- 	  "I" => {
-- 	       "an ideal of ",
-- 	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
-- 	       }
-- 	  },
--      Outputs => {
-- 	  "H" => {
-- 	       "each entry of ", TT "H", " has an integer key and
-- 	       contains the cohomology module in the corresponding degree."
-- 	       }
-- 	  },
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
--      	  "h = localCohom I",
--      	  "pruneLocalCohom h"
-- 	  },
--      Caveat => {"The modules returned are not simplified,
--      	  use ", TO "pruneLocalCohom", "."},
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, Ideal)
  Headline
    local cohomology of a polynomial ring
  Usage
    H = localCohom I
  Inputs
    I:Ideal
      an ideal of $R = k[x_1,\ldots,x_n]$
  Outputs
    H:HashTable
      each entry of $H$ has an integer key and
      contains the cohomology module in the corresponding degree
  Description
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom I
      pruneLocalCohom h
  Caveat
    The modules returned are not simplified,
    use @TO "pruneLocalCohom"@.
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, List, Ideal),
--      Headline => "local cohomology of a polynomial ring",
--      Usage => "localCohom(l,I)",
--      Inputs => { "l", "I" },
--      Outputs => { { "the local cohomology of ", TT "I", " in the degrees specified by ", EM "l" } },
--      "See ", TO (localCohom, Ideal), " for the full description.",
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
--      	  "h = localCohom({1,2}, I)",
--      	  "pruneLocalCohom h"
-- 	  },
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, List, Ideal)
  Headline
    local cohomology of a polynomial ring
  Usage
    localCohom(l, I)
  Inputs
    l:List
    I:Ideal
  Outputs
    :HashTable
      the local cohomology of $I$ in the degrees specified by $l$
  Description
    Text
      See @TO (localCohom, Ideal)@ for the full description.
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom({1,2}, I)
      pruneLocalCohom h
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, ZZ, Ideal),
--      Headline => "local cohomology of a polynomial ring",
--      Usage => "localCohom(d,I)",
--      Inputs => { "d", "I" },
--      Outputs => {{ "the local cohomology of ", TT "I", " in degree ", EM "d" }},
--      "See ", TO (localCohom, Ideal), " for the full description.",
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
-- 	  "h = localCohom (2,I)",
--      	  "Dprune h"
-- 	  },
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, ZZ, Ideal)
  Headline
    local cohomology of a polynomial ring
  Usage
    localCohom(d, I)
  Inputs
    d:ZZ
    I:Ideal
  Outputs
    :Module
      the local cohomology of $I$ in degree $d$
  Description
    Text
      See @TO (localCohom, Ideal)@ for the full description.
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom (2,I)
      Dprune h
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, Ideal, Module),
--      Headline => "local cohomology of a D-module",
--      Usage => "H = localCohom(I,M)",
--      Inputs => {
-- 	  "I" => {
-- 	       "an ideal of ",
-- 	       EM {"R = k[x", SUB "1", ",...,x", SUB "n", "]"}
-- 	       },
-- 	  "M" => {
-- 	       "a holonomic module over Weyl algebra ",
-- 	       EM{"A", SUB "n", "(k)"}
-- 	       }
-- 	  },
--      Outputs => {
-- 	  "H" => {
-- 	       "each entry of ", TT "H", " has an integer key and
-- 	       contains the cohomology module in the corresponding degree."
-- 	       }
-- 	  },
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
--      	  "h = localCohom(I, W^1 / ideal{dX,dY,dZ})",
--      	  "pruneLocalCohom h"
-- 	  },
--      Caveat => {"The modules returned are not simplified,
--      	  use ", TO "pruneLocalCohom", "."},
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, Ideal, Module)
  Headline
    local cohomology of a $D$-module
  Usage
    H = localCohom(I, M)
  Inputs
    I:Ideal
      an ideal of $R = k[x_1,\ldots,x_n]$
    M:Module
      a holonomic module over Weyl algebra $D$
  Outputs
    H:HashTable
      each entry of $H$ has an integer key and
      contains the cohomology module in the corresponding degree
  Description
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom(I, W^1 / ideal{dX,dY,dZ})
      pruneLocalCohom h
  Caveat
    The modules returned are not simplified,
    use @TO "pruneLocalCohom"@.
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, ZZ, Ideal, Module),
--      Headline => "local cohomology of a D-module",
--      Usage => "localCohom(d,I,M)",
--      Inputs => { "d", "I", "M" },
--      Outputs => {{
-- 	  "the local cohomology ",
-- 	  EM {"H", SUB "I", "(M)"}, " in degree ", EM "d", ", where ", EM "I",
-- 	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
-- 	  }},
--      "See ", TO "localCohom(Ideal,Module)", " for the full description.",
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
-- 	  "h = localCohom(2, I, W^1 / ideal{dX,dY,dZ})",
-- 	  "pruneLocalCohom h"
-- 	  },
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, ZZ, Ideal, Module)
  Headline
    local cohomology of a $D$-module
  Usage
    localCohom(d, I, M)
  Inputs
    d:ZZ
    I:Ideal
    M:Module
  Outputs
    :HashTable
      the local cohomology $H^d_I(M)$, where $I$
      is an ideal in a polynomial ring and $M$ is a $D$-module
  Description
    Text
      See @TO (localCohom, Ideal, Module)@ for the full description.
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom(2, I, W^1 / ideal{dX,dY,dZ})
      pruneLocalCohom h
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => (localCohom, List, Ideal, Module),
--      Headline => "local cohomology of a D-module",
--      Usage => "localCohom(l,I,M)",
--      Inputs => { "l", "I", "M" },
--      Outputs => {{
-- 	  "the local cohomology ",
-- 	  EM {"H", SUB "I", "(M)"}, " in degrees listed in ", EM "l",
-- 	  ", where ", EM "I",
-- 	  " is an ideal in a polynomial ring and ", EM "M", " is a D-module"
-- 	  }},
--      "See ", TO "localCohom(Ideal,Module)", " for the full description.",
--      EXAMPLE {
-- 	  "W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]",
--      	  "I = ideal (X*(Y-Z), X*Y*Z)",
-- 	  "h = localCohom({1,2}, I, W^1 / ideal{dX,dY,dZ})",
-- 	  "pruneLocalCohom h"
-- 	  },
--      SeeAlso => {"pruneLocalCohom"}
--      }

doc ///
  Key
    (localCohom, List, Ideal, Module)
  Headline
    local cohomology of a $D$-module
  Usage
    localCohom(l, I, M)
  Inputs
    l:List
    I:Ideal
    M:Module
  Outputs
    :HashTable
      the local cohomology $H^*_I(M)$ in degrees listed in $l$,
      where $I$ is an ideal in a polynomial ring and $M$ is a $D$-module
  Description
    Text
      See @TO (localCohom, Ideal, Module)@ for the full description.
    Example
      W = QQ[X, dX, Y, dY, Z, dZ, WeylAlgebra=>{X=>dX, Y=>dY, Z=>dZ}]
      I = ideal (X*(Y-Z), X*Y*Z)
      h = localCohom({1,2}, I, W^1 / ideal{dX,dY,dZ})
      pruneLocalCohom h
  SeeAlso
    pruneLocalCohom
///

--- old format (commented out) ---
-- document {
--      Key => {(pruneLocalCohom, HashTable), pruneLocalCohom},
--      Headline => "prunes local cohomology modules",
--      Usage => "pruneLocalCohom H",
--      Inputs => {{"the output of ", TO "localCohom"}},
--      Outputs => {HashTable},
--      "This function applies ", TO "Dprune", " to all the keys of ", TT "H", ".",
--      SeeAlso => {"localCohom", "Dprune"}
--      }

doc ///
  Key
    (pruneLocalCohom, HashTable)
    pruneLocalCohom
  Headline
    prunes local cohomology modules
  Usage
    pruneLocalCohom H
  Inputs
    H:HashTable
      the output of @TO "localCohom"@
  Outputs
    :HashTable
  Description
    Text
      This function applies @TO "Dprune"@ to all the keys of $H$.
  SeeAlso
    localCohom
    Dprune
///

--- old format (commented out) ---
-- document {
--      Key => {(deRhamAll, RingElement), deRhamAll},
--      Headline => "deRham complex for the complement of a hypersurface",
--      Usage => "deRhamAll f",
--      Inputs => {"f"},
--      Outputs => {
-- 	  HashTable => {"containing explicit cohomology classes
-- 	       in the deRham complex for the complement
-- 	       of the hypersurface ",  EM "{f = 0}", " and
--      	       supplementary information"}
-- 	  },
--      "The routine deRhamAll can be used to compute cup product structures
--      as in the paper 'The cup product structure for complements
--      of affine varieties' by Walther(2000).",
--      PARA{},
--      "For a more basic functionality see ", TO "deRham", ".",
--      EXAMPLE lines ///
-- 	R = QQ[x,y]
--      	f = x^2-y^3
--      	deRhamAll f
-- 	///,
--      SeeAlso => {"deRham", "Dlocalize", "Dintegration"}
-- }

doc ///
  Key
    (deRhamAll, RingElement)
    deRhamAll
  Headline
    deRham complex for the complement of a hypersurface
  Usage
    deRhamAll f
  Inputs
    f:RingElement
  Outputs
    :HashTable
      containing explicit cohomology classes
      in the deRham complex for the complement
      of the hypersurface $\{f = 0\}$ and supplementary information
  Description
    Text
      The routine deRhamAll can be used to compute cup product structures
      as in [@TO2 ("WeylAlgebras :: Works Cited", "Wal00")@].

      For a more basic functionality see @TO "deRham"@.
    Example
      R = QQ[x,y]
      f = x^2-y^3
      deRhamAll f
  SeeAlso
    deRham
    Dlocalize
    Dintegration
///

--- old format (commented out) ---
-- document {
--      Key => [deRham,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [deRham, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => [deRhamAll,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [deRhamAll, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => {deRham, (deRham, ZZ, RingElement), (deRham, RingElement)},
--      Headline => "deRham cohomology groups for the complement of a hypersurface",
--      Usage => "M = deRham f, Mi = deRham(i,f)",
--      Inputs => {
-- 	  "i" => ZZ,
-- 	  "f" => RingElement
-- 	  },
--      Outputs => {
-- 	  "Mi" => Module => {"the i-th deRham cohomology group of the complement
-- 	  of the hypersurface ",  EM "{f = 0}"},
-- 	  "M" => HashTable => {"containing the entries of the form ", TT "i=>Mi"}
-- 	  },
--      "The algorithm used appears in the paper 'An algorithm for deRham
--      cohomology groups of the complement of an affine variety via D-module
--      computation' by Oaku-Takayama(1999).
--      The method is to compute the localization of the polynomial ring
--      by f, then compute the derived integration of the localization.",
--      EXAMPLE lines ///
-- 	R = QQ[x,y]
--      	f = x^2-y^3
--      	deRham f
-- 	deRham(1,f)
-- 	///,
--      SeeAlso => {"deRhamAll", "Dlocalize", "Dintegration"}
--      }

doc ///
  Key
    deRham
    (deRham, ZZ, RingElement)
    (deRham, RingElement)
  Headline
    deRham cohomology groups for the complement of a hypersurface
  Usage
    M = deRham f
    Mi = deRham(i, f)
  Inputs
    i:ZZ
    f:RingElement
  Outputs
    Mi:Module
      the $i$-th deRham cohomology group of the complement
      of the hypersurface $\{f = 0\}$
    M:HashTable
      containing the entries of the form @TT "i=>Mi"@
  Description
    Text
      The algorithm used appears in [@TO2 ("WeylAlgebras :: Works Cited", "OT99")@].
      The method is to compute the localization of the polynomial ring
      by $f$, then compute the derived integration of the localization.
    Example
      R = QQ[x,y]
      f = x^2-y^3
      deRham f
      deRham(1,f)
  SeeAlso
    deRhamAll
    Dlocalize
    Dintegration
///

--- old format (commented out) ---
-- document {
--      Key => TransferCycles,
--      Headline => "a key in the hashtable created by deRham",
--      SeeAlso => "deRham"
--      }

doc ///
  Key
    TransferCycles
  Headline
    a key in the hashtable created by deRhamAll
  SeeAlso
    deRhamAll
///

--- old format (commented out) ---
-- document {
--      Key => CohomologyGroups,
--      Headline => "a key in the hashtable created by deRham",
--      SeeAlso => "deRham"
--      }

doc ///
  Key
    CohomologyGroups
  Headline
    a key in the hashtable created by deRhamAll
  SeeAlso
    deRhamAll
///

--- old format (commented out) ---
-- document {
--      Key => PreCycles,
--      Headline => "a key in the hashtable created by deRham",
--      SeeAlso => "deRham"
--      }

doc ///
  Key
    PreCycles
  Headline
    a key in the hashtable created by deRhamAll
  SeeAlso
    deRhamAll
///

--- old format (commented out) ---
-- document {
--      Key => LocalizeMap,
--      Headline => "a key in the hashtable created by deRham",
--      SeeAlso => "deRham"
--      }

doc ///
  Key
    LocalizeMap
  Headline
    a key in the hashtable created by deRhamAll
  SeeAlso
    deRhamAll
///

--- old format (commented out) ---
-- document {
--      Key => OmegaRes,
--      Headline => "a key in the hashtable created by deRham",
--      SeeAlso => "deRham"
--      }

doc ///
  Key
    OmegaRes
  Headline
    a key in the hashtable created by deRhamAll
  SeeAlso
    deRhamAll
///
