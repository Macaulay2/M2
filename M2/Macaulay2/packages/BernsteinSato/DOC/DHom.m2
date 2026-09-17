--- old format (commented out) ---
-- document {
--      Key => [DHom,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [DHom, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => {DHom, (DHom,Module,Module), (DHom,Module,Module,List), (DHom,Ideal,Ideal)},
--      Headline=>"D-homomorphisms between holonomic D-modules",
--      Usage => "DHom(M,N), DHom(M,N,w), DHom(I,J)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "N" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
-- 	  "J" => Ideal => {"which represents the module ", EM "N = D/J"},
-- 	  "w" => List => "a positive weight vector"
-- 	  },
--      Outputs => {
-- 	  HashTable => {" a basis of D-homomorphisms between holonomic D-modules ",
-- 	       EM "M", " and ", EM "N" }
--      	  },
--      "The set of D-homomorphisms between two holonomic modules ", EM "M",
--      " and ", EM "N",
--      " is a finite-dimensional vector space over the ground field.
--      Since a homomorphism is defined by where it sends a set of generators,
--      the output of this command is a list of matrices whose columns
--      correspond to the images of the generators of ", EM "M", ".  Here
--      the generators of ", EM "M", " are determined from its presentation
--      by generators and relations.",
--      PARA {
-- 	  "The procedure calls ", TO "Drestriction", ", which uses ",
-- 	  EM "w", " if specified."
-- 	  },
--      PARA {
-- 	  "The algorithm used appears in the paper 'Computing homomorphisms
-- 	  between holonomic D-modules' by Tsai-Walther(2000).
-- 	  The method is to combine isomorphisms of Bjork and Kashiwara with
-- 	  the restriction algorithm."},
--      EXAMPLE lines ///
-- 	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
-- 	     M = W^1/ideal(D-1)
-- 	     N = W^1/ideal((D-1)^2)
-- 	     DHom(M,N)
-- 	     ///,
--      Caveat => {"Input modules ", EM "M", ", ", EM "N", ", ",
-- 	  EM "D/I", " and ", EM "D/J", " should be holonomic."},
--      SeeAlso => {"DExt", "Drestriction"}
--      }

doc ///
  Key
    DHom
    (DHom, Module, Module)
    (DHom, Module, Module, List)
    (DHom, Ideal, Ideal)
  Headline
    $D$-homomorphisms between holonomic $D$-modules
  Usage
    DHom(M, N)
    DHom(M, N, w)
    DHom(I, J)
  Inputs
    M:Module
      over the Weyl algebra $D$
    N:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
    J:Ideal
      which represents the module $N = D/J$
    w:List
      a positive weight vector
  Outputs
    :HashTable
      a basis of $D$-homomorphisms between holonomic $D$-modules $M$ and $N$
  Description
    Text
      The set of $D$-homomorphisms between two holonomic modules $M$
      and $N$ is a finite-dimensional vector space over the ground field.
      Since a homomorphism is defined by where it sends a set of generators,
      the output of this command is a list of matrices whose columns
      correspond to the images of the generators of $M$.  Here
      the generators of $M$ are determined from its presentation
      by generators and relations.

      The procedure calls @TO "Drestriction"@, which uses
      $w$ if specified.

      The algorithm used appears in [@TO2 ("WeylAlgebras :: Works Cited", "TW01")@].
      The method is to combine isomorphisms of Bjork and Kashiwara with
      the restriction algorithm.
    Example
      W = QQ[x, D, WeylAlgebra=>{x=>D}]
      M = W^1/ideal(D-1)
      N = W^1/ideal((D-1)^2)
      DHom(M,N)
  Caveat
    Input modules $M$, $N$, $D/I$ and $D/J$ should be holonomic.
  SeeAlso
    DExt
    Drestriction
///

--- old format (commented out) ---
-- document {
--      Key => [DExt,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [DExt, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => [DExt,Special]
--      }

doc ///
  Key
    [DExt, Special]
///

--- old format (commented out) ---
-- document {
--      Key => Special,
--      SeeAlso => "DExt"
--      }

doc ///
  Key
    Special
  SeeAlso
    DExt
///

--- old format (commented out) ---
-- document {
--      Key => None,
--      Headline => "an option for DExt=>Special",
--      SeeAlso => "DExt"
--      }

doc ///
  Key
    None
  Headline
    a value for the @TT "Special"@ option of @TO DExt@
  SeeAlso
    DExt
///

--- old format (commented out) ---
-- document {
--      Key => [DExt,Output]}

doc ///
  Key
    [DExt, Output]
///

--- old format (commented out) ---
-- document {
--      Key => Output
--      }

doc ///
  Key
    Output
///

--- old format (commented out) ---
-- document {
--      Key => [DExt,Info]
--      }

doc ///
  Key
    [DExt, Info]
///

--- old format (commented out) ---
-- document {
--      Key => Info
--      }

doc ///
  Key
    Info
///

--- old format (commented out) ---
-- document {
--      Key => {DExt, (DExt, Module, Module), (DExt, Module, Module, List)},
--      Headline => "Ext groups between holonomic modules",
--      Usage => "DExt(M,N), DExt(M,N,w)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "N" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "w" => List => "a positive weight vector"
-- 	  },
--      Outputs => {
-- 	  HashTable => {" the ",
-- 	       TT "Ext"," groups between holonomic D-modules", EM "M",
-- 	       " and ", EM "N" }
--      	  },
--      "The ", TEX "Ext", " groups between D-modules ", EM "M"," and ", EM "N",
--      " are the derived functors of ", TEX "Hom", ", and are finite-dimensional
--      vector spaces over the ground field when ", EM "M", " and ", EM "N", " are holonomic.",
--      PARA {
-- 	  "The procedure calls ", TO "Drestriction", ", which uses ",
-- 	  EM "w", " if specified."
-- 	  },
--      PARA {
-- 	  "The algorithm used appears in the paper 'Polynomial and
-- 	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
-- 	  The method is to combine isomorphisms of Bjork and Kashiwara with
-- 	  the restriction algorithm."},
--      EXAMPLE lines ///
-- 	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
-- 	     M = W^1/ideal(x*(D-1))
-- 	     N = W^1/ideal((D-1)^2)
-- 	     DExt(M,N)
-- 	     ///,
--      Caveat =>{
-- 	  "Input modules M, N should be holonomic.",
-- 	  "Does not yet compute explicit representations of Ext groups
-- 	  such as Yoneda representation."
-- 	  },
--      SeeAlso => {"DHom", "Drestriction"}
--      }

doc ///
  Key
    DExt
    (DExt, Module, Module)
    (DExt, Module, Module, List)
  Headline
    Ext groups between holonomic modules
  Usage
    DExt(M, N)
    DExt(M, N, w)
  Inputs
    M:Module
      over the Weyl algebra $D$
    N:Module
      over the Weyl algebra $D$
    w:List
      a positive weight vector
  Outputs
    :HashTable
      the Ext groups between holonomic $D$-modules $M$ and $N$
  Description
    Text
      The Ext groups between $D$-modules $M$ and $N$
      are the derived functors of Hom, and are finite-dimensional
      vector spaces over the ground field when $M$ and $N$ are holonomic.

      The procedure calls @TO "Drestriction"@, which uses
      $w$ if specified.

      The algorithm used appears in [@TO2 ("WeylAlgebras :: Works Cited", "OTT01")@].
      The method is to combine isomorphisms of Bjork and Kashiwara with
      the restriction algorithm.
    Example
      W = QQ[x, D, WeylAlgebra=>{x=>D}]
      M = W^1/ideal(x*(D-1))
      N = W^1/ideal((D-1)^2)
      DExt(M,N)
  Caveat
    Input modules $M$, $N$ should be holonomic.
    Does not yet compute explicit representations of Ext groups
    such as Yoneda representation.
  SeeAlso
    DHom
    Drestriction
///

--- old format (commented out) ---
-- document {
--      Key => {Ddual, (Ddual,Module), (Ddual,Ideal)},
--      Headline => "holonomic dual of a D-module",
--      Usage => "Ddual M, Ddual I",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
-- 	  },
--      Outputs => {
-- 	  Module => {"the holonomic dual of ", EM "M"}
-- 	  },
--      "If M is a holonomic left D-module, then ",
--      BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")",
--      " is a holonomic right D-module.  The holonomic dual is defined to be the left
--      module associated to ",
--      BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")",
--      ".  The dual is obtained by
--      computing a free resolution of ", EM "M", ", dualizing, and applying
--      the standard transposition to the ", EM "n", "-th homology.",
--      EXAMPLE lines ///
--              I = AppellF1({1,0,-3,2})
-- 	     Ddual I
-- 	     ///,
--      Caveat =>{"The input module ", EM "M", " should be holonomic.  The user should
-- 	  check this manually with the script ", TT "Ddim", "."},
--      SeeAlso => {"Ddim", "Dtransposition"}
--      }

doc ///
  Key
    Ddual
    (Ddual, Module)
    (Ddual, Ideal)
  Headline
    holonomic dual of a $D$-module
  Usage
    Ddual M
    Ddual I
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
  Outputs
    :Module
      the holonomic dual of $M$
  Description
    Text
      If $M$ is a holonomic left $D$-module, then
      $\text{Ext}^n_D(M, D)$
      is a holonomic right $D$-module.  The holonomic dual is defined to be the left
      module associated to
      $\text{Ext}^n_D(M, D)$.
      The dual is obtained by
      computing a free resolution of $M$, dualizing, and applying
      the standard transposition to the $n$-th homology.
    Example
      I = AppellF1({1,0,-3,2})
      Ddual I
  Caveat
    The input module $M$ should be holonomic.  The user should
    check this manually with the script @TO Ddim@.
  SeeAlso
    Ddim
    Dtransposition
///

--- old format (commented out) ---
-- document {
--      Key => [polynomialExt,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [polynomialExt, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => {polynomialExt, (polynomialExt,Module), (polynomialExt,ZZ,Ideal), (polynomialExt,ZZ,Module), (polynomialExt,Ideal)},
--      Headline => "Ext groups between a holonomic module and a polynomial ring",
--      Usage => "polynomialExt M, polynomialExt I; rationalFunctionExt(i,M), rationalFunctionExt(i,I)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
-- 	  "i" => ZZ => "nonnegative"
-- 	  },
--      Outputs => {
--      	  { ofClass HashTable, " or ", ofClass Module, ", the ",
-- 	       TEX "Ext^i"," group(s) between holonomic ", EM "M",
-- 	       " and the polynomial ring" }
--      	  },
--      "The ", TT "Ext", " groups between a D-module ", EM "M",
--      " and the polynomial ring are the derived functors of ", TT "Hom",
--      ", and are finite-dimensional vector spaces over the ground field when ",
--      EM "M", " is holonomic.",
--      PARA {
-- 	  "The algorithm used appears in the paper 'Polynomial and
-- 	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
-- 	  The method is to combine isomorphisms of Bjork and Kashiwara with
-- 	  the restriction algorithm."},
--      EXAMPLE lines ///
-- 	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
-- 	     M = W^1/ideal(x^2*D^2)
-- 	     polynomialExt(M)
-- 	     ///,
--      Caveat =>{"Does not yet compute explicit representations of
-- 	  Ext groups such as Yoneda representation."},
--      SeeAlso => {"polynomialSolutions", "rationalFunctionExt", "DExt", "Dintegration"}
--      }

doc ///
  Key
    polynomialExt
    (polynomialExt, Module)
    (polynomialExt, ZZ, Ideal)
    (polynomialExt, ZZ, Module)
    (polynomialExt, Ideal)
  Headline
    Ext groups between a holonomic module and a polynomial ring
  Usage
    polynomialExt M
    polynomialExt I
    polynomialExt(i, M)
    polynomialExt(i, I)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
    i:ZZ
      nonnegative
  Outputs
    :HashTable
      or @ofClass Module@, the $\text{Ext}^i$ group(s) between holonomic $M$
      and the polynomial ring
  Description
    Text
      The Ext groups between a $D$-module $M$
      and the polynomial ring are the derived functors of Hom,
      and are finite-dimensional vector spaces over the ground field when
      $M$ is holonomic.

      The algorithm used appears in [@TO2 ("WeylAlgebras :: Works Cited", "OTT01")@].
      The method is to combine isomorphisms of Bjork and Kashiwara with
      the restriction algorithm.
    Example
      W = QQ[x, D, WeylAlgebra=>{x=>D}]
      M = W^1/ideal(x^2*D^2)
      polynomialExt(M)
  Caveat
    Does not yet compute explicit representations of
    Ext groups such as Yoneda representation.
  SeeAlso
    polynomialSolutions
    rationalFunctionExt
    DExt
    Dintegration
///

--- old format (commented out) ---
-- document {
--      Key => [rationalFunctionExt,Strategy],
-- 	  "Option is passed to Dresolution. See ",
-- 	  TO [Dresolution,Strategy]
-- }

doc ///
  Key
    [rationalFunctionExt, Strategy]
  Description
    Text
      Option is passed to Dresolution. See @TO [Dresolution,Strategy]@.
///

--- old format (commented out) ---
-- document {
--      Key => {rationalFunctionExt, (rationalFunctionExt,Module), (rationalFunctionExt,ZZ,Ideal,RingElement), (rationalFunctionExt,ZZ,Ideal),
-- 	  (rationalFunctionExt,Ideal,RingElement), (rationalFunctionExt,Ideal),(rationalFunctionExt,ZZ,Module,RingElement),
-- 	  (rationalFunctionExt,ZZ,Module), (rationalFunctionExt,Module,RingElement)},
--      Headline => "Ext(holonomic D-module, polynomial ring localized at the singular locus)",
--      Usage => "rationalFunctionExt M, rationalFunctionExt I; rationalFunctionExt(M,f), rationalFunctionExt(I,f);
--                rationalFunctionExt(i,M), rationalFunctionExt(i,I); rationalFunctionExt(i,M,f), rationalFunctionExt(i,I,f)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
-- 	  "f" => RingElement => "a polynomial",
-- 	  "i" => ZZ => "nonnegative"
-- 	  },
--      Outputs => {
--      	  { ofClass HashTable, " or ", ofClass Module, ", the ",
-- 	       TEX "Ext^i"," group(s) between holonomic ", EM "M",
-- 	       " and the polynomial ring localized at the singular locus of ", EM "M",
-- 	       " (or at ", EM "f", " if specified)" }
--      	  },
--      "The Ext groups between M and N
--      are the derived functors of Hom, and are finite-dimensional
--      vector spaces over the ground field when M and N are holonomic.",
--      PARA {
-- 	  "The algorithm used appears in the paper 'Polynomial and
-- 	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
-- 	  The method is to combine isomorphisms of Bjork and Kashiwara with
-- 	  the restriction algorithm."},
--      EXAMPLE lines ///
-- 	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
-- 	     M = W^1/ideal(x*D+5)
-- 	     rationalFunctionExt M
-- 	     ///,
--      Caveat =>{"Input modules M or D/I should be holonomic."},
--      SeeAlso => {"Dresolution", "Dintegration"}
--      }

doc ///
  Key
    rationalFunctionExt
    (rationalFunctionExt, Module)
    (rationalFunctionExt, ZZ, Ideal, RingElement)
    (rationalFunctionExt, ZZ, Ideal)
    (rationalFunctionExt, Ideal, RingElement)
    (rationalFunctionExt, Ideal)
    (rationalFunctionExt, ZZ, Module, RingElement)
    (rationalFunctionExt, ZZ, Module)
    (rationalFunctionExt, Module, RingElement)
  Headline
    Ext(holonomic $D$-module, polynomial ring localized at the singular locus)
  Usage
    rationalFunctionExt M
    rationalFunctionExt I
    rationalFunctionExt(M, f)
    rationalFunctionExt(I, f)
    rationalFunctionExt(i, M)
    rationalFunctionExt(i, I)
    rationalFunctionExt(i, M, f)
    rationalFunctionExt(i, I, f)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
    f:RingElement
      a polynomial
    i:ZZ
      nonnegative
  Outputs
    :HashTable
      or @ofClass Module@, the $\text{Ext}^i$ group(s) between holonomic $M$
      and the polynomial ring localized at the singular locus of $M$
      (or at $f$ if specified)
  Description
    Text
      The Ext groups between a holonomic $D$-module $M$ and the polynomial
      ring localized at the singular locus of $M$ (or at $f$ if specified)
      are derived functors of Hom, and are finite-dimensional vector spaces
      over the ground field.

      The algorithm used appears in [@TO2 ("WeylAlgebras :: Works Cited", "OTT01")@].
      The method is to combine isomorphisms of Bjork and Kashiwara with
      the restriction algorithm.
    Example
      W = QQ[x, D, WeylAlgebra=>{x=>D}]
      M = W^1/ideal(x*D+5)
      rationalFunctionExt M
  Caveat
    Input modules $M$ or $D/I$ should be holonomic.
  SeeAlso
    Dresolution
    Dintegration
///

doc ///
  Key
    polynomialSolutions
    (polynomialSolutions,Module)
    (polynomialSolutions,Ideal,List)
    (polynomialSolutions,Module,List)
    (polynomialSolutions,Ideal)
  Headline
    polynomial solutions of a holonomic system
  Usage
    polynomialSolutions I
    polynomialSolutions M
    polynomialSolutions(I,w)
    polynomialSolutions(M,w)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      holonomic ideal in the Weyl algebra $D$
    w:List
      a weight vector
  Outputs
    :List
      a basis of the polynomial solutions of $I$
      (or of $D$-homomorphisms between $M$ and the polynomial ring)
      using $w$ for Groebner deformations.
      If no $w$ is given, then it is taken to be the all ones vector.
  Description
    Text
      The polynomial solutions of a holonomic system form a
      finite-dimensional vector space.
      There are two algorithms implemented to get these solutions.
      The first algorithm is based on Gröbner deformations and
      works for ideals $I$ of PDEs - see [@TO2 ("WeylAlgebras :: Works Cited", "OTT01")@].
      The second algorithm is based on homological algebra - see
      [@TO2 ("WeylAlgebras :: Works Cited", "TW01")@].
    Example
      makeWA(QQ[x])
      I = ideal(dx^2, (x-1)*dx-1)
      polynomialSolutions I
  SeeAlso
    rationalFunctionSolutions
    Dintegration
///

--- old format (commented out) ---
-- document { Key => Alg }

doc ///
  Key
    Alg
///

--- old format (commented out) ---
-- document {
--      Key => [polynomialSolutions, Alg],
--      Headline => "algorithm for finding polynomial solutions",
--      UL {
-- 	  {BOLD "GD", " -- uses Groebner deformations"},
-- 	  {BOLD "Duality", " -- uses homological duality"}
-- 	  }
--      }

doc ///
  Key
    [polynomialSolutions, Alg]
  Headline
    algorithm for finding polynomial solutions
  Description
    Text
      @UL {
	  {TT "GD", " -- uses Groebner deformations"},
	  {TT "Duality", " -- uses homological duality"}
	  }@
///

--- old format (commented out) ---
-- document {
--      Key => GD,
--      Headline => "an option for polynomialSolutions=>Alg",
--      SeeAlso => "polynomialSolutions"
--      }

doc ///
  Key
    GD
  Headline
    a value for the @TT "Alg"@ option of @TO polynomialSolutions@
  SeeAlso
    polynomialSolutions
///

--- old format (commented out) ---
-- document {
--      Key => Duality,
--      Headline => "an option for polynomialSolutions=>Alg",
--      SeeAlso => "polynomialSolutions"
--      }

doc ///
  Key
    Duality
  Headline
    a value for the @TT "Alg"@ option of @TO polynomialSolutions@
  SeeAlso
    polynomialSolutions
///

doc ///
  Key
    rationalFunctionSolutions
    (rationalFunctionSolutions,Ideal,List,List)
    (rationalFunctionSolutions,Ideal,RingElement,List)
    (rationalFunctionSolutions,Ideal,List)
    (rationalFunctionSolutions,Ideal,RingElement)
    (rationalFunctionSolutions,Ideal)
  Headline
    rational solutions of a holonomic system
  Usage
    rationalFunctionSolutions I
    rationalFunctionSolutions(I,f)
    rationalFunctionSolutions(I,f,w)
    rationalFunctionSolutions(I,ff)
    rationalFunctionSolutions(I,ff,w)
  Inputs
    I:Ideal
      holonomic ideal in the Weyl algebra $D$
    f:RingElement
      a polynomial
    ff:List
      a list of polynomials
    w:List
      a weight vector
  Outputs
    :List
      a basis of the rational solutions of $I$ with poles along $f$
      or along the polynomials in @TT "ff"@ using $w$
      for Groebner deformations
  Description
    Text
      The rational solutions of a holonomic system form a
      finite-dimensional vector space.
      The only possibilities for the poles of a rational solution
      are the codimension one components of the singular locus.
      An algorithm to compute rational solutions
      is based on Gröbner deformations and
      works for ideals $I$ of PDEs - see [@TO2 ("WeylAlgebras :: Works Cited", "OTT01")@].
    Example
      makeWA(QQ[x])
      I = ideal((x+1)*dx+5)
      rationalFunctionSolutions I
  Caveat
    The most efficient method to find rational solutions of a system of differential
    equations is
    to find the singular locus, then try to find its irreducible
    factors.  With these, call rationalFunctionSolutions(I, ff, w), where w
    should be generic enough so that the polynomialSolutions routine
    will not complain of a non-generic weight vector.
  SeeAlso
    polynomialSolutions
    rationalFunctionExt
    DHom
///
