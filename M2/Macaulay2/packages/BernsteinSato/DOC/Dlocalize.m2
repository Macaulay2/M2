
--- old format (commented out) ---
-- document {
--      Key => {[Dlocalize,Strategy],[DlocalizeAll,Strategy],[DlocalizeMap,Strategy],
-- 	 Oaku,OTW,OTWcyclic},
--      Headline=>"strategy for computing a localization of a D-module",
--      UL{
-- 	  {BOLD "Oaku", " -- use Oaku's algorithm"},
-- 	  {BOLD "OTW", " -- use Oaku-Takayama-Walther's algorithm"},
-- 	  {BOLD "OTWcyclic", " -- use Oaku-Takayama-Walther's algorithm for a cyclic module"}
-- 	  }
--      }

doc ///
  Key
    [Dlocalize, Strategy]
    [DlocalizeAll, Strategy]
    [DlocalizeMap, Strategy]
    Oaku
    OTW
    OTWcyclic
  Headline
    strategy for computing a localization of a $D$-module
  Description
    Text
      @UL {
	  {TT "Oaku", " -- use Oaku's algorithm"},
	  {TT "OTW", " -- use Oaku-Takayama-Walther's algorithm"},
	  {TT "OTWcyclic", " -- use Oaku-Takayama-Walther's algorithm for a cyclic module"}
	  }@
///

--- old format (commented out) ---
-- document {
--      Key => {Dlocalize, (Dlocalize,Ideal,RingElement), (Dlocalize,Module,RingElement)},
--      Headline => "localization of a D-module",
--      Usage => "Dlocalize(M,f), Dlocalize(I,f)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
-- 	  "f" => RingElement => "a polynomial",
-- 	  },
--      Outputs => {
-- 	  Module => {"the localized module ", TEX "M_f = M[f^{-1}]", " as a D-module"}
-- 	  },
--      "One of the nice things about D-modules is that if a finitely
--      generated D-module is specializable along ", EM "f", ", then it's localization
--      with respect to ", EM "f", " is also finitely generated.  For instance,
--      this is true for all holonomic D-modules.",
--      PARA{},
--      "There are two different algorithms for localization implemented.
--      The first appears in the
--      paper 'A localization algorithm for D-modules' by Oaku-Takayama-Walther
--      (1999).  The second is due to Oaku and appears in the paper
--      'Algorithmic computation of local cohomology modules and the
--      cohomological dimension of algebraic varieties' by Walther(1999)",
--      EXAMPLE lines ///
-- 	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
--      	M = W^1/(ideal(x*Dx+1, Dy))
--      	f = x^2-y^3
--      	Mf = Dlocalize(M, f)
-- 	///,
--      SeeAlso => {"DlocalizeAll", "DlocalizeMap", "AnnFs", "Dintegration"}
--      }

doc ///
  Key
    Dlocalize
    (Dlocalize, Ideal, RingElement)
    (Dlocalize, Module, RingElement)
  Headline
    localization of a $D$-module
  Usage
    Dlocalize(M, f)
    Dlocalize(I, f)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
    f:RingElement
      a polynomial
  Outputs
    :Module
      the localized module $M_f = M[f^{-1}]$ as a $D$-module
  Description
    Text
      One of the nice things about $D$-modules is that if a finitely
      generated $D$-module is specializable along $f$, then its localization
      with respect to $f$ is also finitely generated.  For instance,
      this is true for all holonomic $D$-modules.

      There are two different algorithms for localization implemented.
      The first appears in [@TO2 ("WeylAlgebras :: Works Cited", "OTW00")@].
      The second is due to Oaku and appears in [@TO2 ("WeylAlgebras :: Works Cited", "Wal99")@].
    Example
      W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
      M = W^1/(ideal(x*Dx+1, Dy))
      f = x^2-y^3
      Mf = Dlocalize(M, f)
  SeeAlso
    DlocalizeAll
    DlocalizeMap
    AnnFs
    Dintegration
///

--- old format (commented out) ---
-- document {
--      Key => {DlocalizeMap, (DlocalizeMap,Ideal,RingElement), (DlocalizeMap,Module,RingElement)},
--      Headline => "localization map from a D-module to its localization",
--      Usage => "DlocalizeMap(M,f), DlocalizeMap(I,f)",
--      Inputs => {
-- 	  "M" => Module => {"over the Weyl algebra ", EM "D"},
-- 	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
-- 	  "f" => RingElement => "a polynomial",
-- 	  },
--      Outputs => {
-- 	  Matrix => {"which represents the natural map from ", TEX "M", " to ", TEX "M_f = M[f^{-1}]"}
-- 	  },
--      "A supplementary function for ", TO "Dlocalize",
--      " that computes the localization map.",
--      EXAMPLE lines ///
-- 	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
--      	M = W^1/(ideal(x*Dx+1, Dy))
--      	f = x^2-y^3
--      	DlocalizeMap(M, f)
-- 	///,
--      SeeAlso => {"Dlocalize", "AnnFs", "Dintegration"}
--      }

doc ///
  Key
    DlocalizeMap
    (DlocalizeMap, Ideal, RingElement)
    (DlocalizeMap, Module, RingElement)
  Headline
    localization map from a $D$-module to its localization
  Usage
    DlocalizeMap(M, f)
    DlocalizeMap(I, f)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M = D/I$
    f:RingElement
      a polynomial
  Outputs
    :Matrix
      which represents the natural map from $M$ to $M_f = M[f^{-1}]$
  Description
    Text
      A supplementary function for @TO "Dlocalize"@
      that computes the localization map.
    Example
      W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
      M = W^1/(ideal(x*Dx+1, Dy))
      f = x^2-y^3
      DlocalizeMap(M, f)
  SeeAlso
    Dlocalize
    AnnFs
    Dintegration
///

doc ///
    Key
    	DlocalizeAll
	(DlocalizeAll,Ideal,RingElement)
	(DlocalizeAll,Module,RingElement)
	LocModule
	GeneratorPower
	LocMap
	annFS
	IntegrateBfunction
	Bfunction
    Headline
    	localization of a $D$-module (extended version)
    Usage
    	DlocalizeAll(M,f)
	DlocalizeAll(I,f)
    Inputs
    	M:Module
	    over the Weyl algebra $D$
	I:Ideal
	    which represents the module $M = D/I$
	f:RingElement
	    a polynomial
    Outputs
    	:HashTable
	    which contains the localized module $M_f = M[f^{-1}]$ and some additional information
    Description
    	Text
	    An extension of @TO Dlocalize@ that in addition computes the localization map,
	    the $b$-function, and the power $s$ of the generator $f^s$.

	    The keys of the output HashTable depend on which strategy is used. Common to each strategy
	    are the keys @TT "LocMap"@ and @TT "LocModule"@, which have the localization map
	    and the localized module, respectively; and @TT "GeneratorPower"@, which is an integer
	    $s$ such that (the images of) the generators of $M$ are $f^{-s}$ times the generators of $M_f$.

	    The other keys depend on @TT "Strategy"@:
	    @TT "IntegrateBfunction"@ holds the factored Bernstein-Sato polynomial
	    when using @TT "OTW"@ or @TT "OTWcyclic"@;
	    @TT "Bfunction"@ holds the same when using @TT "Oaku"@;
	    @TT "annFS"@ holds the annihilator ideal computed by @TO AnnFs@;
	    @TT "Boperator"@ holds the operator $P$ satisfying $P f^{s+1} = b(s) f^s$ (Oaku strategy).
	Example
	    W = makeWeylAlgebra(QQ[x,y])
	    M = W^1/ideal(x*dx + 1, dy)
	    f = x^2 - y^3
	    Mfall = DlocalizeAll(M, f)
	    gens image Mfall.LocMap == f^(-Mfall.GeneratorPower) * gens Mfall.LocModule
    SeeAlso
        Dlocalize
	AnnFs
	Dintegration
///
