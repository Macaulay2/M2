undocumented { "Increment" }

doc ///
Node
  Key
    PrimaryDecomposition
  Headline
    functions for primary decomposition
  Description
    Text
      This package provides computations with components of ideals and modules,
      including associated primes, radicals, and primary decompositions.
  References
    @UL {
	"Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 207-235 (1992)",
	"Shimoyama-Yokoyama, J. Symbolic computation, 22(3) 247-277 (1996)"
	}@
--  Acknowledgement
  Subnodes
    associatedPrimes
    localize
    primaryComponent
    primaryDecomposition
  SeeAlso
    "MinimalPrimes :: MinimalPrimes"
    "Colon :: Colon"

Node
  Key
    primaryDecomposition
   (primaryDecomposition, Ideal)
  Headline
    irredundant primary decomposition of an ideal
  Usage
    primaryDecomposition I
  Inputs
    I:Ideal
      in a (quotient of a) polynomial ring @TT "R"@
  Outputs
    :List
      containing a minimal list of primary ideals whose intersection is @TT "I"@
  Description
    Text
      This routine returns an irredundant primary decomposition for the ideal @TT "I"@.
      The specific algorithm used varies depending on the characteristics of the ideal,
      and can also be specified using the @TT "Strategy"@ option. In all cases, the radical
      of each entry of the output is equal to the corresponding entry of the output of @TO "associatedPrimes"@.

      Primary decomposition algorithms are very sensitive to the input. Some algorithms work very well on certain
      classes of ideals, but poorly on other classes. If this function seems to be taking too long, try another
      algorithm using @TO [primaryDecomposition, Strategy]@.
    Example
      R = QQ[a..i];
      I = permanents(2,genericMatrix(R,a,3,3))
      C = primaryDecomposition I;
      I == intersect C
      #C
    Text
      Recall that @TO (symbol/, List, Function)@ applies a function to each element of a
      list, returning the results as a list. This is often useful with lists of ideals,
      such as the list @TT "C"@ of primary components.
    Example
      C / toString / print;
      C / codim
      C / degree
    Text
      The corresponding list of associated prime ideals is cached
      and can be obtained by using @TO (associatedPrimes, Ideal)@.
    Example
      associatedPrimes I / print;
  Caveat
    -- FIXME
    The ground ring must be a prime field.
  SeeAlso
    (primaryDecomposition, Module)
    (associatedPrimes, Ideal)
    radical
    "MinimalPrimes :: minimalPrimes"
    topComponents
    removeLowestDimension

Node
  Key
   (primaryDecomposition, Module)
   (primaryDecomposition, Ring)
  Headline
    irredundant primary decomposition of a module
  Usage
    primaryDecomposition M
  Inputs
    M:Module
      in a (quotient of a) polynomial ring @TT "R"@
  Outputs
    :List
      containing a minimal list of primary submodules of @TT "M"@ whose intersection is @TT "0"@
  Description
    Text
      This routine returns a minimal primary decomposition for the zero submodule of @TT "M"@,
      i.e. a minimal list of submodules @TT "Q_i"@ of @TT "M"@ such that the intersection of all
      the @TT "Q_i"@ is @TT "0"@ and @TT "Ass(M/Q_i) = {p_i}"@ for some associated prime @TT "p_i"@ of @TT "M"@.

      Here minimality means that the associated primes of the submodules are pairwise distinct,
      and that the decomposition is irredundant, i.e. no submodule contains the intersection of the others.
      The {\tt i}-th element of this output is primary to the {\tt i}-th element of @TT "associatedPrimes M"@.
      The algorithm used is inspired by the Eisenbud-Huneke-Vasconcelos algorithm, modified to work for modules.
    Example
      R = QQ[x_0..x_3]
      (I1,I2,I3) = ({1,2,3},{2,3},{4,5}) / monomialCurveIdeal_R
      M = comodule I1 ++ comodule I2 ++ comodule I3
      associatedPrimes M
      C = primaryDecomposition M;
      netList C
      intersect C == 0 and all(C, isPrimary_M)
      C / degree
    Text
      Recall that in Macaulay2, a module is commonly represented as a @TO "subquotient"@, which is
      an ordered pair consisting of @TO generators@ and @TO relations@ represented as column matrices.
      As submodules of @TT "M"@, each module in the output list has the same relations as @TT "M"@,
      and has generators which are @TT "R"@-linear combinations of generators of @TT "M"@,
      where @TT "R = ring M"@.

      To obtain a primary decomposition of a submodule @TT "N"@, run this function on the quotient @TT "M/N"@.
      Note that the @TT "/"@ command does not check whether @TT "N"@ is actually a submodule of @TT "M"@, and
      a non-sensible result may be returned if this is not the case.
    -- Example
    --   N = coker map(M, R^1, transpose matrix{{1_R,1,1}}) -- coker of diagonal map
    --   primaryDecomposition N
    --   netList(oo/gens)
    Text
      This function generalizes primary decomposition of ideals (more precisely, cyclic modules),
      as can be seen by calling @TT "primaryDecomposition comodule I"@ for an ideal @TT "I"@.
      For convenience, one can also call @TT "primaryDecomposition R"@ for a ring @TT "R"@
      (which is most useful when @TT "R"@ is a @TO "QuotientRing"@).
      When computing primary decompositions of ideals with this function, remember to add back
      the original ideal to obtain the desired primary ideals, as in the following example.
    Example
      I = intersect((ideal(x_0..x_3))^5, (ideal(x_0..x_2))^4, (ideal(x_0..x_1))^3)
      S = R/I
      associatedPrimes S
      comps = primaryDecomposition S
      apply(comps, Q -> ideal mingens(I + ideal gens Q))
      I == intersect oo
    Text
      The results of the computation are stored in @TT "M.cache#\"primaryComponents\""@,
      which is a @TO "HashTable"@ whose keys are associated primes and values are the
      corresponding primary components.
      The list of all associated prime ideals is also cached can be obtained with @TT "associatedPrimes M"@.
      The computation may be interrupted at any point, and can be resumed later without recomputing already
      known primary components. To display detailed information throughout the computation, set the global variable
      @TO "debugLevel"@ to a value greater than 0, e.g. @TT "debugLevel=1"@ (or @TT "debugLevel=2"@ for even more detail).

      This function has one optional input @TT "Strategy"@, which accepts 3
      values that determine the algorithm for finding embedded components.

-- FIXME
      @UL {
	  {TT "Hom", PARA {"This strategy is closest to the original Eisenbud-Huneke-Vasconcelos method."}},
	  {TT "Res"},
	  {TT "Sat"},
	  }@

      While the default (and typically fastest) strategy is @TT "Sat"@, it is recommended to try different
      @TT "Strategy"@ values if the computation of a particular embedded component is taking too long.
      One can start the computation with one strategy, and interrupt and resume with a different strategy
      (even multiple times) if desired.
  Caveat
    Note that although isolated components (i.e. those corresponding to minimal primes) are unique,
    embedded components are never unique, and thus specifying generators of an embedded component requires
    non-canonical choices. For speed purposes, this algorithm searches for embedded components obtained by adding a
    bracket power of the embedded prime, with exponent determined by the degrees of generators of the embedded
    prime and @TT "ann M"@. In particular, the generators of an embedded component may not be of minimal possible degree.
  SeeAlso
    (primaryDecomposition, Ideal)
    (associatedPrimes, Module)
    isPrimary
    topComponents
    [primaryDecomposition, Strategy]

Node
  Key
    associatedPrimes
   (associatedPrimes, Ring)
   (associatedPrimes, Ideal)
   (associatedPrimes, Module)
  Headline
    find associated primes
  Usage
    associatedPrimes I
    ass I
  Inputs
    I:{Ring,Ideal,Module}
      a quotient ring, ideal, or module over a (quotient of a) polynomial ring @TT "R"@
  Outputs
    :List
      a list of the prime ideals in @TT "R"@ that are associated to @TT "I"@
  Description
    Text
      {\tt ass} is an abbreviation for @TT "associatedPrimes"@.

      Computes the list of associated primes for a module @TT "M"@ using Ext modules:
      the codimension @TT "i"@ associated primes of @TT "M"@ and @TT "Ext^i(M,R)"@ are identical,
      as shown in Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235.

      In some cases, @TO primaryDecomposition@ also computes the associated primes, in which case
      calling @TO associatedPrimes@ requires no new computation and the list of associated primes
      is in the same order as the list of primary components returned by @TO primaryDecomposition@.
      Conversely, calling @TO associatedPrimes@ beforehand will speed up the process of @TO (primaryDecomposition, Module)@.
    Example
      R = QQ[a..d]
      M = coker(transpose matrix{{1_R,1,1,1}} | diagonalMatrix vars R)
      associatedPrimes M
    Text
      For an ideal @TT "I"@, @TT "associatedPrimes I"@ is mathematically equivalent to @TT "associatedPrimes comodule I"@.
    Example
      I = intersect(ideal(a^2,b), ideal(a,b,c^5), ideal(b^4,c^4))
      associatedPrimes I
      associatedPrimes comodule I
    Text
      For a quotient ring @TT "R"@, @TT "associatedPrimes R"@ is equivalent to @TT "associatedPrimes ideal R"@,
      the associated primes of the defining ideal of @TT "R"@.
    Example
      R = QQ[x,y,z]/(x^2,x*y)
      associatedPrimes R
    Text
      If the ideal is @ofClass MonomialIdeal@, then a more efficient strategy written by Greg Smith
      and Serkan Hosten is used. The above comments about primary decomposition hold in this case too.
    Example
      R = QQ[a..f];
      I = monomialIdeal ideal"abc,bcd,af3,a2cd,bd3d,adf,f5"
      ass I
      primaryDecomposition I
    Text
      The list of associated primes corresponds to the list of primary components of @TT "I"@:
      the {\tt i}-th associated prime is the radical of the {\tt i}-th primary component.
    Text
      If a value to the option @TT "CodimensionLimit"@ is provided, then only associated primes
      of codimension at most this value are returned. This can save time if the big height
      (= maximal codimension of an associated prime) is less than the projective dimension. ",
      "Calling ", TT "associatedPrimes M", " with a different value of ", TO CodimensionLimit, "
      will remember the primes already found, only performing further computation if necessary.
      The default value (which is the same as not specifying the value) is -1, which is
      equivalent to ", TT "CodimensionLimit => infinity", " the first time ",
      TT "associatedPrimes M", " is called, but will not do any further computation on subsequent
      runs, only returning the previously found primes. To force computation of all associated
      primes after some have been already found, use ", TT "CodimensionLimit => infinity", ".

      Original author (for ideals): @HREF {"http://faculty.mercer.edu/yackel_ca/", "C. Yackel"}@.
      Updated for modules by J. Chen.
  SeeAlso
    (primaryDecomposition, Ideal)
    (primaryDecomposition, Module)
    "radical"
    "MinimalPrimes :: minimalPrimes"
    "topComponents"
    "removeLowestDimension"

Node
  Key
    localize
   (localize, Ideal, Ideal)
  Headline
    localize an ideal at a prime ideal
  Usage
    localize(I, P)
  Inputs
    I:Ideal
      an ideal in a (quotient of a) polynomial ring @TT "R"@
    P:Ideal
      a prime ideal in the same ring
  Outputs
    :Ideal
      the extension contraction ideal $I R_P \cap R$.
  Description
    Text
      The result is the ideal obtained by first extending to the
      localized ring and then contracting back to the original ring.
    Example
      R = ZZ/(101)[x,y];
      I = ideal (x^2,x*y);
      P1 = ideal (x);
      localize(I,P1)
      P2 = ideal (x,y);
      localize(I,P2)
    Example
      R = ZZ/31991[x,y,z];
      I = ideal(x^2,x*z,y*z);
      P1 = ideal(x,y);
      localize(I,P1)
      P2 = ideal(x,z);
      localize(I,P2)
    Text
      Authored by @HREF {"http://faculty.mercer.edu/yackel_ca", "C. Yackel"}@. Last modified June, 2000.
  Caveat
    The ideal {\tt P} is not checked to be prime.
  Subnodes
    [localize, Strategy]
  SeeAlso
    (primaryDecomposition, Ideal)
    radical
    "MinimalPrimes :: minimalPrimes"
    topComponents
    removeLowestDimension

Node
  Key
    [localize, Strategy]
  Description
    Text
      The strategy option value should be one of the following, with default value 1.

      @UL{
	  LI (TT "Strategy => 0", " -- Uses the algorithm of Eisenbud-Huneke-Vasconcelos",
	      PARA {
		  "This strategy does not require the calculation of the assassinator, but can
		  require the computation of high powers of ideals. The method appears in
		  Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235."}),
	  LI (TT "Strategy => 1", " -- Uses a separator to find the localization",
	      PARA {
		  "This strategy uses a separator polynomial - a polynomial in all of the associated primes
		  of {\tt I} but {\tt P} and those contained in {\tt P}.
		  In this strategy, the assassinator of the ideal will be recalled, or recomputed using ",
		  TO2 {[associatedPrimes, Strategy], TT "Strategy => 1"}, " if unknown. The separator
		  polynomial method is described in Shimoyama-Yokoyama, J. Symbolic computation, 22(3) 247-277 (1996).
		  This is the same as ", TT "Strategy => 1", " except that, if unknown, the assassinator
		  is computed using ", TO2 {[associatedPrimes, Strategy], TT "Strategy => 2"}, "."}),
	  LI (TT "Strategy => 2", " -- Uses a separator to find the localization")
	  }@

Node
  Key
    primaryComponent
   (primaryComponent, Ideal, Ideal)
   [primaryComponent, Strategy]
   [primaryComponent, Increment]
  Headline
    find a primary component corresponding to an associated prime
  Usage
    Q = primaryComponent(I, P)
  Inputs
    I:Ideal
      an ideal in a (quotient of a) polynomial ring {\tt R}
    P:Ideal
      an associated prime of {\tt I}
  Outputs
    Q:Ideal
      a {\tt P}-primary ideal of {\tt I}
  Description
    Text
      The output {\tt Q} is @TT "topComponents(I + P^m)"@ for sufficiently large {\tt m}.
      The criterion that {\tt Q} is primary is given in Eisenbud-Huneke-Vasconcelos,
      Invent. Math. 110 (1992) 207-235. However, we use @TO (localize, Ideal, Ideal)@.

      The @TT "Strategy"@ option value sets the strategy option for @TO localize@, and should be one of the following:
      -- TODO: The default value is 2.

      @UL{
	  LI ("Strategy => 0", " -- Uses ", TT "localize", " Strategy 0"),
	  LI ("Strategy => 1", " -- Uses ", TT "localize", " Strategy 1"),
	  LI ("Strategy => 2", " -- Uses ", TT "localize", " Strategy 2")
	  }@

      The @TT "Increment"@ option value should be an integer. The algorithm given in
      Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235, relies on @TT "topComponents(I + P^m)"@
      for $m$ sufficiently large. The algorithm begins with $m = 1$, and increases $m$ by the value
      of the @TT "Increment"@ option until @TT "m"@ is sufficiently large. The default value is 1.

      Authored by @HREF {"http://faculty.mercer.edu/yackel_ca", "C. Yackel"}@. Last modified June, 2000.
  SeeAlso
    (associatedPrimes, Ideal)
    (primaryDecomposition, Ideal)
    radical
    "MinimalPrimes :: minimalPrimes"
    topComponents
    removeLowestDimension

Node
  Key
    isPrimary
   (isPrimary, Module, Module)
   (isPrimary, Ideal)
   (isPrimary, Ideal, Ideal)
  Headline
    determine whether a submodule is primary
  Usage
    isPrimary Q
    isPrimary(Q, P)
    isPrimary(M, Q)
  Inputs
    Q:{Ideal,Module}
      the submodule or ideal to be checked for being primary
    P:Ideal
      the @TO "radical"@ of @TT "Q"@
    M:Module
      the ambient module
  Outputs
    :Boolean
      true if @TT "Q"@ is primary, false otherwise
  Description
    Text
      Checks to see if a given submodule @TT "Q"@ of a module @TT "M"@ is primary,
      i.e. whether or not @TT "M/Q"@ has exactly one associated prime (which is equivalent for finitely
      generated modules over Noetherian rings). If the input is a single ideal, then the ambient module
      is taken to be the ring (i.e. the free module of rank 1), and does not need to be specified.
    Example
      Q = ZZ/101[x,y,z]
      isPrimary ideal(y^6)
      isPrimary(ideal(y^6), ideal(y))
      isPrimary ideal(x^4, y^7)
      isPrimary ideal(x*y, y^2)
  SeeAlso
   (primaryDecomposition, Ideal)
   (primaryDecomposition, Module)
    associatedPrimes
///

-- FIXME
document {
     Key => {[primaryDecomposition,Strategy],EisenbudHunekeVasconcelos,ShimoyamaYokoyama,Hybrid,GTZ},
     "The strategy option value should be one of the following.",
     UL {
          ("Monomial", " -- uses Alexander duality of a monomial ideal"),
	  ("Binomial", " -- finds a cellular resolution of a
	                     binomial ideal.  NOT IMPLEMENTED YET."),
	  ("EisenbudHunekeVasconcelos", " -- uses the algorithm of Eisenbud-Huneke-Vasconcelos"),
	  ("ShimoyamaYokoyama", " -- uses the algorithm of Shimoyama-Yokoyama"),
	  ("Hybrid"," -- uses parts of the above two algorithms"),
	  ("GTZ", " -- uses the algorithm of Gianni-Trager-Zacharias.
	           NOT IMPLEMENTED YET."),
          (///"Hom"///, " -- only used in embedded component selection for modules"),
          (///"Sat"///, " -- only used in embedded component selection for modules"),
          (///"Res"///, " -- only used in embedded component selection for modules"),
          },
     "The default strategy depends on the ideal.  If the ideal is generated
     by monomials, then ", TT "Strategy => Monomial", " is implied.
     In all other cases (for ideals), the default is ", TT "Strategy => ShimoyamaYokoyama", ".",
     HEADER3 "Strategy => Monomial",
     "This strategy only works for monomial ideals, and is the default strategy for such ideals.  See the chapter
     \"Monomial Ideals\" in the Macaulay2 book.",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => Monomial)
     ///,
     HEADER3 "Strategy => EisenbudHunekeVasconcelos",
     "See \"Direct methods for primary decomposition\" by Eisenbud, Huneke, and Vasconcelos, Invent. Math. 110, 207-235 (1992).",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => EisenbudHunekeVasconcelos)
     ///,
     HEADER3 "Strategy => ShimoyamaYokoyama",
     "This strategy is the default for non-monomial ideals.  See \"Localization and Primary Decomposition of Polynomial ideals\" by Shimoyama and Yokoyama, J. Symb. Comp. 22, 247-277 (1996).",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => ShimoyamaYokoyama)
     ///,
     HEADER3 "Strategy => Hybrid",
     "Use a hybrid of the Eisenbud-Huneke-Vasconcelos and Shimoyama-Yokoyama strategies.  The field ",
     TT "Strategy", " is a list of two integers, indicating the strategy to use for finding associated primes and localizing, respectively. ",
     "WARNING: Setting the second paramter to 1 works only if the ideal is homogeneous and equidimensional.",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = intersect(ideal(x^2), ideal(y^2))
	  primaryDecomposition(I, Strategy => new Hybrid from (1,1))
	  primaryDecomposition(I, Strategy => new Hybrid from (1,2))
	  primaryDecomposition(I, Strategy => new Hybrid from (2,1))
	  primaryDecomposition(I, Strategy => new Hybrid from (2,2))
     ///,
     HEADER3 concatenate("Strategy => ", ///"Hom"///, ", Strategy => ", ///"Sat"///, ", Strategy => ", ///"Res"///),
     "These strategies are only valid for the case of primary decomposition of modules ",
     "(and only relevant for embedded components) - see ",
     TO (primaryDecomposition, Module), " for more information.",
     }

document { Key => {(irreducibleDecomposition,MonomialIdeal),irreducibleDecomposition},
     Headline => "express a monomial ideal as an intersection of irreducible monomial ideals",
     Usage => "irreducibleDecomposition I",
     Inputs => { "I" },
     EXAMPLE lines ///
        QQ[x..z];
        I = monomialIdeal (x*y^3, x*y^2*z)
	w = irreducibleDecomposition I
	assert( I == intersect w )
     ///,
     Outputs => {{ "a list of irreducible monomial ideals whose intersection is ", TT "I" }}}

document {
     Key => {kernelOfLocalization,(kernelOfLocalization,Module,Ideal)},
     Headline => "the kernel of the localization map",
     Usage => "kernelOfLocalization(M, P)",
     Inputs => {
	  "M" => Module,
	  "P" => Ideal => "the prime ideal to localize at"
	  },
     Outputs => {
	  Module => {"the kernel of the localization map ", TT "M -> M_P"}
	  },
     "This method computes the kernel of the natural map from a module to its localization at a
     given prime ideal. The efficiency of this method is intimately tied to the efficiency of
     computation of associated primes for the module - if the associated primes of ", TT "M",
     " have previously been computed, then this method should finish quickly.",
     PARA{},
     EXAMPLE lines ///
          R = QQ[x_0..x_3]
	  (I1,I2,I3) = ({1,2,3},{2,3},{4,5})/monomialCurveIdeal_R
          M = comodule I1 ++ comodule I2 ++ comodule I3
          elapsedTime kernelOfLocalization(M, I1)
          elapsedTime kernelOfLocalization(M, I2)
          elapsedTime kernelOfLocalization(M, I3)
     ///,
     SeeAlso => {(associatedPrimes, Module), (primaryDecomposition, Module)}
     }

document {
     Key => {radicalContainment,(radicalContainment,RingElement,Ideal),[radicalContainment,Strategy]},
     Headline => "whether an element is contained in the radical of an ideal",
     Usage => "radicalContainment(g, I)",
     Inputs => {
	  "g" => RingElement,
	  "I" => Ideal
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "g", " is in the radical of ", TT "I", ", and ",
	       TO "false", " otherwise"}
	  },
     "This method determines if a given element ", TT "g", " is contained in the radical of a given ",
     "ideal ", TT "I", ". There are 2 algorithms implemented for doing so: the first (default) uses the
     Rabinowitsch trick in the proof of the Nullstellensatz, and is called with ",
     TT ///Strategy => "Rabinowitsch"///, ". The second algorithm, for homogeneous ideals,
     uses a theorem of Kollar to obtain an effective upper bound on the required power to
     check containment, together with repeated squaring, and is called with ",
     TT ///Strategy => "Kollar"///, ". The latter algorithm is generally quite fast if a Grobner basis
     of ", TT "I", " has already been computed. A recommended way to do so is to check
     ordinary containment, i.e. ", TT "g % I == 0", ", before calling this function.",
     PARA{},
     EXAMPLE lines ///
     	  d = (4,5,6,7)
          n = #d
          R = QQ[x_0..x_n]
          I = ideal homogenize(matrix{{x_1^(d#0)} | apply(toList(1..n-2), i -> x_i - x_(i+1)^(d#i)) | {x_(n-1) - x_0^(d#-1)}}, x_n)
          D = product(I_*/degree/sum)
          x_0^(D-1) % I != 0 and x_0^D % I == 0
          elapsedTime radicalContainment(x_0, I)
          elapsedTime radicalContainment(x_0, I, Strategy => "Kollar")
          elapsedTime radicalContainment(x_n, I, Strategy => "Kollar")
     ///,
     SeeAlso => {radical}
     }

document {
     Key => {regSeqInIdeal,(regSeqInIdeal,Ideal),(regSeqInIdeal,Ideal,ZZ),(regSeqInIdeal,Ideal,ZZ,ZZ,ZZ),[regSeqInIdeal,Strategy]},
     Headline => "a regular sequence contained in an ideal",
     Usage => concatenate("regSeqInIdeal I\n", "regSeqInIdeal(I, n)\n", "regSeqInIdeal(I, n, c, t)"),
     Inputs => {
	  "I" => Ideal,
	  "n" => ZZ => "the length of the regular sequence returned",
          "c" => ZZ => {"the codimension of ", TT "I", ", if known"},
          "t" => ZZ => "a limit on the time spent (in seconds) for each trial"
	  },
     Outputs => {
	  Ideal => {"generated by a regular sequence of length ", TT "n", " contained in ", TT "I"}
	  },
     "This method computes a regular sequence of length ", TT "n", " contained in a given ideal ",
     TT "I", ". It attempts to do so by first trying ", ///"sparse"///, " combinations of the generators, ",
     " i.e. elements which are either generators or sums of two generators. If a sparse ",
     "regular sequence is not found, then dense combinations of generators will be tried.",
     PARA{},
     "If the length ", TT "n", " is either unspecified or greater than the codimension of ", TT "I",
     " then it is silently replaced with the codimension of ", TT "I", ". The ideal ", TT "I", " should be ",
     "in a polynomial (or at least Cohen-Macaulay) ring, so that ", TT "codim I = grade I", ".",
     PARA{},
     EXAMPLE lines ///
          R = QQ[x_0..x_7]
          I = intersect(ideal(x_0,x_1,x_2,x_3), ideal(x_4,x_5,x_6,x_7), ideal(x_0,x_2,x_4,x_6), ideal(x_1,x_3,x_5,7))
          elapsedTime regSeqInIdeal I
     ///,
     PARA{},
     "If ", TT "I", " is the unit ideal, then an ideal of variables of the ring is returned.",
     PARA{},
     "If the codimension of ", TT "I", " is already known, then one can specify this, along with a ",
     "time limit for each trial (normally this is taken from the length of time for computing codim I). ",
     "This can result in a significant speedup: in the following example, ", TT "codim I",
     " takes > 1 minute to complete.",
     PARA{},
     EXAMPLE lines ///
     	  R = QQ[h,l,s,x,y,z]
          I = ideal(h*l-l^2-4*l*s+h*y,h^2*s-6*l*s^3+h^2*z,x*h^2-l^2*s-h^3,h^8,l^8,s^8)
          isSubset(I, ideal(s,l,h)) -- implies codim I == 3
          elapsedTime regSeqInIdeal(I, 3, 3, 1)
     ///
     -- SeeAlso => {radical}
     }
