undocumented {
    (NewMethod, Page),
    (degree, Page),
    (describe, Page),
    (describe, PageMap),
    (describe, SpectralSequence),
    (expression, SpectralSequence),
    (net, FilteredComplex),
    (net, Page),
    (net, PageMap),
    (net, SpectralSequence),
    (net, SpectralSequencePage),
    (page, Page),
    (page, SpectralSequencePage),
    (ring, Page),
    (spots, PageMap),
    (support, FilteredComplex),
    (support, PageMap),
    (support, SpectralSequencePage),
    (symbol _, Page, List),
    (symbol _, PageMap, List),
    targetPruningMap,
    sourcePruningMap,
    }

doc ///
Node
  Key
    SpectralSequences
  Headline
    a package for working with filtered complexes and spectral sequences
  Description
    Text
      Spectral sequences, although notoriously technical, are very useful in applications,
      especially when they degenerate quickly. By contrast, little is known about their general structure
      when they fail to degenerate quickly. Even in cases when the terms in the spectral sequences are well
      understood, the maps remain mysterious. One of the motivations behind this package is to shed light on
      spectral sequences through examples. Its purpose is to allow for effective calculations of particular
      kinds of spectral sequences.

      As one general situation, which illustrates some capabilities of this package, let $k$ be a computable field,
      $S$ a $k$-algebra of finite type, $C$ a bounded chain complex of finitely generated $S$-modules,
      and $FC$ a bounded ascending filtration of $C$.  This package is capable of computing, under these assumptions,
      the spectral sequence determined by $FC$ along with its differentials.

    Tree
      :Main types and methods introduced in this package
        > "FilteredComplex"
	> "SpectralSequence"
	> "Page"
	  > "SpectralSequencePage"
	> "PageMap"
	  > "SpectralSequencePageMap"

    Text
      Here is a list of some examples which illustrate various parts of this package.
    Tree
      :Constructors used in this package
        > "How to make filtered complexes from chain complex maps"
	--> "How to work with filtered complexes"
	--> "Making filtered chain complexes from chain complex maps"
        > "Filtrations and tensor product complexes"
        > "Filtrations and homomorphism complexes"
        > "I-adic filtrations of chain complexes and their spectral sequences"
        --> "Spectral sequences from filtered chain complexes"
	:Examples involving simplicial complexes
	  -- TODO: combine these examples
          > "Filtered complexes and simplicial complexes"
	  > "Filtered complexes and spectral sequences from simplicial complexes"
	  > "Example 1"
	  > "Example 2"
      :Other examples which illustrate this package
        > "Computing the Serre Spectral Sequence associated to a Hopf Fibration"
        > "Balancing Tor"
        > "Spectral sequences and hypercohomology calculations"
        > "Spectral sequences and connecting morphisms"
        > "Spectral sequences and non-Koszul syzygies"
        > "A spectral sequence which fails to degenerate quickly"
        > "Seeing Cancellations"
        > "Edge homomorphisms"
        > "Examples of change of rings Spectral Sequences"
      :More easy topological examples
        > "Identifying anti-podal points of the two sphere"
	--> "The quotient map S^2 --> RP^2"
	--> "More topological examples"
        > "The fibration of the Klein Bottle over the sphere with fibers the sphere"
	--> "The fibration S^1 --> Klein Bottle --> S^1"
        > "The trivial fibration over the sphere with fibers the sphere"
        --> "S^1 --> S^1 x S^1 --> S^1"

  -- Contributors
  --   The following people have generously contributed code or worked on our code.
  --   @UL {
  -- 	HREF("",""},
  -- 	HREF("",""}
  -- 	}@
///

--------------------------------------------
-- FilteredComplex
--------------------------------------------

doc ///
Node
  Key
    FilteredComplex
  Headline
    the type of all filtered complexes
  Description
    Text
      An ascending filtration of a bounded (homological, lower index, or degree $-1$) chain complex
      $C : \cdots \rightarrow C_i \rightarrow C_{i - 1} \rightarrow \cdots$
      is an ordered family of chain subcomplexes
      $FC : \cdots \subseteq F_{n - 1} C \subseteq F_n C \subseteq \cdots $.
      Such a filtration is said to be bounded if $F_s C = C$ for all sufficiently
      large $s$ and $F_t C = 0$ for all sufficiently small $t$.

      Alternatively, a descending filtration of a bounded (cohomological, or upper index, or degree $1$) chain complex
      $C : \cdots  \rightarrow C^i \rightarrow C^{i + 1} \rightarrow \cdots $
      is an ordered family of subchain complexes
      $FC : \cdots \subseteq F^{n + 1} C \subseteq F^n C \subseteq \cdots$.
      Such a filtration is said to be bounded if $F^s C = 0$ for all sufficiently
      large $s$ and $F^t C = C$ for all sufficiently small $t$.

      The type {\tt FilteredComplex} is a data type for working with bounded filtrations of bounded chain complexes.
    Tree
      :Methods involving filtered complexes
        > "filteredComplex(List)"
        > "filteredComplex(Complex)"
        > "filteredComplex(Ideal,Complex,ZZ)"
	> "FilteredComplex ^ ZZ"
	> "min(FilteredComplex)"
	> "max(FilteredComplex)"
	> "associatedGradedHomologyObject"
	> "filteredHomologyObject"
	> "inducedMap(FilteredComplex,ZZ)"
	> "spots(FilteredComplex)"
	> "complex(FilteredComplex)"
	> "Complex ** FilteredComplex"
	> "Hom(FilteredComplex,Complex)"
  Caveat
    By assumption all filtered complexes arise from bounded filtrations of bounded chain complexes.  Filtrations on degree $-1$
    chain complexes are ascending.  Filtrations on degree $1$ chain complexes are
    descending.
  SeeAlso
    "How to make filtered complexes from chain complex maps"
    "Filtered complexes and simplicial complexes"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"

-- TODO: merge
Node
  Key
     filteredComplex
    (filteredComplex, List)
    [filteredComplex, ReducedHomology]
    [filteredComplex, Shift]
    Shift
    ReducedHomology
  Headline
    construct a filtered complex from a filtration
  Usage
    K = filteredComplex L
  Inputs
    L: List
    ReducedHomology => Boolean
    Shift => ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
      We can make a filtered complex from a list of chain complex maps as follows.
      We first need to load the relevant packages.
    Example
      needsPackage "SpectralSequences"
    Text
      We then make a chain complex.
    Example
      R = QQ[x,y,z,w]
      d2 = matrix(R,{{1},{0}})
      d1 = matrix(R,{{0,1}})
      C = complex({d1,d2})
    Text
      We now make the modules of the another chain complex which we will label D.
    Example
      D_2 = image matrix(R,{{1}})
      D_1 = image matrix(R,{{1,0},{0,0}})
      D_0 = image matrix(R,{{1}})
      D = complex({inducedMap(D_0,D_1,C.dd_1),inducedMap(D_1,D_2,C.dd_2)})
    Text
      Now make a chain complex map.
    Example
      d = map(C,D,{inducedMap(C_0,D_0,id_(C_0)),inducedMap(C_1,D_1,id_(C_1)),inducedMap(C_2,D_2,id_(C_2))})
      isWellDefined d
    Text
      We now make the modules of another chain complex which we will label E.
    Example
      E_2 = image matrix(R,{{0}})
      E_1 = image matrix(R,{{1,0},{0,0}})
      E_0 = image matrix(R,{{1}})
      E = complex({inducedMap(E_0,E_1,C.dd_1),inducedMap(E_1,E_2,C.dd_2)})
    Text
      Now make a chain complex map.
    Example
      e = map(C,E,{inducedMap(C_0,E_0,id_(C_0)),inducedMap(C_1,E_1,id_(C_1)),inducedMap(C_2,E_2,id_(C_2))})
      isWellDefined e
    Text
      Now make a filtered complex from a list of chain complex maps.
    Example
      K = filteredComplex({d,e})
    Text
      We can make a filtered complex, with a specified minimum filtration degree
      from a list of ComplexMaps by using the Shift option.
    Example
      L = filteredComplex({d,e},Shift => 1)
      M = filteredComplex({d,e},Shift => -1)
    Text
      Alternatively, we can make a filtered complex from a nested list of simplicial complexes as follows.
    Example
      D = simplicialComplex {x*y*z, x*y, y*z, w*z}
      E = simplicialComplex {x*y, w}
      F = simplicialComplex {x,w}
      K = filteredComplex{D,E,F}
    Text
      If we want the resulting complexes to correspond to the non-reduced homology
      of the simplicial complexes we can do the following.
    Example
      filteredComplex({D,E,F}, ReducedHomology => false)
  SeeAlso
    FilteredComplex
    "Complexes :: Making maps between chain complexes"
    "How to make filtered complexes from chain complex maps"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
    "Filtered complexes and simplicial complexes"

Node
  Key
    (filteredComplex, Complex)
  Headline
      obtain a filtered complex from a chain complex
  Usage
      K = filteredComplex C
  Inputs
    C: Complex
  Outputs
    K: FilteredComplex
  Description
    Text
       Produces the filtered complex obtained by successively truncating the complex.
    Example
      A = QQ[x,y]
      C = koszulComplex vars A
      K = filteredComplex C
  SeeAlso
     (naiveTruncation, Complex, ZZ, ZZ)

Node
  Key
    (filteredComplex, SpectralSequence)
  Headline
      obtain the filtered complex associated to the spectral sequence
  Usage
      K = filteredComplex E
  Inputs
    E: SpectralSequence
  Outputs
    K: FilteredComplex
  Description
    Text
       Produces the filtered complex which determined the spectral sequence.
       Consider the spectral sequence $E$ which arises from a nested list of simplicial
       complexes.
    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D;
      F1D = simplicialComplex {a*c, d};
      F0D = simplicialComplex {a,d};
      K = filteredComplex {F2D, F1D, F0D};
      E = spectralSequence(K) ;
    Text
      The underlying filtered chain complex
      can be recovered from the
      spectral sequence by:
    Example
      C = filteredComplex E
      complex C
      C_infinity
  SeeAlso
     (complex, FilteredComplex)
     (symbol ^, FilteredComplex, InfiniteNumber)

Node
  Key
    (filteredComplex, Ideal, Complex, ZZ)
  Headline
    I-adic filtrations of chain complexes
  Usage
    K = filteredComplex(I,C,n)
  Inputs
    I: Ideal
    C: Complex
    n: ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
      By multiplying a chain complex by successive powers of an ideal we obtain a filtered complex.
    Example
      S = QQ[a..d]
      J = ideal vars S
      C = res monomialCurveIdeal(S,{1,3,4})
      K = filteredComplex(J,C,2)
    Text
      Here are higher some pages of the associated spectral sequence:
    Example
      E = prune spectralSequence K
      E^2
      E^3
      assert all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E, j#0, j#1, 0))
      assert all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E, j#0, j#1, 1))
      assert all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E, j#0, j#1, 2))
      assert all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E, j#0, j#1, 3))
      assert all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E, j#0, j#1, 4))

Node
  Key
    (symbol ^, FilteredComplex, ZZ)
    (symbol ^, FilteredComplex, InfiniteNumber)
    (symbol _, FilteredComplex, ZZ)
    (symbol _, FilteredComplex, InfiniteNumber)
  Headline
    retrieve the filtered pieces of a filtered complex
  Usage
    C = K ^ j
    C = K _ j
  Inputs
    K:FilteredComplex
    j:ZZ
      an integer, infinity, or -infinity
  Outputs
    C:Complex
  Description
    Text
      The notation @TT "K_j"@ returns the chain complex in homological filtration degree j.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A;
      K = filteredComplex C
      K_0
      K_1
      K_2
      K_infinity
      K_(-infinity)
    Text
      The notation @TT "K^j"@ teturns the chain complex in cohomological filtration degree j.
    Example
      K^0
      K^(-1)
      K^(-2)
      K^(-infinity)
      K^infinity
    Text
      The relationship $K ^ j = K _{(-j)}$ holds.
    Example
      K_1 === K^(-1)
      K_2 === K^(-2)
      K_infinity === K^(-infinity)
      K^infinity === K_(-infinity)

Node
  Key
     (symbol **, Complex, FilteredComplex)
     (symbol **, FilteredComplex, Complex)
  Headline
    filtered tensor product of complexes
  Usage
    KK = C ** K
    KK = K ** C
  Inputs
    C:Complex
    K:FilteredComplex
  Outputs
    KK:FilteredComplex
  Description
    Text
      Returns the two filtrations of the tensor product complex determined by
      the double complex.
      The following example illustrates the syntax.
    Example
        A = QQ[x,y];
        B = koszulComplex vars A;
        C = koszulComplex vars A;
        F' = (filteredComplex B) ** C
        F'' = B ** (filteredComplex C)
  SeeAlso
    "Filtrations and tensor product complexes"

Node
  Key
    (inducedMap, FilteredComplex, ZZ)
  Headline
    the i th inclusion map in a filtered complex
  Usage
    f = inducedMap(K,i)
  Inputs
    K:FilteredComplex
    i:ZZ
  Outputs
    f:ComplexMap
  Description
    Text
      Returns the chain complex map specifying the inclusion of the i piece
      of the filtered
      complex to the ambient chain complex.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A;
      K = filteredComplex C
      inducedMap(K,1)

Node
  Key
    (complex, FilteredComplex)
  Headline
    the ambient chain complex of a filtered complex
  Usage
    C = complex K
  Inputs
    K:FilteredComplex
  Outputs
    C:Complex
  Description
    Text
      Returns the ambient chain complex of the filtered complex.
    Example
        A = QQ[x,y];
        C = koszulComplex vars A
        K = filteredComplex C;
        complex K
        K_infinity
  SeeAlso
    (symbol _, FilteredComplex, InfiniteNumber)

Node
  Key
     (Hom, FilteredComplex, Complex)
     (Hom, Complex, FilteredComplex)
  Headline
    the filtered Hom complex
  Usage
    f = Hom(K,C)
  Inputs
    K:FilteredComplex
    C:Complex
  Outputs
    f:FilteredComplex
  Description
    Text
        Returns the filtrations of the Hom complex determined by the double complex.  Here is
        an example which illustrates the syntax.
    Example
       A = QQ[x,y,z,w];
       B = res monomialCurveIdeal(A, {1,2,3});
       C = res monomialCurveIdeal(A, {1,3,4});
       F' = Hom(filteredComplex B, C)
       F'' = Hom(B,filteredComplex C)
  SeeAlso
      "Filtrations and tensor product complexes"

Node
  Key
    filteredHomologyObject
   (filteredHomologyObject, ZZ, ZZ, FilteredComplex)
  Headline
    compute the filtered homology object
  Usage
    M = filteredHomologyObject(ZZ, ZZ, FilteredComplex)
  Inputs
    p:ZZ
    n:ZZ
    K:FilteredComplex
  Outputs
    M:Module
  Description
    Text
      Computes the filtered homology object determined by the filtered chain complex
  SeeAlso
    (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)

Node
  Key
    associatedGradedHomologyObject
   (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)
  Headline
    compute the associated graded homology object
  Usage
    M = associatedGradedHomologyObject(ZZ, ZZ, FilteredComplex)
  Inputs
    p:ZZ
    n:ZZ
    K:FilteredComplex
  Outputs
    M:Module
  Description
    Text
      Computes the associated graded homology object determined by the filtered chain complex

Node
  Key
   (spots, FilteredComplex)
  Headline
    which spots does the given filtered complex has a module.
  Usage
    s = spots L
  Inputs
    L:FilteredComplex
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given filtered complex has a module.

Node
  Key
   (max, FilteredComplex)
  Headline
    maximum spot where the given filtered complex has a module.
  Usage
    m = max L
  Inputs
    L:FilteredComplex
  Outputs
    m:ZZ
  Description
    Text
      Returns the maximum spot where the given filtered complex has a module.

Node
  Key
   (min, FilteredComplex)
  Headline
    minimum spot where the given filtered complex has a module.
  Usage
    m = min L
  Inputs
    L:FilteredComplex
  Outputs
    m:ZZ
  Description
    Text
      Returns the minimum spot where the given filtered complex has a module.
///

--------------------------------------------
-- SpectralSequence
--------------------------------------------

doc ///
Node
  Key
    SpectralSequence
  Headline
    the type of all spectral sequences
  Description
    Text
      A (homological, or lower index) spectral sequence consists of:

      @OL{
	  "A sequence of modules $\\{E^r_{p,q}\\}$ for $p,q \\in \\ZZ$ and $r \\geq 0$;",
	  "A collection of homomorphisms $\\{d^r_{p,q}: E^r_{p,q} \\rightarrow E^r_{p-r,q+r-1} \\}$, for $p,q \\in \\ZZ$ and $ r \\geq 0$, such that $d^r_{p,q} d^r_{p+r,q-r+1} = 0$;",
	  "A collection of isomorphisms $E^{r+1}_{p,q}  \\rightarrow  \\ker d^r_{p,q} / \\operatorname{image} d^r_{p+r,q-r+1}$."
	  }@

      Alternatively a (cohomological, or upper index) spectral sequence consists of:

      @OL{
	  "A sequence of modules $\\{E_r^{p,q}\\}$ for $p,q \\in \\ZZ$, and $r \\geq 0$;",
	  "A collection of homomorphisms $\\{d_r^{p,q}: E_r^{p,q} \\rightarrow E_{r}^{p+r,q-r+1}\\}$ for $p,q \\in \\ZZ, r \\geq 0$ such that $d_r^{p,q} d_r^{p-r,q+r-1} = 0$;",
	  "A collection of isomorphisms $E_{r+1}^{p,q}  \\rightarrow \\ker d_r^{p,q} / \\operatorname{image} d_r^{p-r,q+r-1}$."
	  }@

      The type {\tt SpectralSequence} is a data type for working with spectral sequences.
      In this package, a spectral sequence is represented by a sequence of spectral sequence pages.
    Tree
      :Methods involving spectral sequences
	> "spectralSequence(FilteredComplex)"
	> "SpectralSequence ^ ZZ"
	> "minimalPresentation(SpectralSequence)"
	> "homologyIsomorphism(SpectralSequence,ZZ,ZZ,ZZ)"
        > "filteredComplex(SpectralSequence)"
	> "complex(SpectralSequence)"
  Caveat
    All spectral sequences arise from bounded filtrations of bounded chain complexes.  Ascending filtrations of degree $-1$ chain complexes
    determine spectral sequences of the first type.  Descending filtrations of degree $1$ chain complex determine spectral sequences of the second type.
  SeeAlso
    SpectralSequencePage
    SpectralSequencePageMap
    "Filtered complexes and simplicial complexes"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
///

doc ///
  Key
    spectralSequence
   (spectralSequence, FilteredComplex)
   [spectralSequence, Prune]
  Headline
    construct a spectral sequence from a filtered complex
  Usage
    E = spectralSequence K
  Inputs
    K:FilteredComplex
  Outputs
    E:SpectralSequence
  Description
    Text
      This method constructs the spectral sequence associated to a filtered complex.

      In the example below we construct a spectral sequence $E$ from the filtered complex $K$.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,2);
      E = spectralSequence K
    Text
      To view pages or maps we proceed as follows.
    Example
      E^2
      support E^2 .dd
      E^2 .dd _{0,1}
      E^infinity
  SeeAlso
    SpectralSequence
    SpectralSequencePage
    (symbol ^, SpectralSequence, ZZ)
    (spectralSequence, FilteredComplex)
///

doc ///
  Key
    (symbol ^, SpectralSequence, ZZ)
    (symbol ^, SpectralSequence, InfiniteNumber)
    (symbol _, SpectralSequence, ZZ)
    (symbol _, SpectralSequence, InfiniteNumber)
  Headline
    retrieve the k-th page of a spectral sequence
  Usage
    P = E^k
    P = E_k
  Inputs
    E:SpectralSequence
    k:ZZ
  Outputs
    P: SpectralSequencePage
  Description
    Text
      Returns the k-th page of the spectral sequence.
    Example
      S = QQ[a..d];
      C = koszulComplex vars S
      K = filteredComplex C
    Text
      Let $E$ be the spectral sequence determined by $K$.
    Example
      E = spectralSequence K;
    Text
      We now compute some pages.
    Example
      E_0
      E_1
      E_infinity
      E^0
      E^1
      E^infinity
///

doc ///
  Key
    edgeComplex
   (edgeComplex, SpectralSequence)
   "Edge homomorphisms"
  Headline
    the edge homomorphisms
  Usage
      C = edgeComplex E
  Inputs
    E: SpectralSequence
  Outputs
    C: Complex
  Description
    Text
      Suppose that $E$ is a spectral sequence with the properties that:

      @OL{
	  "$E^2_{p,q} = 0$ for all $p < l$ and all $q \\in \\ZZ$;",
	  "$E^2_{p,q} = 0 $ for all $q < m$ and all $p \\in \\ZZ$;",
	  "$E$ converges to the graded module $\\{H_n\\}$ for $n \\in \\ZZ$."
	  }@

      Then $E$ determines a $5$-term exact sequence
      $$H_{l+m+2} \rightarrow E^2_{l+2,m} \rightarrow E^2_{l,m+1} \rightarrow H_{l+m+1} \rightarrow E^2_{l+1,m} \rightarrow 0$$
      which we refer to as the {\it edge complex}.

      Note that the above properties are satisfied if $E$ is the spectral sequence determined by a bounded filtration of a bounded chain complex.

      The following is an easy example, of a spectral sequence which arises from a nested chain of simplicial complexes, which illustrates this concept.
    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D;
      F1D = simplicialComplex {a*c, d};
      F0D = simplicialComplex {a,d};
      K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
      C = K_infinity;
      prune HH C
    Text
      The second page of the corresponding spectral sequences take the form:
    Example
      E = spectralSequence(K);
      e = prune E;
      E^2
      e^2
    Text
      The acyclic edge complex for this example has the form
      $H_1(C) \rightarrow E^2_{2,-1} \rightarrow E^2_{0,0} \rightarrow H_0(C)  \rightarrow E^2_{1, -1} \rightarrow 0$
      and is given by
    Example
      edgeComplex E
      prune edgeComplex E
    Text
      To see that it is acyclic we can compute
    Example
      prune HH edgeComplex E
  Caveat
    The method currently does not support pruned spectral sequences.
///

doc ///
  Key
    (minimalPresentation, SpectralSequence)
    (prune, SpectralSequence)
  Headline
    compute the minimal presentation of a spectral sequence
  Usage
    E = minimalPresentation e
  Inputs
    e:SpectralSequence
  Outputs
    E:SpectralSequence
  Description
    Text
      Returns the minimal presentation of a spectral sequence.

      If we fail to prune a spectral sequence then the out-put can be highly unintelligible.

      As a specific example consider the filtered complex $K$ below,
      obtained by multiplying the minimal free resolution of the rational
      quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      Compare some pages of the non-pruned version of the spectral sequence with that of the pruned version.
    Example
      E = spectralSequence K;
      E' = prune E;
      E^3
      E'^3
  SeeAlso
    (minimalPresentation, SpectralSequencePage)
    (prune, SpectralSequencePage)
    minimalPresentation
    prune
    pruningMaps
///

doc ///
  Key
    (complex, SpectralSequence)
  Headline
    the underlying chain complex of a Spectral Sequence
  Usage
    K = complex E
  Inputs
    E:SpectralSequence
  Outputs
    K:Complex
  Description
    Text
      Returns the underlying chain complex of a spectral sequence.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A
      K = filteredComplex C;
      E = spectralSequence K
      complex E
///

--------------------------------------------
-- Page and SpectralSequencePage
--------------------------------------------

doc ///
  Key
    Page
  Headline
    the type of all pages
  Description
    Text
      A page is a collection of modules which are indexed by lists of integers.
      This is a parent class for the type @TO"SpectralSequencePage"@.
      The infinity page of a spectral sequence is an example of a page which is not a spectral sequence page.
    Tree
      :Methods involving pages
        --> "new Page"
	> "page"
	> "netPage"
	> "support(Page)"
    Text
      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S,{1,3,4});
      K = filteredComplex(J,C,2);
    Text
      The infinity page of the resulting spectral sequence is computed below.
    Example
      E = prune spectralSequence K;
      E^infinity
  SeeAlso
    SpectralSequencePage
    (symbol ^, SpectralSequence, InfiniteNumber)
    (symbol _, SpectralSequence, InfiniteNumber)
    PageMap
///

doc ///
  Key
    page
   (page, List, List, Page)
   [page, Prune]
  Description
    Text
      Adds keys to a page.
  SeeAlso
    Page
///

doc ///
  Key
    spots
   (spots, Page)
  Headline
    which spots does the given page has a module.
  Usage
    s = spots P
  Inputs
    P:Page
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given page has a module.
///

doc ///
  Key
   (support, Page)
  Headline
    which non-zero modules appear in the given page.
  Usage
    l = support P
  Inputs
    P:Page
  Outputs
    l:List
  Description
    Text
      Returns a list of all the non-zero modules appearing in the given page has a module.
///

doc ///
  Key
    SpectralSequencePage
  Headline
    the type of all spectral sequence pages
  Description
    Text
      A (homological, or lower index) spectral sequence page consists of:

      @OL{
	  "A fixed integer $r \\geq 0$, the page number;",
	  "A sequence of modules $\\{E^r_{p,q}\\}$ for $p,q \\in \\ZZ$;",
	  "A collection of homomorphisms $\\{d^r_{p,q}: E^r_{p,q} \\rightarrow E^r_{p-r,q+r-1}\\}$ for $p,q \\in \\ZZ, r \\geq 0$ such that $d^r_{p,q} d^r_{p+r,q-r+1} = 0$;",
	  "A collection of isomorphisms $E^{r+1}_{p,q}  \\rightarrow \\ker d^r_{p,q} / \\operatorname{image} d^r_{p+r,q-r+1}$."
	  }@

      Alternatively a (cohomological, or upper index) spectral sequence page consists of:

      @OL{
	  "A fixed integer $r \\geq 0$, the page number;",
	  "A sequence of modules $\\{E_r^{p,q}\\}$ for $p,q \\in \\ZZ$;",
	  "A collection of homomorphisms $\\{d_r^{p,q}: E_r^{p,q} \\rightarrow E_r^{p+r,q-r+1}\\}$ for $p,q \\in \\ZZ, r \\geq 0$ such that $d_r^{p,q} d_r^{p-r,q+r-1} = 0$;",
	  "A collection of isomorphisms $E_{r+1}^{p,q} \\rightarrow \\ker d_r^{p,q} / \\operatorname{image} d_r^{p-r,q+r-1}$."
	  }@

      The type {\tt SpectralSequencePage} is a data type for working with spectral sequence pages.
    Tree
      :Methods involving spectral sequence pages
        > "spectralSequencePage"
	> "spectralSequencePage(FilteredComplex,ZZ)"
	> "minimalPresentation(SpectralSequencePage)"
	> "pruningMaps(SpectralSequencePage)"
	> "basis(List,SpectralSequencePage)"
	> "hilbertPolynomial(Page)"
	> "SpectralSequencePage ^ List"
  Caveat
      The isomorphisms above are not explicitly part of the data type,
      but they can be obtained by using the command @TO"homologyIsomorphism"@.
  SeeAlso
    SpectralSequence
    SpectralSequencePageMap
///

doc ///
  Key
    (spectralSequencePage, FilteredComplex, ZZ)
  Headline
    construct a spectral sequence page from a filtered complex
  Usage
    E = spectralSequencePage(K,r)
  Inputs
    K:FilteredComplex
    r:ZZ
  Outputs
    E:SpectralSequencePage
  Description
    Text
      Returns the rth page of the spectral sequence determined by K.

      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      Let $E$ be the spectral sequence determined by $K$.
    Example
      E = spectralSequence K;
    Text
      We now compute some pages.
    Example
      E^0
      E^2
      E^infinity
///

doc ///
  Key
    spectralSequencePage
   [spectralSequencePage, Prune]
  Headline
    construct a spectral sequence page from a filtered complex
  Usage
    E = spectralSequencePage(K,r)
  Inputs
    K:FilteredComplex
    r:ZZ
  Outputs
    E:SpectralSequencePage
  Description
    Text
      This is the primitive spectral sequence page constructor.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S,{1,3,4});
      K = filteredComplex(J,C,2);
      E = spectralSequence K
    Text
      To view pages or maps we proceed as follows.
    Example
      E^2
      support E^2 .dd
      E^2 .dd _{0,1}
      E^infinity
  SeeAlso
    spectralSequence
    (spectralSequence, FilteredComplex)
    SpectralSequencePageMap
///

doc ///
  Key
    (symbol ^, SpectralSequencePage, List)
    (symbol _, SpectralSequencePage, List)
  Headline
    retrieve the module in the {i,j} position on the page
  Usage
    M = P^{i,j}
    M = P_{i,j}
  Inputs
    P:SpectralSequencePage
    "{i,j}": -- a pair of integers
  Outputs
    M:Module
  Description
    Text
      The notation @TT "P^{i,j}"@ returns the module in the cohomological \{i,j\} position in the spectral sequence page.
    Example
      A = QQ[x,y]
      C = koszulComplex vars A;
      K = filteredComplex C;
      E = spectralSequence K
      E_0
      E_0 ^{-1,0}
    Text
      The notation @TT "P_{i,j}"@ returns the module in the homological \{i,j\} \ position in the spectral sequence page.
    Example
      E^0
      E_0 _{1,0}
    Text
      The relationship $E^{-i,-j} = E_{i,j}$ holds.
    Example
      E_0 ^{-1,0} === E_0 _{1,0}
///

doc ///
  Key
    pruningMaps
   (pruningMaps, SpectralSequencePage)
  Headline
    compute the pruning maps on a spectral sequence page
  Usage
    d = pruningMaps E
  Inputs
    E:SpectralSequencePage
  Outputs
    d:PageMap
  Description
    Text
      Returns the pruning maps which are cached in the process of pruning the spectral sequence page.
    Example
      S = QQ[a..d];
      C = koszulComplex vars S
      K = filteredComplex C
      E = prune spectralSequence K
      pruningMaps E^2
  SeeAlso
    (prune, SpectralSequence)
    SpectralSequencePage
    PageMap
///

doc ///
  Key
    (basis, List, SpectralSequencePage)
    (basis, ZZ, SpectralSequencePage)
  Headline
    generators of a particular degree
  Usage
    B = basis(L, E)
  Inputs
    L:List
    E:SpectralSequencePage
  Outputs
    B:Page
  Description
    Text
      Returns generators for the requested (multi)degree of the spectral sequence page.
      It is designed to extend the function @TO"basis"@ which can be applied to modules, for instance.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      We compute the degree $0$ piece of the $E^3$ page below.
    Example
      E = prune spectralSequence K;
      E^3
      basis(0, E^3)
  SeeAlso
      basis
///

doc ///
  Key
    (hilbertPolynomial, Page)
  Headline
    the Hilbert polynomial of a page
  Usage
    H = hilbertPolynomial(E)
  Inputs
    E:Page
  Outputs
      H:Page
  Description
    Text
      Returns the Hilbert polynomials of all modules in a page.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      We compute the degree $0$ piece of the $E^3$ page below.
    Example
      E = prune spectralSequence K;
      hilbertPolynomial(E^0)
      hilbertPolynomial(E^1)
      hilbertPolynomial(E^infinity)
///

doc ///
  Key
    (minimalPresentation, SpectralSequencePage)
    (prune, SpectralSequencePage)
  Headline
    a minimal presentation of a spectral sequence page
  Usage
    E = minimalPresentation e
  Inputs
    e:SpectralSequencePage
  Outputs
    E:SpectralSequencePage
  Description
    Text
      Returns a minimal presentation of the spectral sequence page.

      If we fail to prune a spectral sequence then the out-put can be highly
      unintelligible.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      Compare some pruned and non-prunded pages the spectral sequence $E$ below.
    Example
      E = spectralSequence K;
      E^3
      prune E^3
  SeeAlso
    (minimalPresentation, SpectralSequence)
    (prune, SpectralSequence)
    minimalPresentation
    prune
///

doc ///
  Key
    netPage
   (netPage,Page, List, List)
  Headline
    display a small portion of a given Spectral Sequence page
  Usage
    E' = netPage(E,L1,L2)
  Inputs
    E: Page
    L1: List
    -- A list {minP,minQ}, the bottom left corner coordinates to display
    L2: List
     -- A list {maxP,maxQ}, the top right corner coordinates to display
  Outputs
    E': Net
  Description
    Text
      Produces the portion of a given spectral sequence page that lies in the square
      with given bottom right and top left coordinates.
    Example
      R = QQ[x];
      S = R/ideal"x2";
      N = S^1/ideal"x";
      M = R^1/R_0;
      C = res M;
      C' = C ** S;
      D = res(N,LengthLimit => 10);
      E0 = C' ** (filteredComplex D);
      E = prune spectralSequence E0;
    Text
      The E_2 page has nonzero E_2^{p,q} when 0 <= p <= 10 and 0 <= q <= 1,
      so we may ask to restrict the display to 2 <= p <= 6 and 0 <= q <= 1.
    Example
      netPage(E_2,{2,0},{6,1})
    Text
      If we ask for a square that is too large, only the relevant portion of the page will be displayed.
    Example
      R = QQ[x];
      S = R/ideal"x2";
      N = S^1/ideal"x";
      M = R^1/R_0;
      C = res M;
      C' = C ** S;
      D = res(N,LengthLimit => 10);
      E0 = C' ** (filteredComplex D);
      E = prune spectralSequence E0;
      netPage(E_2,{-5,0},{7,1})
///

--------------------------------------------
-- PageMap and SpectralSequencePageMap
--------------------------------------------

doc ///
  Key
    PageMap
  Headline
    the type of all page maps
  Description
    Text
      A page map is a collection of homomorphisms which are indexed by lists of integers.
      This is a parent class for the type @TO"SpectralSequencePageMap"@.
      The output of the method {\tt pruningMaps(SpectralSequencePage)} is an example of a {\tt Page} which is not a {\tt SpectralSequencePage}.
    Text
      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,3,4});
      K = filteredComplex(J,C,2);
    Text
      We compute an example of a pruning map below.
    Example
      E = prune spectralSequence K;
      D = E^2 .dd
      support D
      D_{0,1}
  SeeAlso
    Page
    SpectralSequencePageMap
    (pruningMaps, SpectralSequencePage)
///

doc ///
  Key
    SpectralSequencePageMap
  Headline
    the type of all spectral sequence page maps
  Description
    Text
      A (homological, or lower index) spectral sequence page map consists of:

      @OL{
	  "A fixed integer $r \\geq 0 $, the page number;",
	  "A collection of homomorphisms $\\{d^r_{p,q}: E^r_{p,q} \\rightarrow E^r_{p-r,q+r-1}\\}$ for $p,q \\in \\ZZ, r \\geq 0$ such that $d^r_{p,q} d^r_{p+r,q-r+1} = 0$."
	  }@

      Alternatively a (cohomological, or upper index) spectral sequence page consists of:

      @OL{
	  "A fixed integer $r \\geq 0$, the page number;",
	  "A collection of homomorphisms $\\{d_r^{p,q}: E_r^{p,q} \\rightarrow E_r^{p+r,q-r+1}\\}$ for $p,q \\in \\ZZ, r \\geq 0$ such that $d_r^{p,q} d_r^{p-r,q+r-1} = 0$."
	  }@

      The type {\tt SpectralSequencePageMap} is a data type for working with the differentials on the pages of a spectral sequence.
    Tree
      :Methods involving spectral sequence page maps
        > "spectralSequencePageMap(FilteredComplex,ZZ)"
	> "SpectralSequencePageMap ^ List"
  SeeAlso
    SpectralSequence
    SpectralSequencePage
///

doc ///
  Key
    spectralSequencePageMap
   (spectralSequencePageMap, FilteredComplex, ZZ)
   [spectralSequencePageMap, Prune]
  Headline
    compute the maps on a spectral sequence page
  Usage
      d = spectralSequencePageMap(FilteredComplex, ZZ)
  Inputs
      K:FilteredComplex
      n:ZZ
  Outputs
      D:SpectralSequencePageMap
  Description
    Text
      Returns the differentials of a spectral sequence page.
///

doc ///
  Key
    (symbol ^, SpectralSequencePageMap, List)
    (symbol _, SpectralSequencePageMap, List)
  Headline
    retrieve the map from the {p,q} term of a spectral sequence page
  Usage
    d = D ^ {p,q}
    d = D _ {p,q}
  Inputs
    D:SpectralSequencePageMap
    "{p,q}": -- a pair of integers
  Outputs
    d: Matrix
  Description
    Text
      The notation @TT "P.dd_{p,q}"@ returns the map with source the homological {p,q} term on a spectral sequence page.
    Example
      S = QQ[a..d];
      J = ideal vars S;
      C = res monomialCurveIdeal(S, {1,2,3});
      K = filteredComplex(J,C,2);
    Text
      We compute a map on the third page of the spectral sequence associated to $K$.
    Example
      E = prune spectralSequence K
      P = E^2
      P.dd_{0,1}
    Text
      The notation @TT "P.dd^{p,q}"@ returns the map with source the cohomological {p,q} term on a spectral sequence page.
    Example
      P.dd^{0,-1}
    Text
      The relationship $P.dd_{p,q} = P.dd^{-p,-q}$ holds.
    Example
      P.dd_{0,1} === P.dd^{0,-1}
///

--------------------------------------------
-- Other methods
--------------------------------------------

doc ///
  Key
    connectingMorphism
   (connectingMorphism, ComplexMap, ZZ)
  Headline
    use spectral sequences to compute connecting morphisms
  Usage
    g = connectingMorphism(f, n)
  Inputs
    f:ComplexMap
    n:ZZ
  Outputs
    g:Matrix
  Description
    Text
      Given a morphism $f: A \rightarrow B$ of chain complexes
      returns the connecting map $H_{n+1}(\coker f) \rightarrow H_n (\im f)$.
///

doc ///
  Key
    homologyIsomorphism
   (homologyIsomorphism, SpectralSequence, ZZ, ZZ, ZZ)
  Headline
    compute the homology isomorphism
  Usage
    g = homologyIsomorphism(SpectralSequence, ZZ, ZZ, ZZ)
  Inputs
    E:SpectralSequence
    p:ZZ
    q:ZZ
    r:ZZ
  Outputs
    g:Matrix
  Description
    Text
      Computes the isomorphism \$ker d^r_{p,q} / \im d^r_{p + r, q - r + 1} \rightarrow E^{r+1}_{p,q}$
    Example
      A = ZZ [s,t,u,v,w] ;
      K = filteredComplex(reverse {
	      simplicialComplex {s},
	      simplicialComplex {s,t},
	      simplicialComplex {s,t,u},
	      simplicialComplex {s*t, u},
	      simplicialComplex {s*t, u, v},
	      simplicialComplex {s*t, u, v, w},
	      simplicialComplex {s*t, s*w ,u, v},
	      simplicialComplex {s*t, s*w ,t * w, u, v},
	      simplicialComplex {s*t, s*w ,t * w, u * v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w}}, ReducedHomology => false);
      E = prune spectralSequence K
      e = spectralSequence K
      apply(keys support E^11, i -> homologyIsomorphism(E, i#0, i#1, 11))
      apply(keys support e^11, i -> homologyIsomorphism(e, i#0, i#1, 11))
  SeeAlso
      homologyIsomorphism
///

--- TODO: Move to Complexes
doc ///
  Key
    (naiveTruncation, Complex, ZZ)
  Headline
    compute the hard truncation of a chain complex
  Usage
    naiveTruncation(C, n)
  Inputs
    C:Complex
    n:ZZ
  Outputs
    :Complex
  Description
    Text
      This method returns the naive truncation of $C$ by truncating
      the low homological degrees if $n$ is positive (i.e. from left)
      and high homological degrees if $n$ is negative (i.e. from right).
    Example
      B = QQ[a..d];
      C = (koszulComplex vars B)[2]
      naiveTruncation(C, 10)
      naiveTruncation(C, 2)
      naiveTruncation(C, 1)
      naiveTruncation(C, 0)
      naiveTruncation(C,-1)
      naiveTruncation(C,-2)
      naiveTruncation(C,-10)
  SeeAlso
    (naiveTruncation, Complex, ZZ, ZZ)
///

--- TODO: Move to Complexes?
doc ///
  Key
    (spots, Complex)
  Headline
    which spots does the given chain complex has a module.
  Usage
    s = spots L
  Inputs
    L:Complex
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given chain complex has a module.
  SeeAlso
    (concentration, Complex)
    (support, Complex)
///

--- TODO: Move to Complexes
doc ///
  Key
    (support, Complex)
  Headline
    nonzero parts of a chain complex
  Description
    Text
      Computes the homological degrees in which the chain complex admits a nonzero module
    Example
      A = QQ[x,y];
      C = koszulComplex vars A
      support C
      D = naiveTruncation(C, 1)
      spots D
      support D
  SeeAlso
    (concentration, Complex)
    (spots, Complex)
///
