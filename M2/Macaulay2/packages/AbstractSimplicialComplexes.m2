-- -*- coding: utf-8 -*-
---------------------------------------------------------------------------------
-- Copyright 2025  Nathan Grieve
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------
-- The code that follows is mostly formatted to max line length of 80 and in some
-- special cases (like method definition) to max line length of 110.
-- The code that follows is designed to run on Macaulay2, version 1.24.11.

newPackage(
    "AbstractSimplicialComplexes",
    Version => "1.1",
    Date => "12 May 2025",
    Headline => "Abstract Simplicial Complexes",
    Authors => {{ Name => "Nathan Grieve", Email => "nathan.m.grieve@gmail.com",
	    HomePage => "https://sites.google.com/view/nathan-grieve"}},
    AuxiliaryFiles => false,
    DebuggingMode => false,
    PackageImports => {"Complexes"},
    Keywords => {"Combinatorial Commutative Algebra"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Abstract simplicial complexes in Macaulay2",
	"acceptance date" => "2025-04-14",
	"published article URI" => "https://msp.org/jsag/2025/15-1/p03.xhtml",
	"published article DOI" => "10.2140/jsag.2025.15.29",
	"published code URI" => "https://msp.org/jsag/2025/15-1/jsag-v15-n1-x03-AbstractSimplicialComplexes.m2",
	"release at publication" => "5b79022404cef1d070584115c819d4436be7bcee",
	"version at publication" => "1.0",
	"volume number" => "15",
	"volume URI" => "https://msp.org/jsag/2025/15-1/"
	}
    )

export {"AbstractSimplicialComplex", "abstractSimplicialComplex",
    "simplicialChainComplex", "reducedSimplicialChainComplex",
    "ambientAbstractSimplicialComplexSize",
    "ambientAbstractSimplicialComplex", "abstractSimplicialComplexFacets",
    "randomAbstractSimplicialComplex", "randomSubSimplicialComplex",
    "inducedSimplicialChainComplexMap","inducedReducedSimplicialChainComplexMap", 
    }

-* Code section *-

-----------
-- spots --
-----------
-- The spots method is extremely useful
-- but we don't export it. 
-----------------------------------------

spots = method()

spots Complex := List => (
  C -> (c := concentration C; toList(c_0 .. c_1)))

max Complex := K -> max spots K
min Complex := K -> min spots K

---------------------------------------
---------------------------------------

---------------------------------
-- Abstract Simplicial Complex -- 
---------------------------------

-- The idea is to make an Abstract Simplicial Complex as a Type of HashTable
-- as a means for working with Abstract Simplicial Complexes.
-- The integer keys will output the list of i-simplicies.

AbstractSimplicialComplex = new Type of HashTable
AbstractSimplicialComplex.synonym = "abstract simplicial complex"

AbstractSimplicialComplex.GlobalAssignHook = globalAssignFunction
AbstractSimplicialComplex.GlobalReleaseHook = globalReleaseFunction
describe AbstractSimplicialComplex := K -> net expression K

new AbstractSimplicialComplex := AbstractSimplicialComplex =>(cl) -> (
     newClass(AbstractSimplicialComplex, new HashTable))

spots AbstractSimplicialComplex := List => (
     K -> sort select(keys K, ZZ))

-- Return the p-faces of a simplicial complex.

AbstractSimplicialComplex _ ZZ := AbstractSimplicialComplex => (K,p) -> (
     K#p ?? {})

-- Given a list of subsets L and A \in L decide if A is maximal.

isMaximal := (x,L) -> (
     myList := select(L,i -> isSubset(x,i));
     #myList == 1)

-- Select the maximal subsets (i.e., facets) of a list of subsets.

listFacets := (L) -> (
     select(L,i-> isMaximal(i,L)))

--- Return the facets of a simplicial complex.

abstractSimplicialComplexFacets = method()

abstractSimplicialComplexFacets(AbstractSimplicialComplex) := List => K ->(
     L := flatten(apply(spots K, i-> K_i));
     listFacets(L))

 facets(AbstractSimplicialComplex):=List => K ->(
     L := flatten(apply(spots K, i-> K_i));
     listFacets(L))
 
 --- Decide if two simplicial complexes are equal.

AbstractSimplicialComplex == AbstractSimplicialComplex := Boolean => (K,L) ->(
     (abstractSimplicialComplexFacets K) == (abstractSimplicialComplexFacets L))

--- Returns the dimension of a simplicial complex.

dim AbstractSimplicialComplex := ZZ => (K) -> (
     max apply(abstractSimplicialComplexFacets(K), i -> #i) - 1)

--- Constructors for AbstractSimplicialComplexs ---

abstractSimplicialComplex = method()

--  What follows is the most basic constructor of AbstractSimplicialComplex.
--  The idea is to make a simplical complex starting from a list of faces.
--  The list of faces need not be facets.
--  The constructor returns the simplicial complex (with all of its faces)
--  that is generated by this list of faces. 
--  By default, it is assumed that the kfaces are all lex ordered
--  positive integers.

makeKFaces := (L,k) -> (
     toList(set(flatten(apply(#L, i -> subsets(sort L_i,k))))))

makeAllFaces := (L) -> (
     numberOfFaces := #L;
     n := max(apply(numberOfFaces, i-> # (L_i))); --  find the highest face dimension    
     flatten(for k from 0 to n list {k-1 => sort makeKFaces(L,k)}))

abstractSimplicialComplex(List) := AbstractSimplicialComplex => L -> (
     new AbstractSimplicialComplex from makeAllFaces(L))

--- The following method will make the (n-1)-dimensional
--  n-simplex on [n] = {1,...,n}.

abstractSimplicialComplex(ZZ) := AbstractSimplicialComplex => (n) -> (
     L := for i from 1 to n list i;
     abstractSimplicialComplex({L}))

--- Make the "r-skeleton" on [n] = {1,...n}.

abstractSimplicialComplex(ZZ,ZZ) := AbstractSimplicialComplex => (n,r) -> (
     abstractSimplicialComplex subsets(for i from 1 to n list i,r))

----------------------------------------
-- Making random simplicial complexes --
----------------------------------------

-- What follows are simple minded (yet still seemingly practical)
-- methods for producing random simplicial complexes.
-- In either case they are fairly efficient.
-- In either case, what follows suffices for our purposes at present.

--  A variant of the above method would yield a random k element subset of a given set.

--  Make a "random" simplicial complex on {1,...,n}.

randomAbstractSimplicialComplex = method()

randomAbstractSimplicialComplex(ZZ) := AbstractSimplicialComplex => (n) -> (
     listLength := 1 + random(2^n);
     x := toList(1..n);
     randomFaces := unique(for i from 1 to listLength list randomSubset x);
     abstractSimplicialComplex randomFaces)

--  Make a random simplicial complex on [n] with r-skeleton.

randomAbstractSimplicialComplex(ZZ,ZZ) := AbstractSimplicialComplex => (n,r) -> (
     listLength := 1 + random(binomial(n,r));
     x := toList(1..n);
     randomFaces := unique(for i from 1 to listLength list randomSubset(x,r));
     abstractSimplicialComplex randomFaces)

--  Make the random complex Y_d(n,m) which has vertex set
--  [n] and complete (d − 1)-skeleton, and has m d-dimensional faces,
--  chosen at random from all binomial(binomial(n,d+1),m) possibilities.
--  Such random complexes appear in lots of different contexts including in the article
--  Cohen-Lenstra heuristics for torsion in homology of random complexes
--  (Kahle, M. and Lutz, F. H. and Newman, A. and Parsons, K.).

randomAbstractSimplicialComplex(ZZ,ZZ,ZZ) := AbstractSimplicialComplex => (n,m,d) -> (
     L := for i from 1 to n list i;
     dDimlSubsets := subsets(L,d+1);
     rdmFaces := for i from 1 to m list (dDimlSubsets#(random(binomial(n,d+1))));
     append(append(rdmFaces,{L}),subsets(L,d));
     abstractSimplicialComplex(rdmFaces))
 
-- Make a random simplicial subcomplex.

randomSubSimplicialComplex = method()

randomSubSimplicialComplex(AbstractSimplicialComplex) := AbstractSimplicialComplex => (K) -> (
     L := abstractSimplicialComplexFacets K;
     abstractSimplicialComplex unique apply(L, i-> randomSubset(i)))

--------------------------------
-- Ambient simplicial complex --
--------------------------------

-- Return the size of the underlying ambient simplex.

ambientAbstractSimplicialComplexSize = method() 

ambientAbstractSimplicialComplexSize(AbstractSimplicialComplex) := (K) -> (
    L := flatten(K_0);
    if L != { } then max L else 0 -- by default {{}} has size 0
    )

-- Return the underlying ambient simplex.

ambientAbstractSimplicialComplex = method() 

ambientAbstractSimplicialComplex(AbstractSimplicialComplex) := AbstractSimplicialComplex => (K) -> (
    abstractSimplicialComplex(ambientAbstractSimplicialComplexSize(K)))

---------------------
--- Boundary maps ---
---------------------
  
--  There are many ways to approach the simplical boundary map
--  for a given simplicial complex. Perhaps the most straight forward way
--  is via \partial_i : C_i --> C_{i-1}.  Here C_i is the free 
--  \ZZ-module (or \kk-vector space) on the set of i+1 simplicies 
--  (i.e., the set of i+1 lexiographically ordered combinations of {1,...,n}).

-- Given input a i+1 lex segment l = [l_0,...,l_i] ---- i.e., an i-face
-- in computing its image under the boundary map it seems most straight
--  forward to think of the output as a i+1 tuple with entry i having the
-- form [(-1)^i,d_i(l)] here d_i(l) is l with the i-th lex entry removed.

-- The following function is useful to construct the simplicial chain complex differential
-- given a k-face y and a k+1 - face x decide if it equals \partial(x,i) for some i.

-- Note that here we are making the chain complex maps directly
-- and are not relying on any special access to the M2 core
-- this makes the construction perhaps a bit less efficient
-- but in the end doesn't affect other calculations (e.g.,
-- calculating the homology of such complexes).

isDLexSeqI := (y,x) -> (
    k := #y;
    sign := 0;
    for i from 0 to # x do (
        z := drop(x,{i,i});
        if y == z then (sign = (-1)^i;
            break);
	);
        sign)

-- Make a constructor for making matrices that represented the simplicial boundary
-- maps of a given simplical complex.

simplicialMakeMatrix = method()

simplicialMakeMatrix(List,List) := (kPlusOneFaces,kFaces) -> (
    n := # kPlusOneFaces;
    m := # kFaces;
    matrixList := 
    	for i from 0 to m-1 list (
    for j from 0 to n-1 list (
	   isDLexSeqI((kFaces)#i,(kPlusOneFaces)#j))
	);
    matrix(matrixList))

--  We can finally make the entire reduced homology chain complex in the following way
--  Given as input the simplcial complex represented as a simplicial set --
--  This will produce the reduced chain complex (so the empty set will
--  appear in the chain complex).

--  Return the reduced chain complex
--  (with contribution from the empty face) that is associated to an
--  an abstract simplicial complex.

reducedSimplicialChainComplex = method() 

reducedSimplicialChainComplex(AbstractSimplicialComplex) := Complex => (L) -> (
    n := max spots L;
    if n == -1 then (return complex hashTable {-1 => map(ZZ^0,ZZ^1,zero)})
    else(
    mapsList := for i from 0 to n list (i => simplicialMakeMatrix(L#i,L#(i-1)));
    append(mapsList,-1 => map(ZZ^0,target(mapsList#0)#1,zero)););
    complex hashTable mapsList)

--  Return the non-reduced simplicial chain complex
--  (i.e., the chain complex with no contribution from the empty face).

simplicialChainComplex = method() 

simplicialChainComplex(AbstractSimplicialComplex) := Complex => (L) -> (
   naiveTruncation(reducedSimplicialChainComplex L, 0, infinity))
        
--  Another method that is of interest,
--  is to give an inclusion (or more general a morphism)
--  of simplicial complexes and then compute
--  the induced chain complex morphism of SimplicialChainComplexes.
--  An important special case is to view a
--  sub simplicial complex of the full simplicial complex (simplex)
--  and then to compute the corresponding induced inclusion morphism.

--  A first step is to make an k-face inclusion map given an inclusion of
--  abstract simplicial complexes.
--  Assume that L <= H.
--  If L_k has no faces then the method returns an error message
--  Otherwise the method produces the appropriate matrix
--  that induces the corresponding inclusion map.

inducedKFaceSimplicialChainComplexMap = method()

inducedKFaceSimplicialChainComplexMap(ZZ,AbstractSimplicialComplex,AbstractSimplicialComplex) := (k,H,L) -> (
     M := L_k;
     N := H_k;
     n := # M;
     m := # N;
     myMatrixList := for i from 0 to m-1 list (
	 for j from 0 to n-1 list (
	     if N#i == M#j then 1 else 0));
     matrix myMatrixList)

--  If H <= L then give the induced chain complex map
--  for (non-reduced) simplicalChainComplexes.

inducedSimplicialChainComplexMap = method()

inducedSimplicialChainComplexMap(AbstractSimplicialComplex,AbstractSimplicialComplex) := (L,H) -> (
     h := simplicialChainComplex H;
     l := simplicialChainComplex L;
     if ((abstractSimplicialComplex {{}}) == H) then return map(l,h,zero)
     else(myKeys := apply(spots h, i ->
	    if i == -1 then i => map(l_(-1),h_(-1),zero)
	    else i => inducedKFaceSimplicialChainComplexMap(i,L,H));
            f := hashTable myKeys;
            return map(l,h,f);))

--  If H <= L then give the induced chain complex map
--  for reduced simplicalChainComplexes.

inducedReducedSimplicialChainComplexMap = method()

inducedReducedSimplicialChainComplexMap(AbstractSimplicialComplex,AbstractSimplicialComplex) := (L,H) -> (
    h := reducedSimplicialChainComplex H;
    l := reducedSimplicialChainComplex L;
    if ((abstractSimplicialComplex {{}}) == H)
    then return map(l,h, hashTable {-2 => map(l_(-2),h_(-2),zero), -1 => map(l_(-1),h_(-1),id_(h_(-1)))})
    else(myKeys := apply(spots h, i ->
	    if i == -1 then i => map(l_(-1),h_(-1),id_(h_(-1)))
	    else i => inducedKFaceSimplicialChainComplexMap(i,L,H));
            f := hashTable myKeys;
            return map(l,h,f);))
--------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------- 
-* Documentation section *-
beginDocumentation()

------------------------------
-- Documentation Index page --
------------------------------

doc ///  
    Key
        AbstractSimplicialComplexes
    Headline 
        a package for working with abstract simplicial complexes
    Description
        Text
	    The package AbstractSimplicialComplexes provides a methodology for 
            working with abstract simplicial complexes.  The starting point is to represent each
	    abstract simplicial complex as a graded list.  In this regard our approach differs
	    fundamentally from that of the package
	    @TO "SimplicialComplexes::SimplicialComplexes"@.
        Text
	    In this package our conventions are that abstract simplicial complexes 
            have vertices supported on the set $[n] := \{1,\dots,n\}$.  Users who wish to use 
            the package to study simplicial complexes on vertex sets different from $[n]$
            should first fix a suitable order preserving bijection which is compatible
            with the standard lexicographic ordering.
	Text
	   Here, we are especially interested in homological aspects of abstract simplicial complexes
           and our approach is to implement such simplicial complexes as certain graded lists. 
           In particular, we provide methods for working with the chain complexes that are 
           associated to each abstract simplicial complex.  
           We also give some functionality for producing random simplicial complexes.
        Text
           @SUBSECTION "An overview of this package"@
	Text   
           For an overview of how to use this package, see
	Text
	   @UL {TO2("How to make abstract simplicial complexes","How to make abstract simplicial complexes"),
	       TO2("How to make reduced and non-reduced simplicial chain complexes",
		   "How to make reduced and non-reduced simplicial chain complexes"),
	       TO2("Calculations with random simplicial complexes",
		   "Calculations with random simplicial complexes") }@
        Text
           @SUBSECTION "Mathematical background, motivation, conventions and notations"@
	Text
	   Our conventions for abstract simplicial complexes, their associated boundary maps,
	   reduced and non-reduced chain complexes are similar to those of the texts
	Text
	   @UL {{HREF("https://www.springer.com/gp/book/9780387223568","Combinatorial commutative algebra"),
		   " by E. Miller and B. Sturmfels [Springer-Verlag, (2005)]"},
		 {HREF("https://link.springer.com/book/10.1007/978-1-4757-1793-8","Basic Topology"),
		     " by M. A. Armstrong [Springer-Verlag (1983)]"},
		 {HREF("https://www.cambridge.org/core/books/cohenmacaulay-rings/938BC2204D8A7C99E2CEBA1695A692A4",
			 "Cohen-Macaulay rings"),
		     " by W. Bruns and J. Herzog [Cambridge University Press (1993)]"}}@
	Text
	   and others.
	Text  
	   This package also has strong motivation from and interplay with Topological Data Analysis.
	   We refer to the works
	Text
           @UL {{HREF("https://www.ams.org/journals/bull/2009-46-02/S0273-0979-09-01249-X/","Topology and data"),
		   " by G. Carlsson [Bull. Amer. Math. Soc. vol. 46, no. 2 (2009)]"},
	       {HREF("https://www.cambridge.org/core/books/topological-data-analysis-with-applications/00B93B496EBB97FB6E7A9CA0176F0E12",
		       "Topological data analysis with applications"),
		   " by G. Carlsson and M. Vejdemo-Johansson [Cambridge University Press (2022)]" }}@	
        Text
           and the references therein for further details about abstract simplicial complexes within the
	   context of Topological Data Analysis.
	Text
	   This package also provides constructors for producing certain classes of random simplicial complexes.
	   We refer to the work
	Text
	   @UL { {HREF("https://www.tandfonline.com/doi/abs/10.1080/10586458.2018.1473821",
		       "Cohen-Lenstra heuristics for torsion in homology of random complexes"),
		   " by  M. Kahle, F. H. Lutz, A. Newman, and K. Parsons [Exp. Math. vol. 29, no. 3 (2020)]"}}@
	Text
	   and the references therein for further details about constructing random simplicial complexes.
///	    


--------------------------------------------
-- Package overview examples ---------------
--------------------------------------------

doc ///
     Key
     	  "How to make abstract simplicial complexes"
     Headline
     	  using the type AbstractSimplicialComplexes to represent abstract simplicial complexes
     Description
     	  Text	  
	     The type AbstractSimplicialComplex is a data type for working with
	     abstract simplicial complexes with vertices supported on $[n] = \{1,\dots ,n\}$.
	     Here we illustrate some of the most basic ways to interact with this data type.
          Text
	     The simplicial complex that is generated by $\{1,2,3,4\}$, $\{2,3,5\}$ and $\{1,5\}$ can be
	     constructed in the following way.	    
	  Example
               K = abstractSimplicialComplex({{1,2,3,4}, {2,3,5},{1,5}})
          Text
	       The simplex on the vertex set $[4]$ can be constructed as     
          Example
	      L = abstractSimplicialComplex(4)
	  Text
	     The faces and facets of such simplicial complexes can be accessed as
	  Example
	      K_(-1)
	      K_0
	      K_1
	      K_2
	      abstractSimplicialComplexFacets K
	      L_(-1)
	      L_0
	      L_1
	      L_2
	      L_3
	      abstractSimplicialComplexFacets L
///



doc ///
     Key
     	  "How to make reduced and non-reduced simplicial chain complexes"
     Headline
     	  simplicial homological constructors 
     Description
          Text
	     Let $K$ be an abstract simplicial complex on the vertex set $[n] = \{1\dots,n\}$.
	     We represent dimension $i$ faces of $K$ as lexicographically ordered
	     cardinality $i+1$ subsets $\{l_0 < \dots < l_i\}$ of $[n]$.
	     We further let $C_i$ be the free abelian group on the set of $i$-dimensional faces of $K$.
	  Text
	     By these conventions the differential from the free abelian group of $K$'s dimension $i$
	     faces to the free abelian group of $K$'s dimension $i-1$ faces sends each $\{l_0 < \dots < l_i\}$
	     to the signed sum of all lexicographically ordered sets $\{l_0 < \dots < l_i\} \backslash \{l_j\}$ as
	     $j$ ranges from $0$ to $i$. 
	  Text  
	     Here, the sign in front of  $\{l_0 < \dots < l_i\} \backslash \{l_j\}$ is $(-1)^j$. 
	  Text	  
	     Using the constructors @TO"simplicialChainComplex"@ and @TO"reducedSimplicialChainComplex"@
	     respectively, non-reduced and reduced simplicial chain complexes can be
	     constructed in the following way.
	  Example
	     K = abstractSimplicialComplex({{1,2,3,4}, {2,3,5},{1,5}}) 
             k = simplicialChainComplex K
             k.dd
             kRed = reducedSimplicialChainComplex K
             kRed.dd
///

doc ///
     Key
     	  "How to make subsimplical complexes and induced simplicial chain complex maps"
     Headline
     	  induced simplicial chain complex maps via subsimplicial complexes 
     Description
     	  Text	  
	     Given a subsimplicial complex there are induced simplicial
	     chain complex maps. This is illustrated in the following way.
	  Example
	     K = abstractSimplicialComplex(4,3)
             L = abstractSimplicialComplex(4,2)
             f = inducedSimplicialChainComplexMap(K,L)
             isWellDefined f
             fRed = inducedReducedSimplicialChainComplexMap(K,L)
             isWellDefined fRed
///

doc ///
     Key
     	  "Calculations with random simplicial complexes"
     Headline
     	  homological calculations on random simplicial complexes
     Description
     	  Text	  
	     In what follows we illustrate a collection of homological calculations that
	     can be performed on random simplicial complexes.
          Text
	     Create a random abstract simplicial complex with vertices supported on a
	     subset of $[n] = \{1,...,n\}$.  
          Example
	     K = randomAbstractSimplicialComplex(4)
	     prune HH simplicialChainComplex K
	  Text
	     Create a random simplicial complex on $[n]$ with dimension at most equal to $r$.
          Example
	     L = randomAbstractSimplicialComplex(6,3)
	     prune HH simplicialChainComplex L
	  Text
	     Create the random simplicial complex $Y_d(n,m)$ which has vertex set
             $[n]$ and complete $(d − 1)$-skeleton, and has exactly m dimension d faces,
             chosen at random from all $\binom{\binom{n}{d+1}}{m}$ possibilities.
	  Example
	     M = randomAbstractSimplicialComplex(6,3,2)
	     prune HH simplicialChainComplex M
          Text
	     Creates a random subsimplicial complex of a given simplicial complex.
          Example
	     K = randomAbstractSimplicialComplex(4)
	     J = randomSubSimplicialComplex(K)
	     inducedSimplicialChainComplexMap(K,J)
    SeeAlso
        "randomAbstractSimplicialComplex"
	"randomSubSimplicialComplex"
        "random"
	"RandomIdeals"
///



--------------------------------------------
-- Documentation of methods and functions --
--------------------------------------------

--
-- Types
--

doc ///
     Key
     	  AbstractSimplicialComplex
     Headline
     	  the type of all abstract simplicial complexes
     Description
     	  Text	  
	     The type AbstractSimplicialComplex is a data type for working
	     with abstract simplicial complexes with vertices
	     supported on $[n] = \{1,\dots,n\}$.
///

doc ///
    Key
       	 (NewMethod, AbstractSimplicialComplex)
    SeeAlso
         "new"
///

--
-- Functions and Commands
--

doc ///
    Key
	 (symbol ==,AbstractSimplicialComplex,AbstractSimplicialComplex)
    Headline
         decide if two simplicial complexes are equal
    Usage
        K == L
    Inputs
        K : AbstractSimplicialComplex
	L : AbstractSimplicialComplex
    Outputs
       : Boolean
    Description
          Text
	     Decides if two simplicial complexes are equal.
	  Example
	     randomAbstractSimplicialComplex(4) == randomAbstractSimplicialComplex(4)
///	     

doc ///
    Key
         randomAbstractSimplicialComplex
	 (randomAbstractSimplicialComplex,ZZ)
	 (randomAbstractSimplicialComplex,ZZ,ZZ)
	 (randomAbstractSimplicialComplex,ZZ,ZZ,ZZ)
    Headline
          create a random abstract simplicial complex
    Usage
        randomAbstractSimplicialComplex(n)
	randomAbstractSimplicialComplex(n,r)
	randomAbstractSimplicialComplex(n,m,d)
    Inputs
        n : ZZ
	r : ZZ
	m : ZZ
	d : ZZ
    Outputs
        : AbstractSimplicialComplex
    Description
          Text
	     Create a random abstract simplicial complex with vertices
	     supported on a subset of $[n] = \{1,\dots ,n\}$.  
          Example
	     K = randomAbstractSimplicialComplex(4)
	  Text
	     Create a random simplicial complex on $[n]$ with dimension at
	     most equal to r.
          Example
	     L = randomAbstractSimplicialComplex(6,3)
	  Text
	     Create the random complex $Y_d(n,m)$ which has vertex set
             $[n]$ and $(d − 1)$-skeleton, and has $m$ $d$-dimensional faces,
             chosen at random from all $\binom{\binom{n}{d+1}{m}$ possibilities.
	     Such random simplicial complexes appear in lots of different
	     contexts including in the article
	     @HREF("https://www.tandfonline.com/doi/abs/10.1080/10586458.2018.1473821",
		       "Cohen-Lenstra heuristics for torsion in homology of random complexes")@
	     by  M. Kahle, F. H. Lutz, A. Newman, and K. Parsons [Exp. Math. vol. 29, no. 3 (2020)].
	     The output of the method may produce, in some cases, complexes which have fewer than
	     $m$ faces of dimension $d$.  Further, not all $d-1$ skeletons will be complete.
	  Example
	     N = randomAbstractSimplicialComplex(6,3,2)
    SeeAlso
        "randomSubSimplicialComplex"
        "random"
	"RandomIdeals"
///

doc ///
    Key
         randomSubSimplicialComplex
	 (randomSubSimplicialComplex,AbstractSimplicialComplex)
	 (randomSubSimplicialComplex,AbstractSimplicialComplex)
    Headline
          create a random subsimplicial complex
    Usage
        randomSubSimplicialComplex(K)
    Inputs
        K : AbstractSimplicialComplex
    Outputs
        : AbstractSimplicialComplex
    Description
          Text
	     Creates a random subsimplicial complex of a given
	     simplicial complex.  
          Example
	     K = randomAbstractSimplicialComplex(4)
	     J = randomSubSimplicialComplex(K)
    SeeAlso
          "randomAbstractSimplicialComplex"
///

doc ///
     Key
     	  ambientAbstractSimplicialComplex
	  (ambientAbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  the ambient simplex
     Usage
          ambientAbstractSimplicialComplex(K)
     Inputs
          K : AbstractSimplicialComplex
     Outputs
          : AbstractSimplicialComplex
     Description
     	  Text	  
	     If an abstract simplicial complex has vertices supported on a
	     subset of $[n] = \{1,\dots,n\}$, and including $n$, then it seems useful
	     to regard this simplicial complex as being a subsimplicial complex
	     of the simplex on $[n]$.  This method returns this simplex as
	     the ambient simplicial complex.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplex(K)
///

doc ///
     Key
     	  ambientAbstractSimplicialComplexSize
	  (ambientAbstractSimplicialComplexSize,AbstractSimplicialComplex)
     Headline
     	  the ambient simplex size
     Usage
         ambientAbstractSimplicialComplex(K)
     Inputs
         K : AbstractSimplicialComplex
     Outputs
         : ZZ
     Description
     	  Text	  
	     If an abstract simplicial complex has vertices supported on a subset of $[n] = \{1,\dots,n\}$,
	     and including $n$, then it seems useful to regard this simplicial complex as being a
	     subsimplicial complex of the simplex on $[n]$.  This method simply returns this largest
	     integer $n$.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplexSize(K)
///

doc ///
     Key
     	  inducedSimplicialChainComplexMap
	  (inducedSimplicialChainComplexMap,AbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  the induced maps that arise via inclusions of abstract simplicial complexes
     Usage
          inducedSimplicialChainComplexMap(K,L)
     Inputs
          K : AbstractSimplicialComplex
	  L : AbstractSimplicialComplex
     Outputs
          : ComplexMap
     Description
     	  Text	  
	     If an abstract simplicial complex can be regarded as a subsimplicial complex
	     of another abstract simplicial complex, then it is useful to calculate the
	     induced map at the level of simplicial chain complexes.  This is made possible
	     by the method inducedSimplicialChainComplexMap.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplex(K)
	       inducedSimplicialChainComplexMap(J,K)
	       L = abstractSimplicialComplex {{}}
               inducedSimplicialChainComplexMap(L,L)
	       M = abstractSimplicialComplex {{1}}
	       L = abstractSimplicialComplex {{}}
	       inducedSimplicialChainComplexMap(M,L)
     SeeAlso
          "inducedReducedSimplicialChainComplexMap"             
///

doc ///
     Key
     	  inducedReducedSimplicialChainComplexMap
	  (inducedReducedSimplicialChainComplexMap,AbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  the induced maps that arise via inclusions of abstract simplicial complexes
     Usage
         inducedReducedSimplicialChainComplexMap(K,L)
     Inputs
          K : AbstractSimplicialComplex
	  L : AbstractSimplicialComplex
     Outputs
          : ComplexMap
     Description
     	  Text	  
	     If an abstract simplicial complex can be regarded as a subsimplicial complex
	     of another abstract simplicial complex, then it is useful to calculate the
	     induced map at the level of reduced simplicial chain complexes.  This is made
	     possible by the method inducedReducedSimplicialChainComplexMap.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplex(K)
	       inducedReducedSimplicialChainComplexMap(J,K)
               L = abstractSimplicialComplex {{}}
               inducedReducedSimplicialChainComplexMap(L,L)
	       M = abstractSimplicialComplex {{1}}
	       L = abstractSimplicialComplex {{}}
	       inducedReducedSimplicialChainComplexMap(M,L)
     SeeAlso
          "inducedSimplicialChainComplexMap"             
///

doc ///
     Key
     	  reducedSimplicialChainComplex
	  (reducedSimplicialChainComplex,AbstractSimplicialComplex)
     Headline
     	  the reduced homological chain complex that is determined by an abstract simplicial complex
     Usage
         reducedSimplicialChainComplex(K)
     Inputs
         K : AbstractSimplicialComplex
     Outputs
         : Complex
     Description
     	  Text	  
	     This method returns the reduced homological chain complex (i.e., there is a nonzero term in
	     homological degree $-1$ that corresponds to the empty face) that is associated
	     to an abstract simplicial complex.  The chain complex is defined over the integers.
          Example
	       K = abstractSimplicialComplex({{1,2,3},{2,4,9},{1,2,3,5,7,8},{3,4}})
	       reducedSimplicialChainComplex(K)
///

doc ///
     Key
     	  simplicialChainComplex
	  (simplicialChainComplex,AbstractSimplicialComplex)
     Headline
     	  the non-reduced homological chain complex that is determined by an abstract simplicial complex
     Usage
         simplicialChainComplex(K)
     Inputs
         K : AbstractSimplicialComplex
     Outputs
         : Complex
     Description
     	  Text	  
	     This method returns the (non-reduced) homological chain complex (i.e., there is no
	     nonzero term in homological degree $-1$ that corresponds to the empty face) that is associated
	     to an abstract simplicial complex.  The chain complex is defined over the integers.
	  Example
	       K = abstractSimplicialComplex({{1,2,3},{1,4,5},{2,4,5,7}})
	       C = simplicialChainComplex(K)
///

doc ///
     Key
     	  abstractSimplicialComplex
	  (abstractSimplicialComplex,List)
	  (abstractSimplicialComplex,ZZ)
	  (abstractSimplicialComplex,ZZ,ZZ)
     Headline
     	  the AbstractSimplicialComplex that is determined by an abstract simplicial complex
     Usage
         abstractSimplicialComplex(l)
	 abstractSimplicialComplex(n)
	 abstractSimplicialComplex(n,r)
     Inputs
         l : List
	 n : ZZ
	 r : ZZ
     Outputs
         : AbstractSimplicialComplex	  
     Description
     	  Text	  
	     This method returns the AbstractSimplicialComplex that represents a
	     given abstract simplicial complex.
	     The input is either a given collection of generating faces or an integer.
	     These faces need not
	     be facets.  Moreover, the elements of the faces need not be written 
	     in lexicographic order.  When the input is an integer $n$, the output is the
	     corresponding simplex on $[n]$.  When the input is a pair of integers, $(n,r)$ the output
	     is the simplicial complex on $[n]$ with complete $r$-skeleton.
	  Example
	       abstractSimplicialComplex({{1,2,3,4}})
	       abstractSimplicialComplex({{4,1,2,3}, {3,2,5},{1,5}})
	       abstractSimplicialComplex(4)
	  Text
	     The simplicial complex on $[n]$ with complete $r$-skeleton can be constructed
	     as follows.
	  Example
	     abstractSimplicialComplex(4,2)
///

doc ///
     Key
     	  (symbol _, AbstractSimplicialComplex, ZZ)
     Headline
     	  the $k$ faces of a simplicial complex
     Usage
       K_k
     Inputs
        K : AbstractSimplicialComplex
	k : ZZ
     Outputs
	  :
	   the list of $k$ faces
     Description
     	  Text	  
	     This method returns the collection of $k$ faces of a given
	     AbstractSimplicialComplex.
	  Example
	       K = abstractSimplicialComplex(3)
	       K_3
	       K_2
	       K_1
	       K_0
	       K_(-1)
///

doc ///
     Key
          abstractSimplicialComplexFacets 
          (abstractSimplicialComplexFacets, AbstractSimplicialComplex)
	  (facets, AbstractSimplicialComplex)
     Headline
     	  the facets of a simplicial complex
     Usage
         abstractSimplicialComplexFacets(K)
     Inputs
         K : AbstractSimplicialComplex
     Outputs
         :
	  the list of facets
     Description
     	  Text	  
	     This method returns the collection of facets of a given AbstractSimplicialComplex.
	  Example
	       K = abstractSimplicialComplex(3)
	       abstractSimplicialComplexFacets K
	       facets K
///

doc ///
     Key
          (dim, AbstractSimplicialComplex)
     Headline
     	  the dimension of a simplicial complex
     Usage
          dim(K)
     Inputs
          K : AbstractSimplicialComplex
     Outputs
          : ZZ
     Description
     	  Text	  
	     This method returns the dimension a given AbstractSimplicialComplex.
	  Example
	       K = abstractSimplicialComplex(3)
	       dim K
///

doc ///
          Key
       	   (describe, AbstractSimplicialComplex)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///



-* Test section *-
TEST /// -* [insert short title for this test] *-

needsPackage"Complexes"  -- in order for some tests to run properly the Complexes package needs to be imported 

-- a test for the exported function/methods
-- abstractSimplicialComplex
-- ambientAbstractSimplicialComplex
-- reducedSimplicialChainComplex
-- inducedReducedSimplicialChainComplexMap

assert(K = abstractSimplicialComplex({{1,2},{3}});
     J = ambientAbstractSimplicialComplex(K);
     isWellDefined inducedReducedSimplicialChainComplexMap(J,K))

-- a test for the exported function/methods
-- abstractSimplicialComplex
-- ambientAbstractSimplicialComplex
-- simplicialChainComplex
-- inducedSimplicialChainComplexMap

assert(K = abstractSimplicialComplex({{1,2},{3}});
     J = ambientAbstractSimplicialComplex(K);
     isWellDefined inducedSimplicialChainComplexMap(J,K))

-- another important test for the exportd function/methods
-- abstractSimplicialComplex
-- inducedReducedSimplicialChainComplexMap

assert(L = abstractSimplicialComplex({{}});
     isWellDefined inducedReducedSimplicialChainComplexMap(L,L))

-- another important test for the exportd function/methods
-- abstractSimplicialComplex
-- inducedReducedSimplicialChainComplexMap

assert(M = abstractSimplicialComplex {{1}};
     L = abstractSimplicialComplex {{}};
     isWellDefined inducedReducedSimplicialChainComplexMap(M,L))

-- a test for randomAbstractSimplicialComplex
-- reducedSimplicialChainComplexes
-- and inducedReducedSimplicialChainComplexMap

assert(K = randomAbstractSimplicialComplex(6);
     J = randomSubSimplicialComplex(K);
     isWellDefined inducedReducedSimplicialChainComplexMap(K,J))

-- a test for randomSubSimplicialComplex
-- simplicialChainComplexes
-- and reducedSimplicialChainComplexMap

assert(K = randomAbstractSimplicialComplex(6);
     J = randomSubSimplicialComplex(K);
     isWellDefined inducedSimplicialChainComplexMap(K,J))

-- a complex of tests for abstractSimplicalComplex

assert(abstractSimplicialComplex(0) == abstractSimplicialComplex({{}}))
assert(abstractSimplicialComplex(1) == abstractSimplicialComplex({{1}}))
assert(abstractSimplicialComplex(3) == abstractSimplicialComplex({{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}))
 
-- a couple of tests for simplicialChainComplex

assert(K = abstractSimplicialComplex({{}});
     simplicialChainComplex(K) == 0)

assert(K = abstractSimplicialComplex(2);
    C = complex hashTable { 1 => map(ZZ^2,ZZ^1,matrix(ZZ,{{-1},{1}}))};
simplicialChainComplex(K) == C)

-- a couple of tests for reducedSimplicialChainComplex

assert(K = abstractSimplicialComplex({{}});
    reducedSimplicialChainComplex(K) == complex hashTable {-1 => map(ZZ^0,ZZ^1,zero)})
assert(K = abstractSimplicialComplex(2);
C = reducedSimplicialChainComplex(K);
c = complex hashTable { -1 => map(ZZ^0,ZZ^1,zero), 0 => map(ZZ^1,ZZ^2,matrix(ZZ,{{1,1}})), 1 => map(ZZ^2,ZZ^1,matrix(ZZ,{{-1},{1}}))};
C == c)

-- a test for ambientAbstractSimplicialComplexSize

assert(K = abstractSimplicialComplex({{1,2},{1,3,5},{3,5},{4,8,9},{8,9}});
     ambientAbstractSimplicialComplexSize(K) == 9)
assert(K = abstractSimplicialComplex({{}});
    ambientAbstractSimplicialComplexSize(K) == 0)

-- a test for ambientAbstractSimplicialComplex

assert(K = abstractSimplicialComplex({{1,2},{1,3,5},{3,5}});
    n = ambientAbstractSimplicialComplexSize(K);
    ambientAbstractSimplicialComplex(K) == abstractSimplicialComplex(n))
assert(ambientAbstractSimplicialComplex(abstractSimplicialComplex({{}}))
    == abstractSimplicialComplex(0))

-- a couple of tests for abstractSimplicialComplexFacets
assert(K = abstractSimplicialComplex({{}});
      abstractSimplicialComplexFacets(K) == {{}})
assert(K = abstractSimplicialComplex({{1,2},{1,2,3,5},{1,2,3,4}});
     abstractSimplicialComplexFacets(K) ==  {{1, 2, 3, 4}, {1, 2, 3, 5}})
assert(abstractSimplicialComplexFacets(abstractSimplicialComplex(4)) == {{1,2,3,4}})
///

end--

-* Development section *-

--
--

restart
uninstallPackage "AbstractSimplicialComplexes"
installPackage("AbstractSimplicialComplexes", RemakeAllDocumentation => true)
check "AbstractSimplicialComplexes"



