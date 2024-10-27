-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2024  Nathan Grieve
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
--------------------------------------------------------------------------------


newPackage(
    "AbstractSimplicialComplexes",
    Version => "0.1",
    Date => "24 September 2024",
    Headline => "AbstractSimplicialComplexes",
    Authors => {{ Name => "Nathan Grieve", Email => "nathan.m.grieve@gmail.com", HomePage => "https://sites.google.com/view/nathan-grieve"}},
    AuxiliaryFiles => false,
    DebuggingMode => false,
    PackageImports => {"Complexes"},
    PackageExports => {"Complexes"}
    )

export {"AbstractSimplicialComplex", "abstractSimplicialComplex","simplicialChainComplex", "reducedSimplicialChainComplex", "ambientAbstractSimplicialComplexSize",
    "ambientAbstractSimplicialComplex", "facets", "randomAbstractSimplicialComplex", "randomSubSimplicialComplex",
     "inducedSimplicialChainComplexMap","inducedReducedSimplicialChainComplexMap","areEqual", "dimAbstractSimplicialComplex",
    }

-* Code section *-

---------------------------------------
-- spots
----------------------------------------
-- the spots method is extremely useful
-- but we don't export it 
-----------------------------------------

spots = method()

spots Complex := List => (
  C -> (c := concentration C; toList(c_0 .. c_1)))

max Complex := K -> max spots K
min Complex := K -> min spots K


---------------------------------------
--------------------------------------

--------------------------
-- simplicial set
-------------------------

-- The idea is to make a SimplicalSet as a Type of HashTable as a means
-- For working with AbstractSimplicial Complexes ---
-- The integer keys will output the list of i-simplicies

AbstractSimplicialComplex = new Type of HashTable
AbstractSimplicialComplex.synonym = "simplicial set"

AbstractSimplicialComplex.GlobalAssignHook = globalAssignFunction
AbstractSimplicialComplex.GlobalReleaseHook = globalReleaseFunction
describe AbstractSimplicialComplex := K -> net expression K


new AbstractSimplicialComplex := AbstractSimplicialComplex =>(cl) -> (
    K := newClass(AbstractSimplicialComplex, new HashTable); -- sigh
    K)

---  It will be better to make some additional keys for this class ---
---  For instance a key ambient, which will be an integer n which specifies the "ambient n-simplex on [n]"
---  That we wish to view the SimplicalSet as being contained in ---
---  This would be slightly different than the ambient size -- i.e., the smallest simplex that contains
---  The given simplicial complex
---  But actually how this is set-up should suffice ---
---  We will also want to make a key "generators" which points to the list of generators used to define
---  We would want to make ``maps" between SimplicalSets 

spots AbstractSimplicialComplex := List => (
  K -> sort select(keys K, i -> class i === ZZ))



-- This returns the p-faces of a simplicial set 

AbstractSimplicialComplex _ ZZ := AbstractSimplicialComplex => (K,p) -> (
  if K#?p then K#p 
  )

-- given a list of subsets L and A \in L decide if A is maximal

isMaximal :=(x,L) -> (
myList := select(L,i -> isSubset(x,i));
if #myList == 1 then
return true
else return false
    )

-- select the maximal subsets (i.e., facets) of a list of subsets

listFacets := (L) -> (
select(L,i-> isMaximal(i,L))
    )


--- return the facets of a simplicial set

facets = method()

facets(AbstractSimplicialComplex) := List => K ->(
    L := flatten(apply(spots K, i-> K_i));
    return listFacets(L)
    )


--- decide if two simplicial sets are equal

areEqual = method()

areEqual(AbstractSimplicialComplex,AbstractSimplicialComplex) := Boolean => (K,L) ->(
    return (facets K) == (facets L)
    )

--- return the dimension of a simplicial set

dimAbstractSimplicialComplex = method()

dimAbstractSimplicialComplex(AbstractSimplicialComplex) := ZZ => (K) -> (
    return max apply(facets(K), i -> #i)
    )


--- Constructors for AbstractSimplicialComplexs

abstractSimplicialComplex = method()

----  We need to make a sort of ``main primitive constructor" for simplicial sets
----  We need to make a method perhaps to check if a simplicial set is a simplicial complex (i.e., to check closure under taking subsets of a face)


-- the most basic constructor of a AbstractSimplicialComplex

-- The idea is to make a simplical set starting from a list of faces.
--  The list of faces need not be facets.
--  The constructor returns the simplicial complex (with all of its faces) that is
--  generated by this list of faces 
--  By default, it is assumed that the kfaces are all lex ordered positive integers 

makeKFaces := (L,k) -> (
    toList(set(flatten(apply(#L, i -> subsets(sort L_i,k)))))
    )

makeAllFaces := (L) -> (
    numberOfFaces := #L;
--  find the highest dimensional face    
    n := max(apply(numberOfFaces, i-> # (L_i)));
    flatten(for k from 0 to n list {k-1 => sort makeKFaces(L,k)})
    )


abstractSimplicialComplex(List) := AbstractSimplicialComplex => L -> (
    return new AbstractSimplicialComplex from makeAllFaces(L)
    )


--- The following method will make the (n-1)-dimensional n-simplex on [n] = {1,...,n}
abstractSimplicialComplex(ZZ) := AbstractSimplicialComplex => (n) -> (
    L := for i from 1 to n list i;
    return abstractSimplicialComplex({L})
    )

--- Make the "r-skeleton" on [n] = {1,...n}

abstractSimplicialComplex(ZZ,ZZ) := AbstractSimplicialComplex => (n,r) -> (
    return abstractSimplicialComplex subsets(for i from 1 to n list i,r)
    )

--

-- making random simplicial sets --

-- make a random subset of {1,...,n}

randomSubset = method()

randomSubset(ZZ) := List => (n) -> (
   setRandomSeed(currentTime());
   k := random(1,n); 
   sort unique (for i from 1 to k list (random(1,n)))
    )

-- random size r subset --

randomSubset(ZZ,ZZ) := List => (n,r) -> (
   setRandomSeed(currentTime());
   sort unique (for i from 1 to r list (random(1,n)))
    )


-- make a random subset of a given set

randomSubset(List) := List => (L) -> (
    setRandomSeed(currentTime());
    n := #L;
    k := random(0,n);
    mySubset := subsets(L,k);
    mySubset_(random(binomial(n,k)))
    )

-- a variant of this is to make a random k element subset of a given set --

-- The following will make a "random" simplicial complex on {1,...,n} --

randomAbstractSimplicialComplex = method()

randomAbstractSimplicialComplex(ZZ) := AbstractSimplicialComplex => (n) -> (
     setRandomSeed(currentTime());
     listLength := 1 + random(2^n);
     abstractSimplicialComplex unique(for i from 1 to listLength list randomSubset(n))
     )

------

--  it likely would also be good to make a randomSimplicial complex
--  on [n] with dimension at most equal to r

-----

randomAbstractSimplicialComplex(ZZ,ZZ) := AbstractSimplicialComplex =>(n,r) -> (
     setRandomSeed(currentTime());
     listLength := 1 + random(binomial(n,r));
     abstractSimplicialComplex unique(for i from 1 to listLength list randomSubset(n,r))
    )


-- can we make the random complex Y_d(n,m) which has vertex set
-- [n] and complete (d − 1)-skeleton, and has exactly m d-dimensional faces,
-- chosen at random from all binomial(binomial(n,d+1),m) possibilities.
-- Such random complexes appear in lots of different contexts including in the article
-- COHEN–LENSTRA HEURISTICS FOR TORSION IN HOMOLOGY OF RANDOM COMPLEXES
-- (MATTHEW KAHLE, FRANK H. LUTZ, ANDREW NEWMAN, AND KYLE PARSONS) --
-- Some additinal testing of this is needed 

randomAbstractSimplicialComplex(ZZ,ZZ,ZZ) := (n,m,d) -> (
    setRandomSeed(currentTime());
    L := for i from 1 to n list i;
    dDimlSubsets := subsets(L,d+1);
    randomFaces := for i from 1 to m list (dDimlSubsets#(random(binomial(n,d+1))));
    append(append(randomFaces,{L}),subsets(L,d));
    return abstractSimplicialComplex(randomFaces)
    )

randomSubSimplicialComplex = method()

randomSubSimplicialComplex(AbstractSimplicialComplex) := AbstractSimplicialComplex => (K) -> (
 setRandomSeed(currentTime());
 L := facets K;
 abstractSimplicialComplex unique apply(L, i-> randomSubset(i))
)

---

-- ambient simplicial set

ambientAbstractSimplicialComplexSize = method() -- return the size of the underyling ambient simplex

ambientAbstractSimplicialComplexSize(AbstractSimplicialComplex) := (K) -> (
    max flatten(K_0)
    )


ambientAbstractSimplicialComplex = method() -- return the underlying ambient simplex 


ambientAbstractSimplicialComplex(AbstractSimplicialComplex) := AbstractSimplicialComplex => (K) -> (
    return abstractSimplicialComplex(ambientAbstractSimplicialComplexSize(K))
    )

---
-- Another method that could be added later is a script to check that a proposed "AbstractSimplicialComplex"
-- is indeed a "AbstractSimplicialComplex" i.e., that the closure property on subsets is indeed satisfied
--  this is something that we will postpone for the present time

---------

--  There are many ways to approach 
--  The simplical boundary map
-- For X a given simplicial complex
--  Perhaps the most straight forward way
--  is via 
-- \partial_k : C_k(X) \rightarrow C_{k-1}(X)
-- Here C_k(X) is the free 
-- \ZZ-module (or \kk-vector space)
-- on the set of k+1 simplicies 
-- (i.e., the set of k+1 combinations
-- of {1,...,n})

  
--  There are many ways to approach 
--  The simplical boundary map
-- For X a given simplicial complex
--  Perhaps the most straight forward way
--  is via 
-- \partial_k : C_k(X) \rightarrow C_{k-1}(X)
--  Here C_k(X) is the free 
-- \ZZ-module (or \kk-vector space)
-- on the set of k+1 simplicies 
-- (i.e., the set of k+1 combinations
-- of {1,...,n})

--  Given input a k+1 lex segment a = [a_0,...,a_k] ---- i.e., a k-face
--  Compute its image under the boundary map
--  It seems most straight forward to give the 
-- output as a k+1 tuple with entry i having the
-- form [(-1)^i,d_i(a)]
-- here d_i(a) is a with the i-th lex entry removed
--  the following is more simply just for
-- testing purposes and is not used explicitly in the sequel
--partial := (L) -> ( 
 -- apply(0 .. (#L-1), i -> {(-1)^i, drop(L,{i,i})})

 
-- The following function seems useful to  
-- useful to construct
-- the simplicial chain complex map
-- given a k-face y and a k+1 - face x
-- decide if it equals \partial(x,i)
-- for some i


isDLexSeqI := (y,x) -> (
    k := #y;
    sign := 0;
    for i from 0 to # x do (
        z := drop(x,{i,i});
        if y == z then (sign = (-1)^i;
            break);
);
return sign
)



-- make a constructor for making matrices
-- that represented the simplicial boundary
-- maps of a given simplical complex
-- what follows appears to work OK
--  more testing is required.

-- make a method for now to allow for additional testing

simplicialMakeMatrix = method()

simplicialMakeMatrix(List,List) := (kPlusOneFaces,kFaces) -> (
    n := # kPlusOneFaces;
    m := # kFaces;
    matrixList := 
    	for i from 0 to m-1 list (
    for j from 0 to n-1 list (
	   isDLexSeqI((kFaces)#i,(kPlusOneFaces)#j))
	);
    return matrix(matrixList)
)


--  We can finally make the entire reduced homology chain complex in the following way
-- Given as input the simplcial complex represented as a simplicial set --
--  This will produce the reduced chain complex (so the empty set will
--  appear in the chain complex)

reducedSimplicialChainComplex = method() -- return the chain complex (with contribution from the empty face) that is associated to a simplicial set (i.e., an abstract simplicial complex)

reducedSimplicialChainComplex(AbstractSimplicialComplex) := Complex => (L) ->
(
    n := max spots L;
        if n == -1 then (return complex hashTable {-1 => map(ZZ^0,ZZ^1,zero)})
    else(
    mapsList := for i from 0 to n list (i => simplicialMakeMatrix(L#i,L#(i-1)));
    append(mapsList,-1 => map(ZZ^0,target(mapsList#0)#1,zero)););
    return complex hashTable mapsList
	)

simplicialChainComplex = method() --  return the non-reduced simplicial chain complex (i.e., the chain complex with no contribution from the empty face)

simplicialChainComplex(AbstractSimplicialComplex) := Complex => (L) ->
(
 return(naiveTruncation(reducedSimplicialChainComplex L, 0, infinity))
	)
        
---  Another method that would be of interest,
--  is to give an inclusion (or more general a morphism)
---- of simplicial sets, then compute the induced chain complex morphism of SimplicialChainComplexes
--- An important special case would be to view a
--  sub simplicial set of the full simplicial set (simplex) and then to compute
--- the corresponding induced inclusion morphism.

---  A first step is to make an k-face inclusion map given an inclusion of simplicial sets 
---  Assume that L <= H
---  If L_k has no faces then the method returns an error message
---  Otherwise the method produces the appropriate matrix
---  That induces the corresponding inclusion map

--- This seems to work on some examples but needs to be tested more

inducedKFaceSimplicialChainComplexMap = method()

inducedKFaceSimplicialChainComplexMap(ZZ,AbstractSimplicialComplex,AbstractSimplicialComplex) := (k,H,L) ->
(
M := L_k;
N := H_k;
n := # M;
m := # N;
myMatrixList := for i from 0 to m-1 list (
    for j from 0 to n-1 list (
	if N#i == M#j then 1 else 0 
	)
    );
return matrix myMatrixList
)



--If H <= L then give the induced chain complex map for (non-reduced) simplicalChainComplexes

inducedSimplicialChainComplexMap = method()

inducedSimplicialChainComplexMap(AbstractSimplicialComplex,AbstractSimplicialComplex) := (L,H) ->
(
    h := simplicialChainComplex H;
    l := simplicialChainComplex L;
    f := hashTable apply(spots h, i -> if i == -1 then i => map(ZZ^0,ZZ^0,zero) else i => inducedKFaceSimplicialChainComplexMap(i,L,H));
    return map(l,h,f);
   )

--If H <= L then give the induced chain complex map for reduced simplicalChainComplexes

inducedReducedSimplicialChainComplexMap = method()

inducedReducedSimplicialChainComplexMap(AbstractSimplicialComplex,AbstractSimplicialComplex) := (L,H) -> (
    h := reducedSimplicialChainComplex H;
    l := reducedSimplicialChainComplex L;
    f := hashTable apply(spots h, i -> if i == -1 then i => map(l_(-1),h_(-1),id_(h_(-1))) else i => inducedKFaceSimplicialChainComplexMap(i,L,H));
    return map(l,h,f);
    )


-----
 
-* Documentation section *-
beginDocumentation()

document { 
  Key => AbstractSimplicialComplexes,
  Headline => "a package for working with abstract simplicial complexes",
  "In this package, by a slight abuse of termionalogy we mostly refer to abstract simplicial complexes as 'AbstractSimplicialComplexs'.  By our viewpoint, `abstract simplicial complexes' have vertices 
supported on the set [n] := {1,...,n}.
  The aim of this package is to provide a methology for working with such objects directly.  We are especially interested in homological aspects thereof; in particular
we provide methods for working with the chain complexes that are associated to each abstract simplicial complex.",

   SUBSECTION "An overview of this package",
   UL {
    TO "How to make abstract simplicial complexes", 
    TO "How to make reduced and non-reduced simplicial chain complexes",
    TO "How to make subsimpliical complexes and induced simplicial chain complex maps",
    },
}


--------------------------------------------
-- Package overview examples ---------------
--------------------------------------------

doc ///
     Key
     	  "How to make abstract simplicial complexes"
     Headline
     	  Using the type AbstractSimplicialComplexs to represent abstract simplicial complexes
     Description
     	  Text	  
	     The type AbstractSimplicialComplex is a data type for working with
	     abstract simplicial complexes with vertices supported on [n] = {1,...,n}.
	     Here we illustrate some of the most basic ways to interact with this data type.
          Text
	     The simplicial complex that is generated by {1,2,3,4}, {2,3,5} and {1,5} can be
	     constructed in the following way.	    
	  Example
               abstractSimplicialComplex({{1,2,3,4}, {2,3,5},{1,5}})
          Text
	       The simplex on the vertex set [4] can be constructed as     
          Example
	       abstractSimplicialComplex(4) 
///



doc ///
     Key
     	  "How to make reduced and non-reduced simplicial chain complexes"
     Headline
     	  Simplicial homological constructors 
     Description
     	  Text	  
	     Non-reduced and reduced simplicial chain complexes can be constructed in the following way.
	     This is illustrated in the following way.
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
     	  Induced simplicial chain complex maps via subsimplicial complexes 
     Description
     	  Text	  
	     Given a subsimplicial complex there are induced simplicial chain complex maps.
	     can be used to make non-reduced and reduced simplicial chain complexes.
	     This is illustrated in the following way.
	  Example
	     K = randomAbstractSimplicialComplex(4)
             randomSubSimplicialComplex(K)
             facets(K)
///

doc ///
     Key
     	  "Calculations with random simplicial complexes"
     Headline
     	  Homological calculations on random simplicial complexes
     Description
     	  Text	  
	     In what follows we illustrate a collection of homological calculations that
	     can be performed on random simplicial complexes. 
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
     	  the type of all simplicial sets
     Description
     	  Text	  
	     The type AbstractSimplicialComplex is a data type for working with
	     abstract simplicial complexes with vertices supported on [n] = {1,...,n}.
///


--
-- Functions and Commands
--

doc ///
    Key
         areEqual
	 (areEqual,AbstractSimplicialComplex,AbstractSimplicialComplex)
    Headline
         Decide if two simplicial sets are equal
    Description
          Text
	     Decides if two simplicial sets are equal
	  Example
	     areEqual(randomAbstractSimplicialComplex(4),randomAbstractSimplicialComplex(4))
///	     

doc ///
    Key
         randomAbstractSimplicialComplex
	 (randomAbstractSimplicialComplex,ZZ)
	 (randomAbstractSimplicialComplex,ZZ,ZZ)
	 (randomAbstractSimplicialComplex,ZZ,ZZ)
    Headline
          Create a random simplicial set
    Description
          Text
	     Creates a random abstract simplicial complex with vertices supported on a subset of [n] = {1,...,n}
          Example
	     setRandomSeed(currentTime());
	     K = randomAbstractSimplicialComplex(4)
    SeeAlso
        "random"
	"randomSquareFreeMonomialIdeal"
///

doc ///
    Key
         randomSubSimplicialComplex
	 (randomSubSimplicialComplex,AbstractSimplicialComplex)
    Headline
          Create a random sub-simplicial set
    Description
          Text
	     Creates a random sub-simplicial complex of a given simplicial complex
          Example
	     K = randomAbstractSimplicialComplex(4)
	     J = randomSubSimplicialComplex(K)
///


doc ///
     Key
     	  ambientAbstractSimplicialComplex
	  (ambientAbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  the ambient simplex
     Description
     	  Text	  
	     If an abstract simplicial complex has vertices supported on a subset of [n] = {1,...,n}, and including n,
	     then it seems useful to regard this simplicial complex as being a subsimplicial
	     complex of the simplex on [n].  This method returns this simplex as
	     the ambient simplical complex.
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
     Description
     	  Text	  
	     If an abstract simplicial complex has vertices supported on a subset of [n] = {1,...,n], and including n,
	     then it seems useful to regard this simplicial complex as being a subsimplicial
	     complex of the simplex on [n].  This method simply returns this largest integer n.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplexSize(K)
///



doc ///
     Key
     	  inducedSimplicialChainComplexMap
	  (inducedSimplicialChainComplexMap,AbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  induced maps that arise via inclusions of abstract simplicial complexes
     Description
     	  Text	  
	     If an abstract simplicial complex can be regarded as a subsimplicial complex of another
	     abstract simplicial complex, then it is useful to calculate the induced map at the level of
	     Simplicial Chain Complexes.  This is made
	     possible by the method inducedSimplicialChainComplexMap.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplex(K)
	       inducedSimplicialChainComplexMap(J,K)
///

doc ///
     Key
     	  inducedReducedSimplicialChainComplexMap
	  (inducedReducedSimplicialChainComplexMap,AbstractSimplicialComplex,AbstractSimplicialComplex)
     Headline
     	  induced maps that arise via inclusions of abstract simplicial complexes
     Description
     	  Text	  
	     If an abstract simplicial complex can be regarded as a subsimplicial complex of another
	     abstract simplicial complex, then it is useful to calculate the induced map at the level of
	     Reduced Simplicial Chain Complexes.  This is made
	     possible by the method inducedReducedSimplicialChainComplexMap.
	  Example
	       K = abstractSimplicialComplex({{1,2},{3}})
	       J = ambientAbstractSimplicialComplex(K)
	       inducedReducedSimplicialChainComplexMap(J,K)
///



doc ///
     Key
     	  reducedSimplicialChainComplex
	  (reducedSimplicialChainComplex,AbstractSimplicialComplex)
     Headline
     	  The reduced homological chain complex that is determined by an abstract simplicial complex 
     Description
     	  Text	  
	     This method returns the reduced homological chain complex (i.e., there is a nonzero term in
		 homological degree -1 that corresponds to the empty face) that is asociated
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
     	  The non-reduced homological chain complex that is determined by an abstract simplicial complex 
     Description
     	  Text	  
	     This method returns the (non-reduced) homological chain complex (i.e., there is no nonzero term in
		 homological degree -1 that corresponds to the empty face) that is asociated
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
     Headline
     	  The abstractSimplicialComplex that is determined by an abstract simplicial complex 
     Description
     	  Text	  
	     This method returns the AbstractSimplicialComplex that represents a
	     given abstract simplicial complex.
	     The input is either a given collection of generating faces or an integer.
	     These facets need not
	     be facets.  Moreover, the elements of the faces need not be written 
	     in lexiographic order.  When the input is an integer, the output is the
	     corresponding simplex.
	  Example
	       abstractSimplicialComplex({{1,2,3,4}})
	       abstractSimplicialComplex({{4,1,2,3}, {3,2,5},{1,5}})
	       abstractSimplicialComplex(4)
///

doc ///
     Key
     	  (symbol _, AbstractSimplicialComplex, ZZ)
     Headline
     	  The k faces of a simplicial set  
     Description
     	  Text	  
	     This method returns the collection of k faces of a given AbstractSimplicialComplex.
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
          facets 
          (facets, AbstractSimplicialComplex)
     Headline
     	  The facets of a simplicial set  
     Description
     	  Text	  
	     This method returns the collection of facets of a given AbstractSimplicialComplex.
	  Example
	       K = abstractSimplicialComplex(3)
	       facets K
///

doc ///
     Key
          dimAbstractSimplicialComplex 
          (dimAbstractSimplicialComplex, AbstractSimplicialComplex)
     Headline
     	  The dimension of a simplicial complex  
     Description
     	  Text	  
	     This method returns the dimension a given AbstractSimplicialComplex.
	  Example
	       K = abstractSimplicialComplex(3)
	       dimAbstractSimplicialComplex K
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
-- test code and assertions here
-- may have as many TEST sections as needed
///

end--

-* Development section *-

--
--

restart
uninstallPackage "AbstractSimplicialComplexes"
installPackage("AbstractSimplicialComplexes", RemakeAllDocumentation => true)
check "AbstractSimplicialComplexes"
viewHelp"AbstractSimplicialComplexes"

--
--

