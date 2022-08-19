--*- coding: utf-8 -*-

---------------------------------------------------------------------------
-- PURPOSE: Computations with vector bundles on toric varieties 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : November 2008, November 2009, April 2010
---------------------------------------------------------------------------
newPackage("OldToricVectorBundles",
    Headline => "vector bundles on toric varieties",
    Version => "1.1",
    Date => "August 21, 2014",
    Authors => {
         {Name => "René Birkner",
	  HomePage => "http://page.mi.fu-berlin.de/rbirkner/indexen.htm",
	  Email => "rbirkner@math.fu-berlin.de"},
         {Name => "Nathan Ilten",
	  HomePage => "http://people.cs.uchicago.edu/~nilten/",
	  Email => "nilten@cs.uchicago.edu"},
         {Name => "Lars Petersen",
	  Email => "petersen@math.fu-berlin.de"}},
    Keywords => {"Toric Geometry"},
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Computations with equivariant toric vector bundles",
	 "acceptance date" => "2010-06-15",
	 "published article URI" => "http://j-sag.org/Volume2/jsag-3-2010.pdf",
	 "published code URI" => "http://j-sag.org/Volume2/ToricVectorBundles.m2",
	 "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/ToricVectorBundles.m2",
	 "release at publication" => "314a1e7a1a5f612124f23e2161c58eabeb491f46",
	 "version at publication" => "1.0",
	 "volume number" => "2",
	 "volume URI" => "http://j-sag.org/Volume2/"
	 },
    DebuggingMode => false,
    PackageExports => {"OldPolyhedra"}
    )

-- Check version compatibility of Polyhedra
if (options OldPolyhedra)#Version < "1.1" then error("expected at least version 1.1 of OldPolyhedra to be installed.")

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2010 René Birkner, Nathan Owen Ilten, and Lars Petersen
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

export {"ToricVectorBundle",
     "ToricVectorBundleKaneyama", 
     "ToricVectorBundleKlyachko",
     "toricVectorBundle", 
     "addBase", 
     "addBaseChange", 
     "addDegrees", 
     "addFiltration", 
     "areIsomorphic", 
     "base",
     "cartierIndex",
     "charts",
     "cocycleCheck", 
     "cotangentBundle",
     "deltaE",
     "details",  
     "eulerChi", 
     "existsDecomposition", 
     "filtration", 
     "findWeights", 
     "isGeneral", 
     "isomorphism", 
     "isVectorBundle", 
     "randomDeformation",
     "regCheck", 
     "tangentBundle", 
     "twist", 
     "weilToCartier", 
     "hirzebruchFan",
     "pp1ProductFan", 
     "projectiveSpaceFan"}


protect allRaysTable
protect isoMatrix
protect gradedRing
protect cech
protect isVB
protect cocyle
protect degreesList
protect cocycle
protect weights
protect isomorphic

---------------------------------------------------------------------------
-- DEFINING NEW TYPES
---------------------------------------------------------------------------


-- Defining the new type ToricVectorBundle, the parent type to the two types of TVB
ToricVectorBundle = new Type of HashTable

-- Defining the new type ToricVectorBundleKaneyama
ToricVectorBundleKaneyama = new Type of ToricVectorBundle
ToricVectorBundleKaneyama.synonym = "vector bundle on a toric variety (Kaneyama's description)"
globalAssignment ToricVectorBundleKaneyama

-- Defining the new type ToricVectorBundleKlyachko
ToricVectorBundleKlyachko = new Type of ToricVectorBundle
ToricVectorBundleKlyachko.synonym = "vector bundle on a toric variety (Klyachko's description)"
globalAssignment ToricVectorBundleKlyachko

-- Modifying the standard output for a ToricVectorBundleKaneyama to give an overview of its characteristica
net ToricVectorBundleKaneyama := tvb -> ( horizontalJoin flatten ( 
	  "{", 
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"dimension of the variety",
			                      "rank of the vector bundle",
					      "number of affine charts"}, key -> (net key, " => ", net tvb#key))),
	  "}" ))

-- Modifying the standard output for a ToricVectorBundleKlyachko to give an overview of its characteristica
net ToricVectorBundleKlyachko := tvb -> ( horizontalJoin flatten ( 
	  "{", 
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"dimension of the variety",
					      "rank of the vector bundle",
					      "number of affine charts",
					      "number of rays"}, key -> (net key, " => ", net tvb#key))),
	  "}" ))


---------------------------------------------------------------
-- Sorting rays
---------------------------------------------------------------

-- A ray is a matrix ZZ^n <-- ZZ^1, so rays can be sorted by assembling them
-- into a matrix and calling "sortColumns".  We sort the rays as in the package
-- OldPolyhedra, so that changes to the algorithm for computing the hash code of
-- matrices doesn't affect what we do.

raySort = value OldPolyhedra#"private dictionary"#"raySort"

---------------------------------------------------------------
-- FUNCTIONS TO CONSTRUCT VECTOR BUNDLES AND MODIFY THEM
---------------------------------------------------------------


-- PURPOSE : Building a Vector Bundle of rank 'k' on the Toric Variety given by the Fan 'F'
toricVectorBundle = method(Options => true)

--   INPUT : '(k,F)',  a strictly positive integer 'k' and a pure and full dimensional Fan 'F'
--  OUTPUT : A ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
toricVectorBundle (ZZ,Fan) := {"Type"=>"Klyachko"} >> opts -> (k,F) -> (
     if opts#"Type" == "Kaneyama" then makeVBKaneyama(k,F) else if opts#"Type" == "Klyachko" then makeVBKlyachko(k,F) else error("Expected Type to be Klyachko or Kaneyama."))


--   INPUT : '(k,F,L1,L2)',  a strictly positive integer 'k',a pure and full dimensional Fan 'F', and two lists 'L1' and 'L2'
--  OUTPUT : A ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko where the base matrices are given in the first list and the 
--     	     filtration matrices are given in the second list, 
--     	     if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama where the degree matrices are given in the first list and the
--     	     transition matrices are given in the second list.
toricVectorBundle (ZZ,Fan,List,List) := {"Type"=>"Klyachko"} >> opts -> (k,F,L1,L2) -> (
     if opts#"Type" == "Kaneyama" then makeVBKaneyama(k,F,L1,L2) else if opts#"Type" == "Klyachko" then makeVBKlyachko(k,F,L1,L2) else error("Expected Type to be Klyachko or Kaneyama."))


-- PURPOSE : Changing the base matrices of a given ToricVectorBundleKlyachko to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundle 'tvb' and a list 'L'of k by k matrices over a common ring/field, one for each
--     	    	      	   ray of the underlying fan
--  OUTPUT : The ToricVectorBundleKlyachko 'tvb' 
-- COMMENT : Note that the  matrices in 'L' will be assigned to the rays in the order they appear in rays tvb
addBase = method(TypicalValue => ToricVectorBundleKlyachko)
addBase (ToricVectorBundleKlyachko,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     k := tvb#"rank of the vector bundle";
     n := tvb#"number of rays";
     R := raySort toList tvb#"ToricVariety"#"rays";
     -- Checking for input errors
     if n != #L then error("Expected number of matrices to match number of rays of the fan.");
     if any(L, l -> not instance(l,Matrix)) then error("Expected the bases to be given as matrices.");
     P := unique apply(L,ring);
     if #P != 1 then (
	  if P === {QQ,ZZ} or P === {ZZ,QQ} then (
	       L = apply(L, l -> promote(l,QQ));
	       P = {QQ})
	  else error("Expected all the bases to be over the same ring."));
     -- Creating the table of bases for the rays
     baseTable := hashTable apply(n, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if numColumns M != k or numRows M != k then error("Expected the base change matrices to be rank times rank matrices.");
	       if det M == 0 then error("Expected the bases to have full rank.");
	       -- Inserting the matrix at the i-th position
	       R#i => M));
     -- Writing the bases into the bundle
     new ToricVectorBundleKlyachko from {
	  "ring" => first P,
	  "rayTable" => tvb#"rayTable",
	  "baseTable" => baseTable,
	  "filtrationMatricesTable" => tvb#"filtrationMatricesTable",
	  "filtrationTable" => tvb#"filtrationTable",
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "number of rays" => tvb#"number of rays",
	  symbol cache => new CacheTable})


-- PURPOSE : Changing the transition matrices of a given ToricVectorBundle to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundle 'tvb' and a list 'L'of k by k matrices over QQ, one for each 
--     	    	      	   	  pair of top dimensional Cones intersecting in a common codim 1 face. 
--  OUTPUT : The ToricVectorBundle 'tvb' 
-- COMMENT : Note that the ToricVectorBundle already has a list of pairs (i,j) denoting the codim 1 intersections 
--     	     of two top dim cones, with i<j and they are ordered in lexicographic order. So the matrices in 'L'
--     	     will be assigned to the pairs (i,j) in that order, where the matrix A assigned to (i,j) denotes the 
--     	     transition
--     	    	 (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)* A
addBaseChange = method(TypicalValue => ToricVectorBundleKaneyama)
addBaseChange (ToricVectorBundleKaneyama,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     pairlist := sort keys tvb#"baseChangeTable";
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if #pairlist != #L then error("Expected the number of matrices to match the number of codim 1 Cones.");
     baseChangeTable := hashTable apply(#pairlist, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if not instance(M,Matrix) then error("Expected the transition matrices to be given as rank times rank matrices.");
	       if numColumns M != k or numRows M != k then error("Expected the base change matrices to be k by k matrices.");
	       if det M == 0 then error("The base change matrices must be invertible.");
	       R := ring source M;
	       M = if R === ZZ or R === QQ then promote(M,QQ) else error("Expected base change over ZZ or QQ");
	       -- Inserting the matrix at the i-th position
	       pairlist#i => M));
     -- Writing the new transition matrices into the bundle
     new ToricVectorBundleKaneyama from {
	  "degreeTable" => tvb#"degreeTable",
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => k,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable",
	  symbol cache => new CacheTable})


-- PURPOSE : Changing the degrees of the local generators of a given ToricVectorBundleKaneyama to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundleKaneyama 'tvb' and a list 'L'of n by k matrices over ZZ, one for each 
--     	    	      	 top dimensional Cone. 
--  OUTPUT : The ToricVectorBundleKaneyama 'tvb' 
-- COMMENT : Note that in the ToricVectorBundleKaneyama the top dimensional Cones are already numbered and that the degree
--     	     matrices will be assigned to the Cones in that order. 
addDegrees = method(TypicalValue => ToricVectorBundleKaneyama)
addDegrees (ToricVectorBundleKaneyama,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     tCT := sort keys tvb#"degreeTable";
     k := tvb#"rank of the vector bundle";
     n := tvb#"dimension of the variety";
     -- Checking for input errors
     if #tCT != #L then error("Number of degree matrices must match the number of top dim cones.");
     degreeTable := hashTable apply(#tCT, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if not instance(M,Matrix) then error("The degrees must be given as dimension times rank matrices.");
	       if ring M =!= ZZ then error("Expected the degrees to be in the ZZ lattice.");
	       if numColumns M != k then error("The number of degrees must match the vector bundle rank.");
	       if numRows M != n then error("The degrees must have the dimension of the underlying toric variety.");
	       -- Inserting the degree matrix
	       tCT#i => M));
     -- Writing the new degree table into the bundle
     new ToricVectorBundleKaneyama from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => tvb#"baseChangeTable",
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => k,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable",
	  symbol cache => new CacheTable})


--   INPUT : '(tvb,L)',  a ToricVectorBundleKlyachko 'tvb' and a list 'L'of 1 by k matrices over ZZ, one for each 
--     	    	      	   	  ray of the fan
--  OUTPUT : The ToricVectorBundleKlyachko 'tvb' 
-- COMMENT : Note that the  matrices in 'L' will be assigned to the rays in the order they appear in rays tvb
addFiltration = method(TypicalValue => ToricVectorBundleKlyachko)
addFiltration (ToricVectorBundleKlyachko,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     n := tvb#"number of rays";
     k := tvb#"rank of the vector bundle";
     R := rays tvb;
     -- Checking for input errors
     if n != #L then error("The number of matrices has to match the number of rays of the fan.");
     if any(L, l -> not instance(l,Matrix)) then error("The filtrations have to be given as matrices.");
     if any(L, l -> ring l =!= ZZ) then error("The filtrations have to be given as matrices over ZZ.");
     if any(L, l -> numColumns l != k or numRows l != 1) then error("The filtrations have to be given as 1 times n matrices.");
     -- Writing the new filtration matrices  into the table
     filtrationMatricesTable := hashTable apply(n, i -> R#i => L#i);
     -- Computing the list of changes in the filtrations
     filtrationTable := hashTable apply(pairs filtrationMatricesTable, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({(min L1 - 1) => {}} |  apply(L1, l -> l => positions(L,e -> e == l)))));
     -- Writing the new filtration maps and changes tables into the bundle
     new ToricVectorBundleKlyachko from {
	  "ring" => tvb#"ring",
	  "rayTable" => tvb#"rayTable",
	  "baseTable" => tvb#"baseTable",
	  "filtrationMatricesTable" => filtrationMatricesTable,
	  "filtrationTable" => filtrationTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "number of rays" => tvb#"number of rays",
	  symbol cache => new CacheTable})


-- PURPOSE : Giving the number of affine charts of a ToricVectorBundle
--   INPUT : 'tvb', a ToricVectorBundle
--  OUTPUT : 'ZZ',  the number of affine charts
charts = method(TypicalValue => ZZ)
charts ToricVectorBundle := tvb -> tvb#"number of affine charts"

	       
-- PURPOSE : Checking if the ToricVectorBundleKaneyama fulfills the cocycle condition
--   INPUT : 'tvb',  a ToricVectorBundleKaneyama 
--  OUTPUT : 'true' or 'false' 
cocycleCheck = method(TypicalValue => Boolean)
cocycleCheck ToricVectorBundleKaneyama := (cacheValue symbol cocycle)( tvb -> (
     	  -- Extracting data out of tvb
     	  n := tvb#"dimension of the variety";
     	  k := tvb#"rank of the vector bundle";
     	  bCT := tvb#"baseChangeTable";
     	  topCones := sort keys tvb#"topConeTable";
     	  L := hashTable {};
     	  -- For each codim 2 Cone computing the list of topCones which have this Cone as a face
     	  -- and save the list of indices of these topCones as an element in L
     	  for i from 0 to #topCones - 1  do L = merge(hashTable apply(faces(2,topCones#i), C -> C => {i}),L,(a,b) -> sort join(a,b));
     	  -- Finding the cyclic order of every list of topCones in L and write this cyclic order as a 
     	  -- list of consecutive pairs
     	  L = for l in values L list (
	       pairings := {};
	       start := l#0;
	       a := start;
	       l = drop(l,1);
	       i := position(l, e -> dim intersection(topCones#a,topCones#e) == n-1);
	       while i =!= null do (
		    pairings = pairings | {(a,l#i)};
		    a = l#i;
		    l = drop(l,{i,i});
		    i = position(l, e -> dim intersection(topCones#a,topCones#e) == n-1));
	       if dim intersection(topCones#a,topCones#start) == n-1 then pairings | {(a,start)} else continue);
     	  -- Check for every cyclic order of topCones if the product of the corresponding transition
     	  -- matrices is the identity
     	  all(L, l -> product apply(reverse l, e -> if e#0 > e#1 then inverse bCT#(e#1,e#0) else bCT#e) == map(QQ^k,QQ^k,1))))


-- PURPOSE : Presenting some details of the given ToricVectorBundle
--   INPUT : 'tvb',  a ToricVectorBundleKaneyama
--  OUTPUT : '(A,C)',	 where 'A' is a hashTable giving the enumeration of the maximal cones with their rays and degree matrix, 
--     	    	      	 and 'B' gives the transition matrices for the codim 1 pairs
-- COMMENT : This function gives the possibility to have a quick overview on the main properties of a ToricVectorBundleKaneyama
details = method()
details ToricVectorBundle := tvb -> (
     if instance(tvb,ToricVectorBundleKaneyama) then (hashTable apply(pairs(tvb#"topConeTable"), p -> ( p#1 => (rays p#0,tvb#"degreeTable"#(p#0)))),tvb#"baseChangeTable")
     else hashTable apply(rays tvb, r -> r => (tvb#"baseTable"#r,tvb#"filtrationMatricesTable"#r)))


-- PURPOSE : Checking if a ToricVectorBundleKaneyama satisfies the regularity conditions of the degrees
--   INPUT : 'tvb', a ToricVectorBundleKaneyama
--  OUTPUT : 'true' or 'false'
-- COMMENT : This function is for checking ToricVectorBundles whose degrees and matrices 
--     	     are inserted by hand. Those generated for example by tangentBundle fulfill the 
--     	     conditions automatically.
regCheck = method(TypicalValue => Boolean)
regCheck ToricVectorBundleKaneyama := (cacheValue symbol regCheck)( tvb -> (
     	  -- Extracting the neccesary data
     	  tCT := sort keys tvb#"topConeTable";
     	  c1T := tvb#"codim1Table";
     	  bCT := tvb#"baseChangeTable";
     	  dT := tvb#"degreeTable";
     	  k := tvb#"rank of the vector bundle";
     	  all(keys bCT, p -> (
	       	    -- Taking a pair corresponding to a codim 1 cone, the corresponding transition matrix and its inverse
	       	    A := bCT#p;
	       	    B := inverse A;
	       	    -- Computing the dual of the codim 1 cone
	       	    C := dualCone c1T#p;
	       	    -- Check for all pairs of degree vectors of the two top Cones the reg condition
	       	    all(k, i -> (
			      ri := (dT#(tCT#(p#1)))_{i};
			      all(k, j -> (
				   	rj := (dT#(tCT#(p#0)))_{j};
				   	(if A^{i}_{j} != 0 then contains(C,rj-ri) else true) and (if A^{j}_{i} != 0 then contains(C,ri-rj) else true)))))))))



----------------------------------------------------------------------------
-- OPERATIONS ON TORIC VECTOR BUNDLES
----------------------------------------------------------------------------


-- PURPOSE : Returning the base representation of the bundle
--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : A HashTable which gives for each ray of the fan the matrix of the basis
base = method(TypicalValue => HashTable)
base ToricVectorBundleKlyachko := tvb -> tvb#"baseTable"


-- PURPOSE : Returning the filtration matrices of the bundle
--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : A HashTable which gives for each ray of the matrix of the filtration
filtration = method(TypicalValue => HashTable)
filtration ToricVectorBundleKlyachko := tvb -> tvb#"filtrationMatricesTable"


-- PURPOSE : Checking for the descriptions of two given vector bundles in Klyachko's description if they are isomorphic
--   INPUT : '(T1,T2)',  two ToricVectorBundleKlyachko
--  OUTPUT : 'true', if they are isomorphic, 'false' otherwise
-- COMMENT : If the check reveals that they are isomorphic, the isomorphism can be obtained with the function isomorphism
areIsomorphic = method(TypicalValue => Boolean)
areIsomorphic (ToricVectorBundleKlyachko,ToricVectorBundleKlyachko) := (T1,T2) -> (
     -- Creating the entries in the cacheTables of the two bundles if they are not yet present
     if not T1.cache.?isomorphic then (
	  T1.cache.isomorphic = new MutableHashTable;
	  if not T1.cache.?isoMatrix then T1.cache.isoMatrix = new MutableHashTable);
     if not T2.cache.?isomorphic then (
	  T2.cache.isomorphic = new MutableHashTable;
	  if not T2.cache.?isoMatrix then T2.cache.isoMatrix = new MutableHashTable);
     -- If this pairing has not been checked before, check it now
     if not T1.cache.isomorphic#?T2 then (
	  local isoMatrix;
	  T1.cache.isomorphic#T2 = (
	       -- To be isomorphic, the bundles must be over the same TV, over the same ring and must have the same rank
	       T1#"ToricVariety" == T2#"ToricVariety" and T1#"ring" === T2#"ring" and T1#"rank of the vector bundle" == T2#"rank of the vector bundle" and (
		    -- If this is the case, extract the filtrations
		    fMT1 := T1#"filtrationMatricesTable";
		    fMT2 := T2#"filtrationMatricesTable";
		    bT1 := T1#"baseTable";
		    bT2 := T2#"baseTable";
		    bundleRing := T1#"ring";
		    R := rays T1;
		    r0 := R#0;
		    R = drop(R,1);
		    -- Check for the first ray, if they have the same filtration numbers and dimensions of the filtration steps
		    sort fMT1#r0 == sort fMT2#r0 and (
			 -- if this is the case, resort both base matrices according to the filtration and compute the possible isomorphism
			 A := submatrix'(sort(promote(fMT1#r0,bundleRing) || bT1#r0),{0},);
			 B := submatrix'(sort(promote(fMT2#r0,bundleRing) || bT2#r0),{0},);
			 isoMatrix = B*(A^-1);
			 -- check for the remaining rays if the filtrations are identical
			 all(R, r -> (
				   f1 := flatten entries fMT1#r;
				   f2 := flatten entries fMT2#r;
				   sort f1 == sort f2 and all(unique f1, e -> (
					     E1 := (bT1#r)_(positions(f1, i -> i <= e));
					     E2 := (bT2#r)_(positions(f2, i -> i <= e));
					     image(isoMatrix*E1) == image E2)))))));
	  -- If they are isomorphic then write the isomorphism into the cache of both bundles
	  if T1.cache.isomorphic#T2 then (
	       T1.cache.isoMatrix#T2 = isoMatrix;
	       T2.cache.isomorphic#T1 = true;
	       T2.cache.isoMatrix#T1 = isoMatrix^-1));
     T1.cache.isomorphic#T2)


-- PURPOSE : Obtaining the isomorphism if two vector bundles are isomorphic
--   INPUT : '(T1,T2)',  two ToricVectorBundleKlyachko
--  OUTPUT : The isomorphism, if they are isomorphic, otherwise an error
isomorphism = method(TypicalValue => Matrix)
isomorphism (ToricVectorBundleKlyachko,ToricVectorBundleKlyachko) := (T1,T2) -> (
     if not areIsomorphic(T1,T2) then error("The bundles are not isomorphic");
     T1.cache.isoMatrix#T2)				


-- PURPOSE : Compute the Euler characteristic
eulerChi = method(TypicalValue => ZZ)

--   INPUT : '(T,u)',  where 'T' is a ToricVectorBundle and 'u' is a one column matrix over ZZ giving a degree vector
--  OUTPUT : The Euler characteristic of the Cech complex at degree 'u'
eulerChi (Matrix,ToricVectorBundle) := (u,T) -> (
     if not T.cache.?eulerChi then T.cache.eulerChi = new MutableHashTable;
     if not T.cache.eulerChi#?u then (
	  n := T#"dimension of the variety";
	  -- Compute the Cech complex and compute the alternating sum of the dimensions
	  if instance(T,ToricVectorBundleKlyachko) then T.cache.eulerChi#u = sum apply(n+1, i -> (-1)^i * sum values (cechComplex(i,T,u))#1)
	  else T.cache.eulerChi#u = sum apply(n+2, i -> (-1)^i * numColumns (cechComplex(i,T,u))#1));
     T.cache.eulerChi#u)

--   INPUT : 'T',  a ToricVectorBundle
--  OUTPUT : The Euler characteristic of the bundle
eulerChi ToricVectorBundle := T -> (
     -- Compute the set of degrees with possible cohomology
     L := latticePoints deltaE T;
     -- Sum up their characteristics
     sum apply(L, l -> eulerChi(l,T)))


-- PURPOSE : Returning the table of codimension 1 cones of the underlying fan
--   INPUT : 'T',  a ToricVectorBundleKaneyama
--  OUTPUT : a HashTable
codim1Table = method(TypicalValue => HashTable)
codim1Table ToricVectorBundleKaneyama := T -> T#"codim1Table"     


-- PURPOSE : Computing the cohomology group of a given ToricVectorBundle
--   INPUT : '(i,T,weight)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundle, and 'weight' the degree
--  OUTPUT : 'ZZ',	     the graded module of the degree 'weight' part of the 'i'th cohomology group of 'T'
cohomology(ZZ,ToricVectorBundle,Matrix) := opts -> (i,T,weight) -> cohom(i,T,weight)


-- PURPOSE : Computing the cohomology group of a given ToricVectorBundle
--   INPUT : '(i,T,P)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundle, and 'P' a list of degrees
--  OUTPUT : 'List',	     the list of the graded modules of the corresponding degree parts of the cohomology group which are non zero
cohomology(ZZ,ToricVectorBundle,List) := opts -> (i,T,P)-> (
     if opts.Degree == 1 then print ("Number of degrees to calculate: "|(toString(#P)));
     for j in P list (
	  if opts.Degree == 1 then << "." << flush;
	  j = cohomology(i,T,j);
	  if j != 0 then j else continue))

   
-- PURPOSE : Computing the cohomology group of a given ToricVectorBundle
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundle
--  OUTPUT : the group as a graded module where the generators have the corresponding degree of the weight vector
-- COMMENT : if the option "Degree" => 1 is given then it displays the number of degrees to calculate
cohomology(ZZ,ToricVectorBundle) := opts -> (i,T)-> (
     L := cohomology(i,T,latticePoints deltaE T,Degree => opts.Degree);
     if L == {} then (ring T)^0 else directSum L)


-- PURPOSE : Computing the rank of the cohomology group of a given ToricVectorBundle
--   INPUT : '(i,S)',  'i' for the 'i'th cohomology group, 'S' a Sequence of ToricVectorBundle and a weight vector
--  OUTPUT : 'ZZ',	     the rank of the degree 'weight' part of the 'i'th cohomology group of the bundle
hh(ZZ,Sequence) := (i,S) -> (
     -- Checking for input errors
     if #S != 2 then error("The Sequence has to contain a toric vector bundle and a weight vector.");
     if not instance(S#1,Matrix) then error("The second argument has to be a weight vector given by a matrix.");
     if not instance(S#0,ToricVectorBundleKaneyama) and not instance(S#0,ToricVectorBundleKlyachko) then error("The first argument has to be a toric vector bundle.");
     (T,u) := S;
     rank cohomology(i,T,u))


-- PURPOSE : Computing the rank of the cohomology group of a given ToricVectorBundle
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundle
--  OUTPUT : 'ZZ',  the rank of the 'i'th cohomology group
hh(ZZ,ToricVectorBundle) := ZZ => (i,T) -> rank cohomology(i,T)


-- PURPOSE : Computing the coker bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as target
--  OUTPUT : The bundle given by the cokernels of the filtrations
coker (ToricVectorBundleKlyachko,Matrix) := (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numRows M then error("The source of the matrix has to be the vector bundle.");
     if tRing =!= ring M then error("Matrix and bundle have to be over the same ring."); 
     -- Computing the map from the bundle to the kernel
     N := transpose mingens ker transpose M;
     -- Computing a basis of the cokernel
     coKerGens := mingens image N;
     newRank := numColumns coKerGens;
     bT := T#"baseTable";
     fT := T#"filtrationTable";
     -- Computing the new baseTable with filtrations
     bT = hashTable apply(keys bT, j -> (
	       fTj := drop(sort keys fT#j,1);
	       cols := {};
	       oldCoKer := map(tRing^newRank,tRing^0,0);
	       -- Going through the filtration steps and computing the cokernel for each step
	       j => apply(fTj, i -> (
			 cols = cols | fT#j#i;
			 -- Computing the cokernel
			 A := N * (bT#j)_cols;
			 -- Representing this in the basis chosen
			 gkMA := (gens ker (coKerGens | A))^{0..newRank-1};
			 -- Selecting the new basis elements that appear in this filtration step
			 gkMA = mingens (image(oldCoKer | gkMA) / image oldCoKer);
			 -- Appending these new vectors
			 oldCoKer = oldCoKer |gkMA;
			 -- appending the filtration step number
			 (gkMA,matrix {toList(numColumns gkMA:i)})))));
     -- Generating the new filtration matrices and tables
     fMT := hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,last)});
     fT = hashTable apply(pairs fMT, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({min L1 - 1 => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
     bT = hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,first)});
     Tnew := new ToricVectorBundleKlyachko from {
	  "ring" => T#"ring",
	  "rayTable" => T#"rayTable",
	  "baseTable" => bT,
	  "filtrationMatricesTable" => fMT,
	  "filtrationTable" => fT,
	  "ToricVariety" => T#"ToricVariety",
	  "number of affine charts" => T#"number of affine charts",
	  "dimension of the variety" => T#"dimension of the variety",
	  "rank of the vector bundle" => newRank,
	  "number of rays" => T#"number of rays",
	  symbol cache => new CacheTable};
     if T.cache.?isVB and T.cache.isVB then Tnew.cache.isVB = T.cache.isVB;
     Tnew)     	       

	       
-- PURPOSE : Computing the cotangent bundle on a smooth, pure, and full dimensional Toric Variety 
cotangentBundle = method(Options => {"Type" => "Klyachko"})

--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle
cotangentBundle Fan := opts -> F -> (
     if opts#"Type" == "Klyachko" then dual tangentBundleKlyachko F else if opts#"Type" == "Kaneyama" then cotangentBundleKaneyama F else error("Expected Type to be Klyachko or Kaneyama."))

-- PURPOSE : Computing the polytope deltaE in the degree space such that outside this polytope
--     	     every cohomology is 0 
deltaE = method()

--   INPUT : 'tvb',  a ToricVectorBundle
--  OUTPUT : a Polyhedron
deltaE ToricVectorBundle := (cacheValue symbol deltaE)( T -> (
	  if not isComplete T#"ToricVariety" then error("The toric variety needs to be complete.");
     	  n := T#"dimension of the variety";
	  if instance(T,ToricVectorBundleKaneyama) then (
	       dT := values T#"degreeTable";
	       dT = matrix {dT};
	       convexHull dT)
	  else (
	       W := findWeights T;
	       W = apply(W,first);
	       W = matrix {W};
	       convexHull W)))

--oldDeltaE = method()
--oldDeltaE ToricVectorBundle := (cacheValue symbol oldDeltaE)( tvb -> (
--     	  if not isComplete tvb#"ToricVariety" then error("The toric variety needs to be complete.");
--     	  n := tvb#"dimension of the variety";
--	  if instance(tvb,ToricVectorBundleKaneyama) then (
--	       -- Extracting neccesary data
--     	       raylist := rays tvb;
--     	       rl := #raylist;
--     	       k := tvb#"rank of the vector bundle";
--     	       tCT := sort keys tvb#"topConeTable";
--     	       dT := tvb#"degreeTable";
--     	       -- Creating an index table, for each ray the first top cone containing it
--     	       raytCTindex := hashTable apply(#raylist, r -> r => position(tCT, C -> contains(C,raylist#r)));
--     	       raylist = transpose matrix {raylist};
--     	       -- Get the subsets of 'n' elements in 'rl'
--     	       sset := subsets(rl,n);
--     	       jList := {{}};
--     	       -- Get all different combinations of choices of variety dimension many degree vectors
--     	       for i from 0 to n-1 do jList = flatten apply(jList, l -> apply(k, j -> l|{j}));
--     	       M := map(QQ^1,QQ^n,0);
--     	       v := map(QQ^1,QQ^1,0);
--     	       -- For every 'n' in 'l' subset and any combination in jList get the intersection of the dual cones
--     	       -- of the corresponding rays. If this is a non-empty compact polytope then add the vertices to the
--     	       -- list L
--     	       L := unique flatten apply(sset, s -> (
--	       	    	 unique for j in jList list (
--		    	      N := matrix apply(n, i -> {raylist^{s#i},raylist^{s#i} * ((dT#(tCT#(raytCTindex#(s#i))))_{j#i})});
--		    	      w := N_{n};
--		    	      N = submatrix'(N,{n});
--		    	      P := intersection(M,v,N,w);
--		    	      if isCompact P and (not isEmpty P) then vertices P else continue)));
--     	       -- Make a matrix of all the vertices in L
--     	       M = matrix {L};
--     	       convexHull M)
--	  else (
--	       -- Extracting neccesary data
--	       rayTable := tvb#"rayTable";
--	       l := #rayTable;
--	       fMT := hashTable apply(pairs tvb#"filtrationMatricesTable", (i,j) -> (j = flatten entries j; i => matrix{{-(min j),max j}}));
--	  		      sset1 := select(subsets(rays tvb,n), s -> rank matrix {s} == n);
--	  		      convexHull matrix {apply(sset1, s -> (
--			 		     M := transpose matrix {apply(s, r -> (-r | r) || (fMT#r))};
--			 		     vertices intersection(M_{0..n-1},M_{n})))})))


--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the direct sum
ToricVectorBundle.directSum = args -> (
     args = toList args;
     T := args#0;
     scan(drop(args,1), E -> T = T ++ E);
     T)

      
ToricVectorBundle ++ ToricVectorBundle := (tvb1,tvb2) -> (
	  -- Checking for input errors
	  if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("Expected the bundles to be over the same toric variety.");
	  -- Extracting data out of tvb1 and tvb2
	  k1 := tvb1#"rank of the vector bundle";
	  k2 := tvb2#"rank of the vector bundle";
	  if instance(tvb1,ToricVectorBundleKaneyama) and instance(tvb2,ToricVectorBundleKaneyama) then (
	       -- Generating the trivial bundle of dimension k1+k2
	       E := makeVBKaneyama(k1 + k2,tvb1#"ToricVariety");
	       -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
	       E = new ToricVectorBundleKaneyama from {
	       	    "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> a|b),
	       	    "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> a++b),
	       	    "ToricVariety" => E#"ToricVariety",
	       	    "number of affine charts" => E#"number of affine charts",
	       	    "dimension of the variety" => E#"dimension of the variety",
	       	    "rank of the vector bundle" => k1 + k2,
	       	    "codim1Table" => E#"codim1Table",
	       	    "topConeTable" => E#"topConeTable",
	       	    symbol cache => new CacheTable};
	       if (tvb1.cache.?regCheck and tvb2.cache.?regCheck and tvb1.cache.regCheck and tvb2.cache.regCheck and (
		    	 tvb1.cache.?cocycle and tvb2.cache.?cocycle and tvb1.cache.cocycle and tvb2.cache.cocycle)) then (
	       	    E.cache.regCheck = true;
	       	    E.cache.cocycle = true);
	       E)
	  else if instance(tvb1,ToricVectorBundleKlyachko) and instance(tvb2,ToricVectorBundleKlyachko) then (
	       k := k1 + k2;
     	       F := tvb1#"ToricVariety";
     	       R := tvb1#"ring";
     	       tvb := makeVBKlyachko(k,F);
     	       fT1 := tvb1#"filtrationMatricesTable";
     	       fT2 := tvb2#"filtrationMatricesTable";
     	       bT1 := tvb1#"baseTable";
     	       bT2 := tvb2#"baseTable";
     	       filtrationTable := apply(rays tvb, r -> fT1#r | fT2#r);
     	       baseTable := apply(rays tvb, r -> bT1#r ++ bT2#r);
     	       tvb = addFiltration(tvb,filtrationTable);
     	       tvb = addBase(tvb,baseTable);
     	       if tvb1.cache.?isVB and tvb2.cache.?isVB and tvb1.cache.isVB and tvb2.cache.isVB then tvb.cache.isVB = true;
     	       tvb)
	  else error("The two bundles have to be in the same description."))


-- ToricVectorBundleKlyachko ++ ToricVectorBundleKlyachko := (tvb1,tvb2) -> (
--     -- Extracting data out of tvb1 and tvb2
--     k1 := (tvb1#"rank of the vector bundle");
--     k2 := (tvb2#"rank of the vector bundle");
--     k := k1 + k2;
--     F := tvb1#"ToricVariety";
--     R := tvb1#"ring";
--     tvb := makeVBKlyachko(k,F);
--     fT1 := tvb1#"filtrationMatricesTable";
--     fT2 := tvb2#"filtrationMatricesTable";
--     bT1 := tvb1#"baseTable";
--     bT2 := tvb2#"baseTable";
--     filtrationTable := apply(rays tvb, r -> fT1#r | fT2#r);
--     baseTable := apply(rays tvb, r -> bT1#r ++ bT2#r);
--     tvb = addFiltration(tvb,filtrationTable);
--     tvb = addBase(tvb,baseTable);
--     if tvb1.cache.?isVB and tvb2.cache.?isVB and tvb1.cache.isVB and tvb2.cache.isVB then tvb.cache.isVB = true;
--     tvb)


-- PURPOSE : Computing the dual bundle to a given ToricVectorBundle
--   INPUT : 'tvb',  a ToricVectorBundle
--  OUTPUT : the dual ToricVectorBundle
dual ToricVectorBundle := {} >> opts -> tvb -> (
     if instance(tvb,ToricVectorBundleKaneyama) then (
     	  -- Inverting the degrees and the transition matrices
     	  degreeTable := hashTable apply(pairs tvb#"degreeTable", p -> p#0 => -(p#1));
     	  baseChangeTable := hashTable apply(pairs tvb#"baseChangeTable", p -> p#0 => transpose inverse p#1);
     	  -- Writing the inverted tables into the bundle
     	  E := new ToricVectorBundleKaneyama from {
	       "degreeTable" => degreeTable,
	       "baseChangeTable" => baseChangeTable,
	       "ToricVariety" => tvb#"ToricVariety",
	       "number of affine charts" => tvb#"number of affine charts",
	       "dimension of the variety" => tvb#"dimension of the variety",
	       "rank of the vector bundle" => tvb#"rank of the vector bundle",
	       "codim1Table" => tvb#"codim1Table",
	       "topConeTable" => tvb#"topConeTable",
	       symbol cache => new CacheTable};
     	  if tvb.cache.?regCheck and tvb.cache.regCheck and tvb.cache.?cocycle and tvb.cache.cocycle then (
	       E.cache.regCheck = true;
	       E.cache.cocycle = true);
     	  E)
     else (
	  -- Inverting the filtration. If the filtration has d steps then the new n-th boundary is -(d-n+1th boundary)-1 and the n-th step is the 
     	  -- d-n+2 th step
     	  fT := hashTable apply(pairs tvb#"filtrationTable", (r,e) -> r => (k:=sort keys e;e = apply(#k-1, i -> (k#i,e#(k#(i+1))))|{(last k,{})}; hashTable apply(e, entry -> -(entry#0)-1 => entry#1)));
     	  fMT := hashTable apply(pairs fT, q -> q#0 => (q1new:= hashTable flatten apply(pairs q#1, p -> apply(p#1, i -> i => p#0)); matrix {apply(#q1new, j -> q1new#j)}));
     	  -- The orthogonal complement is given by the transpose of the inverse matrix
     	  bT := hashTable apply(pairs tvb#"baseTable", p -> p#0 => transpose inverse p#1);
     	  T := new ToricVectorBundleKlyachko from {
	       "ring" => tvb#"ring",
	       "rayTable" => tvb#"rayTable",
	       "baseTable" => bT,
	       "filtrationMatricesTable" => fMT,
	       "filtrationTable" => fT,
	       "ToricVariety" => tvb#"ToricVariety",
	       "number of affine charts" => tvb#"number of affine charts",
	       "dimension of the variety" => tvb#"dimension of the variety",
	       "rank of the vector bundle" => tvb#"rank of the vector bundle",
	       "number of rays" => tvb#"number of rays",
	       symbol cache => new CacheTable};
     	  if tvb.cache.?isVB and tvb.cache.isVB then T.cache.isVB = true;
     	  T))
	  
	  
-- PURPOSE : Checking if a given List of possible degree vectors admits a Decomposition in torus eigenspaces that give the filtration
--   INPUT : '(T,L)',  where 'T' is a ToricVectorBundleKlyachko and 'L' is a List where the i-th entry is either a matrix or a List of 
--     	    	       matrices of possible degree vectors for the i-th cone in maxCones
--  OUTPUT : 'true' if a selection of degrees for each maximal cone admits a decomposition, 'false' otherwise
existsDecomposition = method()
existsDecomposition (ToricVectorBundleKlyachko,List) := (T,L) -> (
     -- Checking if the list contains only matrices and lists and converting the former into a list with this matrix
     L = apply(L, l -> if instance(l,List) then l else if instance(l,Matrix) then {l} else error("The elements of the list have to be either matrices or lists of them."));
     if not T.cache.?degreesList then T.cache.degreesList = {};
     mC := maxCones T;
     mC = apply(mC, C -> (C = C#"rays"; apply(numColumns C, i -> C_{i})));
     -- Checking for input errors
     if #mC != #L then error("There has to be a degree matrix or list of degree matrices for each maximal cone of the fan.");
     -- Check if any combination of matrices in L has already been checked and thus saved in the cache
     if any(T.cache.degreesList, dl -> all(toList(0..#dl-1), i -> (set L#i)#?(dl#i))) then true 
     -- otherwise for each maximal cone check the decomposition criterion
     else (
	  -- Add to each Cone the list of possible degrees
     	  mC = apply(#mC, i -> (mC#i,L#i));
     	  allRaysTable := tableForAllRays T;
     	  n := T#"dimension of the variety";
     	  k := rank T;
     	  R := T#"ring";
	  -- Recursive function that runs through all possible combinations of filtration steps for the rays of a cone
     	  recursiveCheck := (fList,Es,D) -> (
	       -- if there is still a list of filtration steps, call recursiveCheck again for each entry
	       if fList != {} then (
	       	    Lr := fList#0#1;
	       	    r := fList#0#0;
	       	    all(Lr, l -> recursiveCheck(drop(fList,1),intersectMatrices(Es,l#1),select(D, d -> (d * r)_(0,0) <= l#0))))
	       -- otherwise we have a choice of filtration steps and check the condition
	       else numColumns Es == #D);
	  -- The check for the criterion begins with the complete bundle
     	  E := map(R^k,R^k,1);
	  -- For each cone check if there is one of the degree matrices that admits a decomposition
     	  L = for C in mC list (
	       fList := apply(C#0, r -> (r,allRaysTable#r));
	       d := select(1,C#1, D -> (
		    	 D = promote(D,QQ);
		    	 D = apply(numColumns D, i -> transpose D_{i});
		    	 recursiveCheck(fList,E,D)));
	       -- If there is one that admits a decomposition return that, otherwise return the empty set for L
	       if d == {} then break {} else d#0);
	  -- If there is a combination then save it to the cache
     	  if L != {} then (
	       if not T.cache.?isVB then T.cache.isVB = true;
	       T.cache.degreesList = T.cache.degreesList|{L});
	  L != {}))


-- PURPOSE : Computing the 'l'-th exterior power of a ToricVectorBundle
--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundle
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the 'l'-th exterior power
exteriorPower (ZZ,ToricVectorBundle) := ToricVectorBundle => opts -> (l,tvb) -> (
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if l < 0 then error("The power has to be positive.");
     -- Generating the list of 'l'-tuples of 0..k-1 and the corresponding index table
     ind := subsets(k,l);
     indtable := hashTable apply(#ind, i -> ind#i => i);
     if instance(tvb,ToricVectorBundleKlyachko) then (
     	  if l == 0 then toricVectorBundle(1,tvb#"ToricVariety")
	  else if l > k then toricVectorBundle(0,tvb#"ToricVariety")
	  else (
	       -- Extracting data
     	       baseTable := tvb#"baseTable";
     	       filtrationTable := tvb#"filtrationMatricesTable";
     	       Rs := rays tvb;
     	       R := tvb#"ring";
     	       F := tvb#"ToricVariety";
     	       -- Computing the 'l'-th exterior powers of the base matrices
     	       baseTable = apply(Rs, r -> (
	       	    	 B := baseTable#r;
	       	    	 M := mutableMatrix(R,#ind,#ind);
	       	    	 for j in ind do for k in ind do M_(indtable#k,indtable#j) = det(B^k_j);
	       	    	 matrix M));
     	       -- Computing the 'l'-th exterior power of the filtration matrices
     	       filtrationTable = apply(Rs, r -> (
	       	    	 filt := filtrationTable#r;
	       	    	 matrix {apply(ind, j -> ( sum flatten entries filt_j))}));
     	       T := makeVBKlyachko(#ind,F,baseTable,filtrationTable);
     	       if tvb.cache.?isVB and tvb.cache.isVB then T.cache.isVB = true;
     	       T))
     else (
	  if l == 0 then toricVectorBundle(1,tvb#"ToricVariety","Type" => "Kaneyama")
	  else if l > k then toricVectorBundle(0,tvb#"ToricVariety","Type" => "Kaneyama")
	  else (
	       -- Computing the 'l'-th exterior powers of the transition matrices
     	       baseChangeTable := hashTable apply(pairs tvb#"baseChangeTable", p -> p#0 =>  matrix apply(ind, j -> apply(ind, k -> det (p#1)^j_k)));
     	       -- Computing the 'l'-th exterior power of the degrees
     	       degreeTable := hashTable apply(pairs tvb#"degreeTable", p -> p#0 => matrix {apply(ind, j -> (p#1)_j * matrix toList(l:{1}))});
     	       E := new ToricVectorBundleKaneyama from {
	       	    "degreeTable" => degreeTable,
	       	    "baseChangeTable" => baseChangeTable,
	       	    "ToricVariety" => tvb#"ToricVariety",
	       	    "number of affine charts" => tvb#"number of affine charts",
	       	    "dimension of the variety" => tvb#"dimension of the variety",
	       	    "rank of the vector bundle" => #ind,
	       	    "codim1Table" => tvb#"codim1Table",
	       	    "topConeTable" => tvb#"topConeTable",
	       	    symbol cache => new CacheTable};
     	       if tvb.cache.?regCheck and tvb.cache.regCheck and tvb.cache.?cocycle and tvb.cache.cocycle then (
	       	    E.cache.regCheck = true;
	       	    E.cache.cocycle = true);
     	       E)))


-- PURPOSE : Returning the underlying fan of a toric vector bundle
--   INPUT : 'T',  a ToricVectorBundleKaneyama
--  OUTPUT : a Fan
fan ToricVectorBundle := T -> T#"ToricVariety"


-- PURPOSE : Finding all possible sets of weight vectors for each maximal cone in the fan that admit the 
--           filtration steps on the rays
--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : a List,  where the i-th entry is the list of possible weight matrices for the i-th cone in maxCones T
findWeights = method(TypicalValue => List)
findWeights ToricVectorBundleKlyachko := (cacheValue symbol weights)( T -> (
     	  -- Get the maximal cones and save their rays
	  mC := maxCones T;
     	  mC = apply(mC, C -> (C = C#"rays"; apply(numColumns C, i -> C_{i})));
     	  n := T#"dimension of the variety";
     	  k := rank T;
	  -- Recursive function that goes through the rays and checks for the current ray which filtration steps are possible and for 
	  -- these calls itself again
	  -- E is the intersection of filtrations of the rays considered so far, L is the list of remaining rays with filtration steps not choosen so far, 
	  -- R is the list of filtration steps not choosen before for rays already handled, these are the possible steps for the next column and newColumn 
	  -- is the already created part of the new column
     	  recursiveColumnsConstructer := (E,L,R,newColumn) -> (
	       if L != {} then (
	       	    l := L#0;
	       	    L = drop(L,1);
	       	    flatten for e in unique l list (
		    	 -- Check if e admits an intersection of the filtrations
			 if ker(E|e#1) != 0 then (
			      -- if so call the function again for the next ray
			      i := position(l, le -> le == e);
			      recursiveColumnsConstructer(intersectMatrices(E,e#1),L,R|{drop(l,{i,i})},newColumn|{e#0}))
		    	 else continue))
	       else {(R,newColumn)});
	  -- Recursive function that generates the columns (filtration combinations for a weight vector) by calling the columns constructor and then, if
	  -- this has created columns, call it self again with the list of remaining filtration steps
     	  recursiveMatricesConstructer := (Elist,L,M) -> (
	       Lnew := recursiveColumnsConstructer(Elist#0#1,L,{},{Elist#0#0});
	       if #L#0 != 1 then flatten apply(Lnew, (f,s) -> recursiveMatricesConstructer(drop(Elist,1),f,M|{s}))
	       else apply(Lnew, (f,s) -> M|{s}));
     	  fMT := T#"filtrationMatricesTable";
     	  bT := T#"baseTable";
     	  bundleRing := T#"ring";
	  allRaysTable := tableForAllRays T;
     	  apply(mC, C -> (
		    -- For each maximal cone compute the possible weightvector matrices
	       	    L := apply(C, r -> allRaysTable#r);
	       	    E := L#0;
		    -- Compute the possible combinations of filtration steps
	       	    Flist := recursiveMatricesConstructer(E,drop(L,1),{});
	       	    Flist = apply(Flist, m -> promote(transpose matrix m,QQ));
	       	    R := promote(transpose matrix {C},QQ);
		    Rrank := rank R;
		    -- Check if this combination admits a weightvector matrix
		    if Rrank != n then (
			 M := R^{0..Rrank-1};
			 for F in Flist list (
			      D := systemSolver(M,F^{0..Rrank-1});
			      if liftable(D,ZZ) and R*D == F then lift(D,ZZ)
			      else continue))
		    else (
			 Rn := inverse R^{0..n-1};
			 for F in Flist list (
			      Dn := Rn * (F^{0..Rrank-1});
			      if liftable(Dn,ZZ) and R*Dn == F then lift(Dn,ZZ)
			      else continue))))))


-- PURPOSE : Generating the graded Ring for the cohomology groups
--   INPUT : 'T',  a ToricVectorBundle
--  OUTPUT : the ring of the bundle with degree space the lattice of the variety
ring ToricVectorBundle := (cacheValue symbol gradedRing)( T -> (
	  if instance(T,ToricVectorBundleKlyachko) then (T#"ring")[DegreeRank => T#"dimension of the variety"]
	  else QQ[DegreeRank => T#"dimension of the variety"]))


-- PURPOSE : Computing the image bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as its source
--  OUTPUT : The bundle given by the images of the filtrations
image (ToricVectorBundleKlyachko,Matrix) := (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numColumns M then error("The source of the matrix has to be the vector bundle.");
     if tRing =!= ring M then error("The matrix and the bundle have to be over the same ring."); 
     -- Compute a basis of the image
     Mgens := mingens image M;
     ranknew := numColumns Mgens;
     bT := T#"baseTable";
     fT := T#"filtrationTable";
     -- for each ray compute the image of the filtration
     bT = hashTable apply(keys bT, j -> (
	       fTj := drop(sort keys fT#j,1);
	       cols := {};
	       oldImage := map(tRing^ranknew,tRing^0,0);
	       -- for each filtration step compute the image
	       j => apply(fTj, i -> (
			 cols = cols | fT#j#i;
			 -- take the image of the i-th filtration
			 A := M * (bT#j)_cols;
			 -- Represent this in the basis chosen
			 gkMA := (gens ker (Mgens | A))^{0..ranknew-1};
			 -- Select the new basis vectors of the filtration
			 gkMA = mingens (image(oldImage | gkMA) / image oldImage);
			 -- and add them to the matrix
			 oldImage = oldImage |gkMA;
			 -- save the new matrix and filtration step
			 (gkMA,matrix {toList(numColumns gkMA:i)})))));
     -- Generate the new filtration matrices and tables
     fMT := hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,last)});
     fT = hashTable apply(pairs fMT, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({(min L1-1) => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
     bT = hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,first)});
     Tnew := new ToricVectorBundleKlyachko from {
	  "ring" => T#"ring",
	  "rayTable" => T#"rayTable",
	  "baseTable" => bT,
	  "filtrationMatricesTable" => fMT,
	  "filtrationTable" => fT,
	  "ToricVariety" => T#"ToricVariety",
	  "number of affine charts" => T#"number of affine charts",
	  "dimension of the variety" => T#"dimension of the variety",
	  "rank of the vector bundle" => ranknew,
	  "number of rays" => T#"number of rays",
	  symbol cache => new CacheTable};
     if T.cache.?isVB and T.cache.isVB then Tnew.cache.isVB = true;
     Tnew)


-- PURPOSE : Check for a ToricVectorBundleKlyachko if it is general
--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : 'true' or 'false'
-- COMMENT : A toricVectorBundle is general if for every generating cone 'C' the following holds:
--     	     For every choice of filtration steps i_1,...,i_n for the rays r_1,..,r_n of C 
--     	     codim \bigcap E^r_j(i_j) = min {\sum codim E^r_j(i_j),rank E}
--     	     holds.
isGeneral = method()
isGeneral ToricVectorBundleKlyachko := (cacheValue symbol isGeneral)( tvb -> (
	  fT := tvb#"filtrationMatricesTable";
     	  fT = hashTable apply(pairs fT, p -> p#0 => flatten entries p#1);
     	  bT := tvb#"baseTable";
     	  L := hashTable apply(pairs fT, (j,q) -> j => apply(sort unique q, i -> (bT#j)_(positions(fT#j, e -> e <= i))));     
     	  -- recursive function to check every combination of filtration steps
     	  recursiveCheck := (L,Es) -> (
	       -- if there is still a list of filtration steps, call recursiveCheck again for each entry
	       if L != {} then all(L#0, l -> recursiveCheck(drop(L,1),Es|{l}))
	       -- otherwise we have a choice of filtration steps and check the condition
	       else (
	       	    n := numRows Es#0;
	       	    codimSum := sum apply(Es, A -> n - numColumns A);
	       	    codimSum = min(codimSum,n);	       
	       	    R := ring Es#0;
	       	    E := map(R^n,R^n,1);
	       	    Es = select(Es, e -> numColumns e != n);
	       	    scan(Es, A -> E = intersectMatrices(E,A));
	       	    n - numColumns E == codimSum));
     	  F := maxCones tvb#"ToricVariety";
     	  all(F, C -> (
	       	    C = C#"rays";
	       	    C = apply(numColumns C, i -> C_{i});
	       	    recursiveCheck(apply(C, r -> L#r),{})))))


-- PURPOSE : Checking if the data in T in fact defines a vectorbundle, i.e., satisfies the decomposition condition or
--     	     regularity and cocycle condition
--   INPUT : 'T',  a ToricVectorBundle
--  OUTPUT : 'true' if if 'T' is fact a bundle, 'false' otherwise
isVectorBundle = method()
isVectorBundle ToricVectorBundle := (cacheValue symbol isVB)( T -> (
	  if instance(T,ToricVectorBundleKlyachko) then (
	       L := findWeights T;
	       all(L, l -> l != {}) and existsDecomposition(T,L))
	  else regCheck T and cocycleCheck T))


-- PURPOSE : Computing the kernel bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as source
--  OUTPUT : The bundle given by the kernels of the filtrations
ker (ToricVectorBundleKlyachko,Matrix) := opts -> (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numColumns M then error("The source of the matrix has to be the vector bundle.");
     if tRing =!= ring M then error("Matrix and bundle have to be over the same ring.");
     -- Compute a basis of the kernel
     M = mingens ker M;
     ranknew := numColumns M;
     bT := T#"baseTable";
     fT := T#"filtrationTable";
     -- Compute the new filtration for each ray
     bT = hashTable apply(keys bT, j -> (
	       fTj := drop(sort keys fT#j,1);
	       cols := {};
	       oldKer := map(tRing^ranknew,tRing^0,0);
	       -- compute each filtration step
	       j => apply(fTj, i -> (
			 cols = cols | fT#j#i;
			 A := (bT#j)_cols;
			 -- Represent the kernel intersected with the actual filtration step in the basis chosen
			 gkMA := (gens ker (M | A))^{0..ranknew-1};
			 -- Select the "new" vectors
			 gkMA = mingens (image(oldKer | gkMA) / image oldKer);
			 oldKer = oldKer |gkMA;
			 -- Save the new vectors and the filtration step
			 (gkMA,matrix {toList(numColumns gkMA:i)})))));
     -- Compute the filtration matrices and tables
     fMT := hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,last)});
     fT = hashTable apply(pairs fMT, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({min L1 - 1 => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
     bT = hashTable apply(pairs bT, p -> p#0 => matrix {apply(p#1,first)});
     Tnew := new ToricVectorBundleKlyachko from {
	  "ring" => T#"ring",
	  "rayTable" => T#"rayTable",
	  "baseTable" => bT,
	  "filtrationMatricesTable" => fMT,
	  "filtrationTable" => fT,
	  "ToricVariety" => T#"ToricVariety",
	  "number of affine charts" => T#"number of affine charts",
	  "dimension of the variety" => T#"dimension of the variety",
	  "rank of the vector bundle" => ranknew,
	  "number of rays" => T#"number of rays",
	  symbol cache => new CacheTable};
     if T.cache.?isVB and T.cache.isVB then Tnew.cache.isVB = true;
     Tnew)
     

-- PURPOSE : Returning the maximal cones of the underlying fan
--   INPUT : 'T',  a ToricVectorBundle
--  OUTPUT : a List of Cones
maxCones ToricVectorBundle := T -> sort maxCones T#"ToricVariety"


-- PURPOSE : Compute a random deformation of a ToricVectorBundleKlyachko
randomDeformation = method(TypicalValue => ToricVectorBundleKlyachko)

--   INPUT : '(tvb,l,h)',  where 'tvb' is a ToricVectorBundleKlyachko, 'l' and 'h' are integers
--  OUTPUT : a ToricVectorBundleKlyachko, a random deformation
-- COMMENT : Simply replaces the base matrices by random matrices of full rank with entries between 
--     	     'l' and 'h'
randomDeformation (ToricVectorBundleKlyachko,ZZ,ZZ) := (tvb,l,h) -> (
     -- Checking for input errors
     if l > h then error("Expected the first integer to be less or equal than the second integer.");
     k := tvb#"rank of the vector bundle";
     -- For each ray generate a new k by k matrix of full rank with entries between 'l' and  'h'
     bT := hashTable apply(pairs tvb#"baseTable", p -> (
	       A := 0 * p#1;
	       while det A == 0 do A = generateRandomMatrix(k,k,l,h);
	       p#0 => promote(A,tvb#"ring")));
     -- Keep the old filtration
     new ToricVectorBundleKlyachko from {
	  "ring" => tvb#"ring",
	  "rayTable" => tvb#"rayTable",
	  "baseTable" => bT,
	  "filtrationMatricesTable" => tvb#"filtrationMatricesTable",
	  "filtrationTable" => tvb#"filtrationTable",
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "number of rays" => tvb#"number of rays",
	  symbol cache => new CacheTable})

--   INPUT : '(tvb,h)',  where 'tvb' is a ToricVectorBundleKlyachko and 'h' an integer
--  OUTPUT : a ToricVectorBundleKlyachko, a random deformation
-- COMMENT : Simply replaces the base matrices by random matrices of full rank with entries between 
--     	     0 and 'h'
randomDeformation (ToricVectorBundleKlyachko,ZZ) := (tvb,h) -> randomDeformation(tvb,0,h)


-- PURPOSE : Returning the rank of the vector bundle
--   INPUT : 'T',  a ToricVectorBundle
rank ToricVectorBundle := T -> T#"rank of the vector bundle"


-- PURPOSE : Giving the rays of the underlying Fan of a toric vector bundle
--   INPUT : 'tvb',  a TorcVectorBundle
--  OUTPUT : 'L',  a List containing the rays of the Fan underlying the bundle
rays ToricVectorBundle := tvb -> raySort toList tvb#"ToricVariety"#"rays"


-- PURPOSE : Computing the 'l'-th symmetric power of a Toric Vector Bundle
--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb' is a ToricVectorBundle
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the 'l'-th symmetric power
symmetricPower(ZZ,ToricVectorBundle) := (l,tvb) -> (
     -- Checking for input errors
     if l < 0 then error("The power has to be strictly positive.");
     -- Extracting data
     k := tvb#"rank of the vector bundle";
     -- Generating the list of 'l'-tuples of 0..k-1 with duplicates and the corresponding index table
     ind := sort apply(subsets(k+l-1,l),s -> apply(#s, i -> s#i-i));
     allind := sort unique flatten apply(ind, permutations);
     indtable := hashTable apply(#ind, i -> ind#i => i);
     if instance(tvb,ToricVectorBundleKlyachko) then (
	  if l == 0 then toricVectorBundle(1,tvb#"ToricVariety")
	  else (
     	       baseTable := tvb#"baseTable";
     	       filtrationTable := tvb#"filtrationMatricesTable";
     	       Rs := rays tvb;
     	       R := tvb#"ring";
     	       F := tvb#"ToricVariety";
     	       -- Computing the 'l'-th symmetric product of the base matrices
     	       baseTable = apply(Rs, r -> (
	       	    	 B := baseTable#r;
	       	    	 M := mutableMatrix(R,#ind,#ind);
	       	    	 for i1 in ind do (
		    	      Bi := B_(i1);
		    	      for j in allind do M_(indtable#(sort j),indtable#i1) = M_(indtable#(sort j),indtable#i1) + product apply(#j, j1 -> Bi_(j#j1,j1)));
	       	    	 matrix M));
     	       -- Computing the 'l'-th symmetric products of the filtration matrices
     	       filtrationTable = apply(Rs, r -> (
	       	    	 filt := filtrationTable#r;
	       	    	 matrix {apply(ind, j -> sum flatten entries filt_j)}));
     	       T := makeVBKlyachko(#ind,F,baseTable,filtrationTable);
     	       if tvb.cache.?isVB and tvb.cache.isVB then T.cache.isVB=true;
     	       T))
     else (
	  if l == 0 then toricVectorBundle(1,tvb#"ToricVariety","Type" => "Kaneyama")
	  else (
	       -- Computing the 'l'-th symmetric powers of the transition matrices
     	       baseChangeTable := hashTable apply(pairs tvb#"baseChangeTable", p -> (
	       	    	 B := p#1;
	       	    	 M := mutableMatrix(QQ,#ind,#ind);
	       	    	 for i1 in ind do (
		    	      Bi := B_(i1);
		    	      for j in allind do M_(indtable#(sort j),indtable#i1) = M_(indtable#(sort j),indtable#i1) + product apply(#j, j1 -> Bi_(j#j1,j1)));
	       	    	 M = matrix M;
	       	    	 p#0 => M));
     	       -- Computing the 'l'-th symmetric powers of the degrees
     	       degreeTable := hashTable apply(pairs tvb#"degreeTable", p -> (
	       	    	 dM := p#1;
	       	    	 dM = transpose matrix apply(ind, j -> flatten entries(dM_j * matrix toList((#j):{1})));
	       	    	 p#0 => dM));
     	       E := new ToricVectorBundleKaneyama from {
	       	    "degreeTable" => degreeTable,
	       	    "baseChangeTable" => baseChangeTable,
	       	    "ToricVariety" => tvb#"ToricVariety",
	       	    "number of affine charts" => tvb#"number of affine charts",
	       	    "dimension of the variety" => tvb#"dimension of the variety",
	       	    "rank of the vector bundle" => #ind,
	       	    "codim1Table" => tvb#"codim1Table",
	       	    "topConeTable" => tvb#"topConeTable",
	       	    symbol cache => new CacheTable};
     	       if tvb.cache.?regCheck and tvb.cache.regCheck and tvb.cache.?cocycle and tvb.cache.cocycle then (
	       	    E.cache.regCheck = true;
	       	    E.cache.cocycle = true);
     	       E)))


-- PURPOSE : Computing the tangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
tangentBundle = method(Options => {"Type" => "Klyachko"})
tangentBundle Fan := opts -> F -> (
     if opts#"Type" == "Klyachko" then tangentBundleKlyachko F else if opts#"Type" == "Kaneyama" then dual cotangentBundleKaneyama F else error("Expected Type to be Klyachko or Kaneyama."))


-- PURPOSE : Checking if two toric vector bundles are equal
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle
--  OUTPUT : 'true' or 'false' 
ToricVectorBundle == ToricVectorBundle := (tvb1,tvb2) -> tvb1 === tvb2


-- PURPOSE : Computing the tensor product of two toric vector bundles over the same Fan
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle over the same Fan in the same description
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the tensor product in the same description
tensor(ToricVectorBundle, ToricVectorBundle) := ToricVectorBundle => {} >> opts -> (tvb1, tvb2) -> (
     -- Checking for input errors
     if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("Expected bundles over the same toric variety.");
     k1 := tvb1#"rank of the vector bundle";
     k2 := tvb2#"rank of the vector bundle";
     if instance(tvb1,ToricVectorBundleKaneyama) and instance(tvb2,ToricVectorBundleKaneyama) then (
     	  -- Extracting data out of tvb1 and tvb2
     	  -- Generating the trivial bundle of dimension k1+k2
     	  E := makeVBKaneyama(k1 * k2,tvb1#"ToricVariety");
     	  -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
     	  E = new ToricVectorBundleKaneyama from {
	       "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> matrix {flatten apply(k2, j -> apply(k1, i -> a_{i}+b_{j}))}),
	       "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> (
		    	 matrix flatten apply(k2, j -> apply(k1, i -> flatten apply(k2, j' -> apply(k1, i' -> a_(i,i') * b_(j,j'))))))),
	       "ToricVariety" => E#"ToricVariety",
	       "number of affine charts" => E#"number of affine charts",
	       "dimension of the variety" => E#"dimension of the variety",
	       "rank of the vector bundle" => k1 + k2,
	       "codim1Table" => E#"codim1Table",
	       "topConeTable" => E#"topConeTable",
	       symbol cache => new CacheTable};
     	  if (tvb1.cache.?regCheck and tvb2.cache.?regCheck and tvb1.cache.regCheck and tvb2.cache.regCheck and (
	       	    tvb1.cache.?cocycle and tvb2.cache.?cocycle and tvb1.cache.cocycle and tvb2.cache.cocycle)) then (
	       E.cache.regCheck = true;
	       E.cache.cocycle = true);
     	  E)
     else if instance(tvb1,ToricVectorBundleKlyachko) and instance(tvb2,ToricVectorBundleKlyachko) then (
	  -- Extracting data out of tvb1 and tvb2
     	  F := tvb1#"ToricVariety";
     	  bT1 := tvb1#"baseTable";
     	  bT2 := tvb2#"baseTable";
     	  fmT1 := tvb1#"filtrationMatricesTable";
     	  fmT2 := tvb2#"filtrationMatricesTable";
     	  -- Computing the bases and filtration matrices
     	  k := k1 * k2;
     	  tvb := makeVBKlyachko(k,F);
     	  R := rays tvb;
     	  baseTable := apply(R, r -> bT1#r ** bT2#r);
     	  filtrationTable := apply(R, r -> matrix {flatten apply(flatten entries fmT1#r, e1 -> apply(flatten entries fmT2#r, e2 -> e1 + e2))});
     	  -- Writing the new Tables into the bundle
     	  tvb = addBase(tvb,baseTable);
     	  tvb = addFiltration(tvb,filtrationTable);
     	  if tvb1.cache.?isVB and tvb2.cache.?isVB and tvb1.cache.isVB and tvb2.cache.isVB then tvb.cache.isVB = true;
     	  tvb)
     else error("The two toric vector bundles have to be in the same description."))


ToricVectorBundle ** ToricVectorBundle := (tvb1,tvb2) -> tensor(tvb1,tvb2)
-- ToricVectorBundleKlyachko ** ToricVectorBundleKlyachko := tensor


-- PURPOSE : Computing the twist of a Toric Vector Bundle by a divisor line bundle
--   INPUT : '(T,d)',  where 'T' is a toricVectorBundleKlyachko and 'd' a list of integers one for each ray of the fan
--  OUTPUT : a ToricVectorBundleKlyachko
-- COMMENT : If d={d_1,..d_l} then this corresponds to the line bundle which is the d_i twist on the i-th ray
twist = method(TypicalValue => ToricVectorBundleKlyachko)
twist (ToricVectorBundleKlyachko,List) := (T,d) -> (
     k := T#"rank of the vector bundle";
     fT := T#"filtrationMatricesTable";
     -- Checking for input errors
     if #d != #fT then error("The number of twists has to match the number of rays of the fan.");
     R := rays T;
     fT = apply(#R, i -> fT#(R#i) + matrix{toList(k:-(d#i))});
     addFiltration(T,fT))


-- PURPOSE : Computing the Cartier index of a Weil divisor
--   INPUT : '(L,F)',  where 'F' is a Fan and 'L' is a list of integers defining a Weil divisor
--  OUTPUT : The smallest multiple of the divisor which is Cartier if the divisor is QQ-Cartier, if not 
--     	     an error is returned
cartierIndex = method(TypicalValue => ZZ)
cartierIndex (List,Fan) := (L,F) -> (
     rl := raySort toList F#"rays";
     -- Checking for input errors
     if #L != #rl then error("The number of weights has to equal the number of rays.");
     n := ambDim F;
     -- Checking for further errors and assigning the weights to the rays
     L = hashTable apply(#rl, i -> (if class L#i =!= ZZ then error("The weights have to be in ZZ."); rl#i => L#i));
     -- Keeping track of the lowest common multiple of denominators of the degrees,
     -- to check wether the divisor itself is Cartier or which multiple
     denom := 1;
     -- Computing the degree vector for every top dimensional cone
     scan(sort maxCones F, C -> (
	       rC := C#"rays";
	       -- Taking the first n x n submatrix
	       rC1 := rC_{0..n-1};
	       -- Setting up the solution vector by composing the corresponding weights
	       v := matrix apply(n, i -> (c := rC1_{i}; {-(L#c)}));
	       -- Computing the degree vector
	       w := vertices intersection(matrix {toList(n:0)},matrix {{0}},transpose rC1,v);
	       -- Checking if w also fulfils the equations given by the remaining rays
	       if numColumns rC != n then (
		    v = v || matrix apply(toList(n..(numColumns rC)-1), i -> {-(L#(rC_{i}))});
	            if (transpose rC)*w - v != 0 then error("The weights do not define a Cartier divisor."));
	       -- Check if w is QQ-Cartier
	       scan(flatten entries w, e -> denom = lcm(denominator e ,denom))));
     denom)


-- PURPOSE : Generating the Vector Bundle given by a divisor
weilToCartier = method(Options => {"Type" => "Klyachko"})

--   INPUT : '(L,F)',  a list 'L' of weight vectors, one for each ray of the Fan 'F'
--  OUTPUT : 'tvb',  a ToricVectorBundle
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
weilToCartier (List,Fan) := opts -> (L,F) -> (
     rl := raySort toList F#"rays";
     -- Checking for input errors
     if #L != #rl then error("The number of weights has to equal the number of rays.");
     n := ambDim F;
     if opts#"Type" == "Kaneyama" then (
	  if not isPure F or ambDim F != dim F then error("Expected the Fan to be pure of maximal dimension.");
     	  -- Creating 0 matrices to compute intersection of hyperplanes to compute the degrees
	  Mfull := matrix {toList(n:0)};
	  vfull := matrix {{0}};
	  -- Checking for further errors and assigning the weights to the rays
	  L = hashTable apply(#rl, i -> (if class L#i =!= ZZ then error("The weights have to be in ZZ."); rl#i => L#i));
	  -- Keeping track of the lowest common multiple of denominators of the degrees,
	  -- to check wether the divisor itself is Cartier or which multiple
	  denom := 1;
	  -- Computing the degree vector for every top dimensional cone
	  tvb := makeVBKaneyama(1,F);
	  gC := sort keys tvb#"degreeTable";
	  gC = apply(gC, C -> (
		    rC := C#"rays";
		    -- Taking the first n x n submatrix
		    rC1 := rC_{0..n-1};
		    -- Setting up the solution vector by composing the corresponding weights
		    v := matrix apply(n, i -> (c := rC1_{i}; {-(L#c)}));
		    -- Computing the degree vector
		    w := vertices intersection(Mfull,vfull,transpose rC1,v);
		    -- Checking if w also fulfils the equations given by the remaining rays
		    if numColumns rC != n then (
			 v = v || matrix apply(toList(n..(numColumns rC)-1), i -> {-(L#(rC_{i}))});
			 if (transpose rC)*w - v != 0 then error("The weights do not define a Cartier divisor."));
		    -- Check if w is QQ-Cartier
		    scan(flatten entries w, e -> denom = lcm(denominator e ,denom));
		    w));
	  -- If the divisor is only QQ Cartier, then its replaced by its first Cartier multiple
	  if denom != 1 then error("The divisor is only QQ-Cartier, but "|toString(denom)|" times the divisor is Cartier.");
	  gC = apply(gC, e -> substitute(denom*e,ZZ));
	  -- Construct the actual line bundle
	  addDegrees(tvb,gC))
     else if opts#"Type" == "Klyachko" then (
	  if any(L, l -> not instance(l,ZZ)) then error("The weights have to be in ZZ.");
	  ind := cartierIndex(L,F);
	  if ind != 1 then error("The divisor is only QQ-Cartier, but "|toString(ind)|" times the divisor is Cartier.");
	  T := makeVBKlyachko(1,F,apply(L, l -> matrix{{1_QQ}}),apply(L, l -> matrix{{-l}}));
     	  T.cache.isVB = true;
	  T)
     else error("Expected Type to be Klyachko or Kaneyama."))


-- PURPOSE : Constructing the fan of projective n-space
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The fan of projective n-space
projectiveSpaceFan = method(TypicalValue => Fan)
projectiveSpaceFan ZZ := n -> (
     if n < 1 then error("The dimension has to be strictly positive.");
     normalFan convexHull (map(ZZ^n,ZZ^n,1)|map(ZZ^n,ZZ^1,0)))


-- PURPOSE : Constructing the fan of the product of n projective 1-spaces
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The fan of the product of n projective 1-spaces
pp1ProductFan = method(TypicalValue => Fan)
pp1ProductFan ZZ := n -> (
     if n < 1 then error("The number of PP^1's has to be strictly positive.");
     normalFan hypercube n)


-- PURPOSE : Constructing the fan of the Hirzebruch n-surface
--   INPUT : 'n',  a positive integer
--  OUTPUT : The fan of the Hirzebruch n-surface
hirzebruchFan = method(TypicalValue => Fan)
hirzebruchFan ZZ := n -> hirzebruch n



---------------------------------------
-- AUXILLIARY FUNCTIONS, not public
---------------------------------------


-- PURPOSE : Computing the Cech complex of a vector bundle
cechComplex = method()

--   INPUT : '(k,T,u)', where 'k' is an integer between -1 and the dimension of the bundle +1, 'T' a ToricVectorBundleKlyachko, and 'u' a
--     	    	        one column matrix giving a degree vector
--  OUTPUT : '(Fk,Fkcolumns,FktoFk+1)', where 'Fk' is a hashTable with the summands of the 'k'th chain, 'Fkcolumns' is a hashTable with the
--     	    	      	   	        dimensions of these summands, and 'FktoFk+1' is a hashTable with the components of the 'k'th 
--     	    	      	   	        boundary operator
cechComplex (ZZ,ToricVectorBundleKlyachko,Matrix) := (k,T,u) -> (
     -- Checking for input errors
     if numRows u != T#"dimension of the variety" or numColumns u != 1 then error("Expected a matrix with 1 column and ", toString T#"dimension of the variety", " rows.");
     if ring u =!= ZZ then error("The degree has to be an integer vector.");
     if k < -1 or T#"dimension of the variety"+1 < k then error("k has to be between 0 and the variety dimension for the k-th cohomolgy");
     -- For a given space F1 at chain k in the filtration together with the degree vector 'u' and the information of the bundle this auxillary 
     -- function computes the boundary operator to the next chain (k+1) which is F1toF2, the dimensions of the summands of 'F1' in 'F1columns' 
     -- and the next chain 'F2'
     makeNewDiffAndTarget := (F1,u,fMT,rT,bT,tvbR,tvbrank,k,n) -> (
	  F2 := {};
	  F1toF2 := {};
	  counter := 0;
	  F1columns := {};
	  -- if k==n then the next chain is 0 as well as the boundary operator
	  if k == n then (
	       F2 = {(0,{},map(tvbR^tvbrank,tvbR^0,0))};
	       F1toF2 = {};
	       F1columns = {0 => tvbrank})
	  -- k==n-1 then the next chain is "complete bundle" and the boundary operator is the map of all summands of Fn-2
	  else if k == n-1 then (
	       F2 = {(0,{},map(tvbR^tvbrank,tvbR^tvbrank,1))};
	       F1toF2 = apply(pairs F1, (j,dat) -> (
			 F1columns = append(F1columns,j => numColumns(dat#1));
			 (j,0,dat#1))))
	  else (
	       -- for each cone in F1 compute the cones of one dimension less and their bundle
	       scan(pairs(F1), (num,dat) -> (
			 R := dat#0;
			 Er := dat#1;
			 -- go through the rays of the cone and remove each of them at a time
			 scan(#R, i -> (
				   Ri := drop(R,{i,i});
				   pos := position(F2, f -> f#1 === Ri);
				   -- Check if the resulting cone already exists in the new chain F2, if so just add Er to the boundary operator
				   if pos =!= null then F1toF2 = append(F1toF2,(num,pos,((-1)^i)*Er)) else (
					-- if not compute E for new cone and append it to F2
					Esum := apply(Ri, r -> (rT#r,((transpose u)*r)_(0,0),r));
					Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#2)), j -> (j <= e#1)),e#2));
					if any(Esum, e -> e#1 == {}) then F2 = append(F2,(counter,Ri,map(tvbR^tvbrank,tvbR^0,0))) else (
					     E := map(tvbR^tvbrank,tvbR^tvbrank,1);
					     Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
					     Esum = apply(Esum, e -> (bT#(e#2))_(e#1));
					     scan(Esum, A -> E = intersectMatrices(E,A));
					     F2 = append(F2,(counter,Ri,E)));
					F1toF2 = append(F1toF2,(num,counter,((-1)^i)*Er));
					counter = counter + 1)));
			 -- Save the dimension of Er into F1columns
			 F1columns = append(F1columns,num => numColumns Er))));
	  (hashTable apply(F1toF2, f -> (f#0,f#1) => f#2),hashTable F1columns,hashTable apply(F2, f -> f#0 => (f#1,f#2))));
     if not T.cache.?cech then T.cache.cech = new MutableHashTable;
     fMT := T#"filtrationMatricesTable";
     tvbR := T#"ring";
     tvbrank := T#"rank of the vector bundle";
     n := T#"dimension of the variety";
     -- if k==n+1 the chain is 0 and there is no map
     if k == n+1 then (hashTable {0 => ({},map(tvbR^tvbrank,tvbR^0,0))},hashTable {},hashTable {}) else (
	  rT := T#"rayTable";
	  bT := T#"baseTable";
	  if not T.cache.cech#?(k,u) then (
	       -- rT will be used to sort the rays
	       
	       -- if the previous chain has not been computed we have to compute the cones of the right dimension (n-k)
	       if not T.cache.cech#?(k-1,u) or k == 0 then (
		    -- if k==n then the chain is the "complete bundle" and the next chain is 0
		    if k == n then (
			 T.cache.cech#(k,u) = (hashTable {0 => ({},map(tvbR^tvbrank,tvbR^tvbrank,1))},hashTable {0 => tvbrank},hashTable {});
			 T.cache.cech#(k+1,u) = hashTable {0 => ({},map(tvbR^tvbrank,tvbR^0,0))})
		    -- if k==-1 the chain is 0
		    else if k == -1 then T.cache.cech#(k,u) = (hashTable { 0 => ({},map(tvbR^tvbrank,tvbR^0,0))},hashTable {0 => 0},hashTable {})
		    else (
			 F1 := cones(n-k,T#"ToricVariety");
			 -- for each n-k cone in the fan compute Er, the bundle over this cone for the degree u
			 F1 = hashTable apply(#F1, Cnum -> (
				   C := F1#Cnum;
				   R := C#"rays";
				   R = apply(numColumns R, i -> (R_{i}));
				   R = sort apply(R, r -> (rT#r,r));
				   Esum := apply(R, r -> (r#0,((transpose u)*(r#1))_(0,0),r#1));
				   R = apply(R, r -> (r#1));
				   Esum = apply(Esum, e -> (e#0,positions(flatten entries fMT#(e#2), j -> (j <= e#1)),e#2));
				   if any(Esum, e -> e#1 == {}) then Cnum => (R,map(tvbR^tvbrank,tvbR^0,0)) else (
					E := map(tvbR^tvbrank,tvbR^tvbrank,1);
					Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
					Esum = apply(Esum, e -> (bT#(e#2))_(e#1));
					scan(Esum, A -> E = intersectMatrices(E,A));
					Cnum => (R,E))));
			 -- Compute the boundary operator with the auxillary function
			 (F1toF2,F1columns,F2) := makeNewDiffAndTarget(F1,u,fMT,rT,bT,tvbR,tvbrank,k,n);
			 T.cache.cech#(k,u) = (F1,F1columns,F1toF2);
			 -- Save the next chain to the cache
			 if not T.cache.cech#?(k+1,u) then T.cache.cech#(k+1,u) = F2))
	       else (
		    -- if the previous chain exists use this to compute the chain in question
		    F10 := T.cache.cech#(k-1,u);
		    (F10toF11,F10columns,F11) := makeNewDiffAndTarget(F10,u,fMT,rT,bT,tvbR,tvbrank,k-1,n);
		    (F11toF12,F11columns,F12) := makeNewDiffAndTarget(F11,u,fMT,rT,bT,tvbR,tvbrank,k,n);
		    T.cache.cech#(k-1,u) = (F10,F10columns,F10toF11);
		    T.cache.cech#(k,u) = (F11,F11columns,F11toF12);
		    -- save the next chain to the cache as well
		    if not T.cache.cech#?(k+1,u) then T.cache.cech#(k+1,u) = F12))
	  -- if the cache only consists of the chain but not of the boundary operator compute this
	  else if not instance(T.cache.cech#(k,u),Sequence) then (
	       F21 := T.cache.cech#(k,u);
	       (F21toF22,F21columns,F22) := makeNewDiffAndTarget(F21,u,fMT,rT,bT,tvbR,tvbrank,k,n);
	       T.cache.cech#(k,u) = (F21,F21columns,F21toF22);
	       if not T.cache.cech#?(k+1,u) then T.cache.cech#(k+1,u) = F22);
	  T.cache.cech#(k,u)))

--   INPUT : '(k,T,u)', where 'k' is an integer between -1 and the dimension of the bundle +1, 'T' a ToricVectorBundleKlyachko, and 'u' a
--     	    	        one column matrix giving a degree vector
--  OUTPUT : '(Fk,Fkcolumns,FktoFk+1)', where 'Fk' is a hashTable with the summands of the 'k'th chain, 'Fkcolumns' is a hashTable with the
--     	    	      	   	        dimensions of these summands, and 'FktoFk+1' is a hashTable with the components of the 'k'th 
--     	    	      	   	        boundary operator
cechComplex (ZZ,ToricVectorBundleKaneyama,Matrix) := (k,tvb,u) -> ( 
     -- Checking for input errors
     if numRows u != tvb#"dimension of the variety" or numColumns u != 1 then error("Expected a matrix with 1 column and ", toString tvb#"dimension of the variety", " rows.");
     if ring u =!= ZZ then error("The degree has to be an integer vector.");
     if k < 0 or tvb#"dimension of the variety"+1 < k then error("k has to be between 0 and the variety dimension for the k-th cohomolgy.");
     -- For a given space F1 at chain k in the filtration together with the degree vector 'u' and the information of the bundle this auxillary 
     -- function computes the boundary operator to the next chain (k+1) which is F1toF2, the dimensions of the summands of 'F1' in 'F1columns' 
     -- and the next chain 'F2'
     makeNewDiffAndTarget := (M1,rk,l,tCT,bCT,dT) -> (
	  -- Recursive function that finds a path over codim 1 cones from one topdim cone ('i') to another ('j')
     	  -- using the steps in 'pl'
     	  findpath := (i,j,pl) -> (
	       -- Recursive function finds a path from the actual cone 'i' to the Cone 'j' using the steps in 'pl'
	       -- where 'cl' is the  sequence of steps taken so far from the original 'i' and 'minpath' is the 
	       -- shortest path found so far
	       findrecursive := (i,j,pl,cl,minpath) -> (
	       	    -- If the last step from 'i' to 'j' is part of 'pl' then add '(i,j)' to 'cl'
	       	    if member((i,j),pl) or member((j,i),pl) then (
		    	 cl = append(cl,(i,j));
		    	 -- Check if the new found path is shorter than shortest so far
		    	 if #cl < #minpath or minpath == {} then minpath = cl)
	       	    -- otherwise find a path with the remaining steps in 'pl'
	       	    else (
		    	 L1 := {};
		    	 L2 := {};
		    	 -- Sort the remaining possible steps into those containing 'i'in 'L1' and those who not in 'L2'
		    	 for e in pl do if member(i,e) then L1 = append(L1,e) else L2 = append(L2,e);
		    	 -- Call findrecursive for each step in 'L1', with new starting cone the other index in the pair and new 
		    	 -- remaining pairs list 'L2' and add the step to 'cl'
		    	 for e in L1 do ( 
			      if e#0 == i then minpath = findrecursive(e#1,j,L2,append(cl,e),minpath)
			      else minpath = findrecursive(e#0,j,L2,append(cl,(e#1,e#0)),minpath)));
	       	    minpath);
	       -- Start with an empty sequence of steps, no minimal path yet and all possible stepsd
	       cl := {};
	       minpath := {};
	       findrecursive(i,j,pl,cl,minpath));
	  M2 := {};
	  for p in pairs M1 do (
	       L := select(toList(0..rk-1), i -> not member(i,p#1#0));
	       for i from last(p#0)+1 to l-1 do (
		    cl := append(p#0,i);
		    C := intersection(p#1#1,tCT#i);
		    degs := dT#(tCT#(cl#0));
		    M2 = append(M2,cl => (sort unique join(p#1#0,select(L, i -> contains(dualCone C,u- degs_{i}))),C))));
	  M2 = hashTable M2;
	  -- Constructing the zero map over QQ
     	  d1 := map(QQ^0,QQ^0,0);
     	  -- Constructing the matrix of the sequence for the cohomology
	  scan(pairs M1, (a,b) -> (
		    b = b#0;
		    -- 'A' will be a column of the matrix d1 of the sequence
		    A := map(QQ^0,QQ^(#b),0);
		    -- One intersection in M1 is selected, by going through the intersections in M2 we get the first "column" of block matrices in A 
		    -- by looking at the images in all intersections in M2
		    scan(pairs M2, (c,d) -> (
			      -- Only if the intersection is made by intersecting with one more cone, the resulting matrix has to be computed, 
			      -- because otherwise it is automatically zero
			      if isSubset(a,c) then (
				   -- get the signum by looking at the position the new cone is inserted
				   signum := (-1)^(#c - position(c, e -> not member(e,a)) - 1);
				   i := a#0;
				   j := c#0;
				   -- if i == j then no base change between the two representations has to be made, so the submatrix of the 
				   -- identity inserting the positions of the degrees 'b' into the degrees 'd' is added in this column
				   if i == j then A = A || (signum * (map(QQ^rk,QQ^rk,1))_b)
				   -- Otherwise we have to find the transition matrix from cone 'i' to Cone 'j'
				   else (
					-- find the transition matrix
					mpath := findpath(i,j,keys bCT);
					-- If the path has one element then we take the 'b'-'d' part of that matrix, otherwise the multiplication 
					-- of the matrices corresponding to the steps in the path and add the path as a new step with corresponding matrix
					if #mpath == 1 then (
					     if i < j then A = A || (signum * (bCT#(i,j))_b)
					     else A = A || (signum * (inverse (bCT#(j,i)))_b))
					else (
					     A1 := map(QQ^rk,QQ^rk,1);
					     for p in mpath do (
						  if p#0 < p#1 then A1 = bCT#p * A1
						  else A1 = (inverse bCT#(p#1,p#0))*A1);
					     if i < j then bCT = hashTable join(apply(pairs bCT, ps -> ps#0 => ps#1), {(i,j) => A1})
					     else bCT = hashTable join(apply(pairs bCT, ps -> ps#0 => ps#1), {(j,i) => inverse A1});
					     A = A || (signum * A1_b))))
			      else (
				   A = A || map(QQ^rk,QQ^(#b),0))));
		    -- Adding the new column to d1
		    if d1 == map(QQ^0,QQ^0,0) then d1 = A
		    else d1 = d1 | A));
	  (d1,M2));
     if not tvb.cache.?cech then tvb.cache.cech = new MutableHashTable;
     rk := tvb#"rank of the vector bundle";
     l := tvb#"number of affine charts";
     tCT := sort keys tvb#"topConeTable";
     bCT := tvb#"baseChangeTable";
     dT := tvb#"degreeTable";
     if not tvb.cache.cech#?(k,u) then (
     	  if k == 0 then (
	       M20 := hashTable apply(subsets(l,k+1), cl -> (
		    	 C := intersection apply(cl, i -> tCT#i);
		    	 degs := dT#(tCT#(cl#0));
		    	 L := select(toList(0..rk-1), i -> contains(dualCone C,u - degs_{i}));
		    	 cl => (L,C)));
	       (d20,M30) := makeNewDiffAndTarget(M20,rk,l,tCT,bCT,dT);
	       tvb.cache.cech#(k,u) = (M20,d20);
	       tvb.cache.cech#(k+1,u) = M30)
	  else (
	       M1 := if not tvb.cache.cech#?(k-1,u) then (
	       	    hashTable apply(subsets(l,k), cl -> (
		    	      C := intersection apply(cl, i -> tCT#i);
		    	      degs := dT#(tCT#(cl#0));
		    	      L := select(toList(0..rk-1), i -> contains(dualCone C,u - degs_{i}));
		    	      cl => (L,C)))) else tvb.cache.cech#(k-1,u);
	       (d1,M2) := makeNewDiffAndTarget(M1,rk,l,tCT,bCT,dT);
	       (d2,M3) := makeNewDiffAndTarget(M2,rk,l,tCT,bCT,dT);
	       tvb.cache.cech#(k-1,u) = (M1,d1);
	       tvb.cache.cech#(k,u) = (M2,d2);
	       tvb.cache.cech#(k+1,u) = M3))
     else if not instance(tvb.cache.cech#(k,u),Sequence) then (
	  M21 := tvb.cache.cech#(k,u);
	  (d21,M31) := makeNewDiffAndTarget(M21,rk,l,tCT,bCT,dT);
	  tvb.cache.cech#(k,u) = (M21,d21);
	  tvb.cache.cech#(k+1,u) = M31);
     tvb.cache.cech#(k,u))


-- PURPOSE : Checking for a matrix if it is over ZZ or QQ and returning an error if not
--   INPUT : '(M,msg)',  where 'M' is a matrix and 'msg' is the name of the object 'M' describes
--  OUTPUT : The matrix promoted to QQ if it was over ZZ or QQ, otherwise an error
chkZZQQ = (M,msg) -> (
     R := ring M;
     if R =!= ZZ and R =!= QQ then error("expected matrix of ",msg," to be over ZZ or QQ");
     promote(M,QQ));


-- PURPOSE : Computing the cohomology of a given ToricVectorBundle
cohom = method()
--   INPUT : '(k,tvb,u)',  'k' for the 'k'th cohomology group, 'tvb' a ToricVectorBundleKaneyama, and 'u' the degree
--  OUTPUT : 'ZZ',	     the dimension of the degree 'u' part of the 'k'th cohomology group of 'tvb'
cohom (ZZ,ToricVectorBundleKaneyama,Matrix) := (k,T,u) -> (
     if not T.cache.?HH then T.cache.HH = new MutableHashTable;
     if not T.cache.HH#?(k,u) then (
	  -- Get the k-1 th and k th differential
	  d := if k == 0 then rank ker (cechComplex(k,T,u))#1 else (
	       -- Generate the two boundary operators
	       d1 := (cechComplex(k-1,T,u))#1;
	       d2 := (cechComplex(k,T,u))#1;
	       (rank ker d2) - (rank image d1));
	  T.cache.HH#(k,u) = (ring T)^(toList(d:flatten entries(-u))));
     T.cache.HH#(k,u))

--   INPUT : '(k,tvb,u)',  'k' for the 'k'th cohomology group, 'tvb' a ToricVectorBundleKlyachko, and 'u' the degree
--  OUTPUT : 'ZZ',	     the dimension of the degree 'u' part of the 'k'th cohomology group of 'tvb'
cohom (ZZ,ToricVectorBundleKlyachko,Matrix) := (k,T,u) -> (
     if not T.cache.?HH then T.cache.HH = new MutableHashTable;
     if not T.cache.HH#?(k,u) then (
	  -- Get the k-1 th, k th and k+1 th chain in the Cech complex
     	  (F1,F1columns,F1toF2) := cechComplex(k-1,T,u);
     	  (F2,F2columns,F2toF3) := cechComplex(k,T,u);
     	  F3 := (cechComplex(k+1,T,u))#0;
     	  tvbR := T#"ring";
     	  tvbrank := T#"rank of the vector bundle";
     	  -- Generate the two boundary operators
     	  MapF1toF2 := matrix apply(#F2, j -> apply(#F1, i -> if F1toF2#?(i,j) then F1toF2#(i,j) else map(tvbR^tvbrank,tvbR^(F1columns#i),0)));
     	  MapF2toF3 := matrix apply(#F3, j -> apply(#F2, i -> if F2toF3#?(i,j) then F2toF3#(i,j) else map(tvbR^tvbrank,tvbR^(F2columns#i),0)));
     	  -- Compute the cohomology
     	  d := (rank ker MapF2toF3)-(rank image MapF1toF2);
     	  T.cache.HH#(k,u) = (ring T)^(toList(d:flatten entries(-u))));
     T.cache.HH#(k,u))


-- PURPOSE : Computing the cotangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama 
cotangentBundleKaneyama = F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety has to be smooth.");
     if not isComplete F then error("The Toric Variety has to be complete.");
     if not isPointed F then error("The Fan has to be pointed.");
     -- Generating the trivial bundle of dimension n
     n := dim F;
     tvb := makeVBKaneyama(n,F);
     tCT := sort keys tvb#"topConeTable";
     pairlist := keys tvb#"baseChangeTable";
     -- Computing the degrees and transition matrices of the cotangent bundle
     degreeTable := hashTable apply(tCT, p -> p => substitute(rays dualCone p,ZZ));
     baseChangeTable := hashTable apply(pairlist, p -> ( p => substitute(inverse(degreeTable#(tCT#(p#1)))*(degreeTable#(tCT#(p#0))),QQ)));
     -- Writing the data into the bundle
     E := new ToricVectorBundleKaneyama from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => n,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable",
	  symbol cache => new CacheTable};
     E.cache.regCheck = true;
     E.cache.cocyle = true;
     E)


-- PURPOSE : Constructing the fan of projective n-space
generateRandomMatrix = method(TypicalValue => Matrix)

--   INPUT : '(m,n,h)',  where 'm' and 'n' are strictly positive integers and 'h' is an integer
--  OUTPUT : An 'm' by 'n' matrix with random entries between 0 and 'h'
generateRandomMatrix (ZZ,ZZ,ZZ) := (m,n,h) -> matrix apply(m, i -> apply(n, j -> random h+1))

--   INPUT : '(m,n,l,h)',  where 'm' and 'n' are strictly positive integers and 'l' 'h' are integers 
--     	    	      	   of which 'l' is the smaller one
--  OUTPUT : An 'm' by 'n' matrix with random entries between 0 and 'h'
generateRandomMatrix (ZZ,ZZ,ZZ,ZZ) := (m,n,l,h) -> matrix apply(m, i -> apply(n, j -> random(l,h)))


-- PURPOSE : Computing the intersection of the images of two matrices
--   INPUT : '(M,N)', two matrices with the same target
--  OUTPUT : a matrix with the minimal generators of the intersection
intersectMatrices = (M,N) -> (
     m := numColumns M;
     N = gens ker(M | N);
     N = N^{0..m-1};
     gens trim image(M*N));


-- PURPOSE : Building a Vector Bundle of rank 'k' on the Toric Variety given by the Fan 'F'
--           with 0 degrees and identity transition matrices
--   INPUT : '(k,F)',  a strictly positive integer 'k' and a pure and full dimensional
--                     Fan 'F' 
--  OUTPUT : The ToricVectorBundleKaneyama 'VB'
makeVBKaneyama = method(TypicalValue => ToricVectorBundleKaneyama)
makeVBKaneyama (ZZ,Fan) := (k,F) -> (
     -- Checking for input errors
     if k < 0 then error("The vector bundle must have a positive rank.");
     if not isComplete F then error("The fan has to be complete.");
     if not isPointed F then error("The fan has to be pointed.");
     -- Writing the table of Cones of maximal dimension
     n := dim F;
     topConeTable := sort toList F#"generatingCones";
     topConeTable = hashTable apply(#topConeTable, i -> topConeTable#i => i);
     -- Saving the index pairs of top dimensional Cones that intersect in a codim 1 Cone
     Ltable := hashTable {};
     scan(pairs topConeTable, (C,a) -> Ltable = merge(Ltable,hashTable apply(faces(1,C), e -> e => a),(b,c) -> if b < c then (b,c) else (c,b)));
     Ltable = hashTable flatten apply(pairs Ltable, p -> if instance(p#1,Sequence) then p#1 => p#0 else {});
     -- Removing Cones on the "border" of F, which have only 1 index
     pairlist := sort keys Ltable;
     -- Saving the identity into the Table of transition matrices
     baseChangeTable := hashTable apply(pairlist, p -> p => map(QQ^k,QQ^k,1));
     -- Saving 0 degrees into the degree table
     degreeTable := hashTable apply(keys topConeTable, C -> C => map(ZZ^n,ZZ^k,0));
     -- Making the vector bundle
     new ToricVectorBundleKaneyama from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "codim1Table" => Ltable,
	  "ToricVariety" => F,
	  "number of affine charts" => #topConeTable,
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => k,
	  "topConeTable" => topConeTable,
	  symbol cache => new CacheTable})

--   INPUT : '(k,F,degreeList,matrixList)',  a strictly positive integer 'k', a pure and full dimensional
--                     Fan 'F' of dimension n, a list 'degreeList' of k by n matrices over ZZ, one for each 
--     	    	       top dimensional Cone in 'F' where the columns give the degrees of the generators in the 
--     	    	       corresponding affine chart to this Cone, and a list 'matrixList' of  k by k matrices 
--     	    	       over QQ, one for each pair of top dimensional Cones intersecting in a common codim 1 face. 
--  OUTPUT : The ToricVectorBundleKaneyama 'tvb' 
-- COMMENT : Note that the top dimensional cones are numbered starting with 0 and the codim 1 intersections are 
--           labelled by pairs (i,j) denoting the the two top dim cones involved, with i<j and they are ordered
--     	     in lexicographic order. So the matrices in 'matrixList' will be assigned to the pairs (i,j) in that 
--     	     order, where the matrix A assigned to (i,j) denotes the transition
--     	    	 (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)* A
--     	     The matrices in 'degreeList' will be assigned to the cones in the order in which they are numbered.
makeVBKaneyama (ZZ,Fan,List,List) := (k,F,degreelist,matrixlist) -> (
     -- Generating the trivial vector bundle of rank k
     tvb := makeVBKaneyama(k,F);
     -- Adding the given degrees and transition matrices
     tvb = addDegrees(tvb,degreelist);
     tvb = addBaseChange(tvb,matrixlist);
     tvb)


-- PURPOSE : Building a Vector Bundle in the Klyachko description of rank 'k' on the Toric Variety given by the Fan 'F'
--           with trivial Filtration for every ray
--   INPUT : '(k,F)',  a strictly positive integer 'k' and a pure and full dimensional Fan 'F' 
--  OUTPUT : The ToricVectorBundleKlyachko 'VB'
makeVBKlyachko = method(TypicalValue => ToricVectorBundleKlyachko)
makeVBKlyachko (ZZ,Fan) := (k,F) -> (
     -- Checking for input errors
     if k < 0 then error("The vector bundle must have a positive rank.");
     if not isPointed F then error("The Fan has to be pointed");
     -- Writing the table of rays
     rT := raySort toList F#"rays";
     rT = hashTable apply(#rT, i -> rT#i => i);
     -- Writing the table of identity matrices for the vector bundle bases
     bT := hashTable apply(keys rT, i -> i => map(QQ^k,QQ^k,1));
     -- Writing the table of matrices for the filtration maps
     fMT := hashTable apply(keys rT, i -> i =>  matrix {toList(k:0)});
     -- Computing the list of changes in the filtrations
     fT := hashTable apply(pairs fMT, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({min L1 - 1 => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
     -- Generating the vector bundle
     tvb := new ToricVectorBundleKlyachko from {
	  "ring" => QQ,
	  "rayTable" => rT,
	  "baseTable" => bT,
	  "filtrationMatricesTable" => fMT,
	  "filtrationTable" => fT,
	  "ToricVariety" => F,
	  "number of affine charts" => #(F#"generatingCones"),
	  "dimension of the variety" => dim F,
	  "rank of the vector bundle" => k,
	  "number of rays" => #rT,
	  symbol cache => new CacheTable};
     tvb.cache.isVB = true;
     tvb)

--   INPUT : '(k,F,baseList,filtrationList)',  a strictly positive integer 'k', a pure and full dimensional
--                     Fan 'F' of dimension n, a list 'baseList' of k by k matrices over the same ring/field, one for each 
--     	    	       ray of 'F' where the columns give the basis of the vector bundle over the ray, and a list 
--     	    	       'filtrationList' of  1 by k matrices over ZZ, one for each ray such that the i-th column of 
--     	    	       the base matrix is at first in the part of the filtration indexed by the i-th entry in the filtration 
--     	    	       matrix.
--  OUTPUT : The ToricVectorBundleKlyachko 'tvb' 
-- COMMENT : Note that the bases and filtration matrices will be assigned to the rays in the order, they appear in rays F
makeVBKlyachko (ZZ,Fan,List,List) := (k,F,Bm,Fm) -> (
     tvb := makeVBKlyachko(k,F);
     tvb = addBase(tvb,Bm);
     addFiltration(tvb,Fm))


-- PURPOSE : Solving the system R*X=F
--   INPUT : '(R,F)',  two matrices over ZZ
--  OUTPUT : a matrix of QQ solutions
systemSolver = (R,F) -> (
     (R1,Lmatrix,Rmatrix) := smithNormalForm lift(R,ZZ);
     F1 := entries(Lmatrix * F);
     Rmatrix * (matrix apply(numRows R1, i -> F1#i / R1_(i,i)) || map(QQ^(numColumns R1 - numRows R1),QQ^(#F1#0),0)))


-- PURPOSE : Generating the table of all rays together with their filtration 
--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : a hashTable,  with keys the rays of the variety and for each ray a list of pairs (the filtration step, the filtration)
tableForAllRays = method(TypicalValue => HashTable)
tableForAllRays ToricVectorBundleKlyachko := (cacheValue symbol allRaysTable)( T -> (
	  fMT := T#"filtrationMatricesTable";
     	  bT := T#"baseTable";
	  hashTable apply(rays T, r -> (
		    fT := flatten entries fMT#r;
	       	    r => apply(fT, e -> (e,(bT#r)_(positions(fT, i -> i <= e))))))))


-- PURPOSE : Computing the tangent bundle (Klyachko) on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko 
tangentBundleKlyachko = F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety has to be smooth.");
     -- Generating the trivial bundle of dimension n
     n := dim F;
     tvb := makeVBKlyachko(n,F);
     -- Extracting the rayTable
     rayTable := apply(rays tvb,r -> promote(r,QQ));
     -- Adding the filtration matrix |-1,0,0,...,0| for each ray
     filtrationTable := apply(rayTable, r -> matrix{flatten({-1,toList(n-1:0)})});
     -- Adding the base which has as first vector the ray itself to each ray
     baseTable := apply(rayTable, r -> r | complement r);
     -- Adding bases filtration matrices to the bundle
     tvb = addFiltration(tvb,filtrationTable);
     tvb = addBase(tvb,baseTable);
     tvb.cache.isVB = true;
     tvb)



---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     Key => OldToricVectorBundles,
     Headline => "cohomology computations of equivariant vector bundles on toric varieties",
     
     "Using the descriptions of Kaneyama and Klyachko this package implements the construction of
     equivariant vector bundles on toric varieties.",
     
     PARA{}, "Note that this package implements vector bundles in Kaneyama's description only over 
     pure and full dimensional fans.",
     
     PARA{}, TT "OldToricVectorBundles", " uses the ", TO OldPolyhedra, " package by ", 
     HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm", "René Birkner"), ". At least version 1.1 
     of ",TO OldPolyhedra," must be installed via ",TO installPackage," to use ",TT "OldToricVectorBundles",".",
     
     PARA{}, "Each vector bundle is saved either in the description of Kaneyama or the one of Klyachko. The 
     first description gives the multidegrees (in the dual lattice of the fan) of the generators of the bundle 
     over each full dimensional cone, and for each codimension-one cone a transition matrix 
     (See ",TO ToricVectorBundleKaneyama,"). The description of an equivariant vector bundle given by Klyachko 
     consists of filtrations of a fixed vector space for each ray in the fan of the base variety. Furthermore,
     these filtrations have to satisfy a certain compatibility condition (See ",TO ToricVectorBundleKlyachko,").",
     
     PARA{}, "For the mathematical background see ",
     
     UL {
	  {"Tamafumi Kaneyama,",EM "On equivariant vector bundles on an almost homogeneous variety", ", Nagoya Math. J. 57, 1975."},
	  {"Alexander A. Klyachko,",EM "Equivariant bundles over toral varieties", ", Izv. Akad. Nauk SSSR Ser. Mat., 53, 1989."},
	  {"Markus Perling,",EM "Resolution and moduli for equivariant sheaves over toric varieties", ", PhD Thesis, 2003."}
	},
     
     SeeAlso => {"OldPolyhedra::OldPolyhedra"}
     
     }

document {
     Key => ToricVectorBundle,
     Headline => "the class of all toric vector bundles",
     
     "In ",TO OldToricVectorBundles," an equivariant vector bundle on some toric variety is given as an object of class ",TT "ToricVectorBundle"," 
     which can be given in two descriptions:",
     
     UL {
	  {"By a collection of vector spaces with filtration for each ray of the underlying fan, ",TO ToricVectorBundleKlyachko,"."},
	  {"By a set of degree vectors for each maximal cone and a transition matrix for each pair of maximal cones of the underlying 
	       fan, ",TO ToricVectorBundleKaneyama,"."}
	},
     
     PARA{}, "For more detailed descriptions see the corresponding pages of the two subtypes.",
     
     SeeAlso => {ToricVectorBundleKlyachko,
	  ToricVectorBundleKaneyama}
     
     }
   
document {     
     Key => ToricVectorBundleKaneyama,
     Headline => "the class of all toric vector bundles in Kaneyama's description",     
     
     TEX ///"Consider an equivariant vector bundle $E$ of rank $k$ on a toric variety $X$ corresponding to a fan $\Sigma$. Then $E$ is 
     trivial on any invariant open affine subvariety of $X$ and moreover homogeneously generated by $k$ elements. 
     Furthermore, the transition maps between these trivializations are homogeneous of degree zero. Thus, 
     after fixing local homogeneous generators, we get a list of degrees of generators for each cone 
     in $\Sigma$, along with a transition map for each pair of cones. Conversely, given a list of $k$ degrees for every 
     cone of $\Sigma$ along with transition maps satisfying compatibility and regularity conditions for every pair of cones, 
     one can construct an equivariant vector bundle of rank $k$ on $X$."///,
     
     PARA{}, TEX ///"This description of equivariant vector bundles, due to Kaneyama, is implemented for complete, pointed fans in the following way: 
     It is only necessary to consider charts corresponding to maximal dimensional cones of $\Sigma$. Furthermore, each codimension-one cone of 
     $\Sigma$ corresponds to a pair of maximal dimensional cones, and thus to a transition map. Due to the compatibility condition for transition maps, 
     one can reconstruct the transition map corresponding to an arbitrary pair from the maps of this sort. If the dimension of $\Sigma$ is $n$ then 
     for each maximal dimensional cone the degree list of the corresponding chart is saved as an $n$ times $k$ matrix over ",TO ZZ,", giving 
     $k$ degree vectors in the dual lattice of the fan, one for each local generator of the bundle. Additionally, 
     for every pair of maximal cones intersecting in a common codimension-one face, there is a matrix in 
     GL($k$,",TO QQ,"), representing the transition map between these two affine charts. Indeed, suppose that 
     cones $\sigma_1$ and $\sigma_1$ intersect in some codimension-one face, with corresponding affine 
     charts $U_1$ and $U_2$. Then on the intersection, the $i$-th generator for $U_1$ has a unique 
     representation as a linear combination in the generators for $U_2$ after being multiplied with characters to all 
     have the required degree. The coefficients in this representation form the $i$-th column of the desired matrix."///,
     
     PARA{}, TEX ///"We briefly consider the example of  $\mathbb{P}^2$, corresponding to the complete fan with rays 
     through $(0,1)$, $(1,0)$, and $(-1,-1)$. Denote by $x$ the character of weight $[1,0]$ and by $y$ the character 
     of weight $[0,1]$. Now the coordinate rings of the three standard affine charts of $\mathbb{P}^2$ are generated by 
     respectively $(x^{ -1},x^{ -1}y)$, $(x,y)$, and $(xy^{ -1},y^{ -1})$. This means that the modules of differentials 
     are generated by respectively $(d(x^{ -1}),d(x^{ -1}y))$, $(dx,dy)$, and $(d(xy^{ -1}),d(y^{ -1}))$. These modules give us 
     local trivializations of the cotangent bundle on $\mathbb{P}^2$. The degrees of the generators for the first chart 
     then are $[-1,0]$ and $[-1,1]$, for example. Now, since $d(x^{ -1})=-x^{ -2}dx$ and $d(x^{ -1}y) = -x^{ -2}ydx + x^{ -1}dy$, 
     we get that the transition map between the generators of the first and second chart is given by the matrix with 
     columns $(-1,0)$ and $(-1,1)$."///,
          
     PARA{},"An instance of class ToricVectorBundleKlyachko, when displayed or printed, gives an overview  of the 
     characteristics of the bundle:",
     
     EXAMPLE {
	  " E = cotangentBundle(projectiveSpaceFan 2,\"Type\" => \"Kaneyama\")"
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,". The data described above is all stored in a single hash table. In the example from above, the first chart has the key 0, and transition map described above has key (0,1):",
     
     EXAMPLE {
	  " details E"
	  },
     
     Caveat=> {"This implementation only supports vector bundles where the corresponding transition maps have coefficients in ",TO QQ,"."},
     
     SeeAlso => {ToricVectorBundleKlyachko,
	  ToricVectorBundle}
     
          }
     	  
document {     
     Key => ToricVectorBundleKlyachko,
     Headline => "the class of all toric vector bundles in Klyachko's description",
     
     TEX ///"A toric vector bundle on a toric variety $X$ is a locally free sheaf $E$
     together with an action of the torus $T$ on the geometric vector bundle $V(E)$
     such that the projection to the base $X$ is equivariant, and the action of $T$ on the fibers is linear. 
     There also is an induced action of $T$ on the local sections $s \in{}  \Gamma(U,E)$ 
     given by $(t*s)(x) = t^{ -1}(s(t x))$ . This implies that a regular section $x^u \in{}  \Gamma(X,O_X)$ 
     for an element $u$ in the character lattice $M$ also has weight $u$. 
     Other choices for the induced action are possible. In fact, the upper one is different from Klyachko's in his original description 
     where $x^u \in{}  \Gamma(X,O_X)$ has weight $-u$. 
     We denote by $E_0$ the fiber over the unit $t_0 \in{} T$,
     and by $U_\sigma \subset X$ the open affine torus invariant subset associated with the cone $\sigma$. 
     The primitive generator of the ray $\rho$ in the fan $\Sigma$ is denoted by $v_\rho$.
     Evaluating local homogeneous sections $\Gamma(U_{\rho},E)_u$ of weight $u$ at $t_0$ 
     provides us with an embedding of these finite dimensional vector spaces into $E_0$. One can show that 
     the upper choice of the induced torus action implies that the image of $\Gamma(U_\rho,E)_{u_1}$ is contained 
     in the image of $\Gamma(U_\rho,E)_{u_2}$ if and only if  the pairing  $(u_1-u_2,v_\rho) \leq 0$. 
     Furthermore one observes that the image only depends on the class of the weight $u$ 
     in the quotient lattice $M_\rho := M/M^\rho$, where $M^\rho$ denotes the intersection of $M$ 
     with the vector space perpendicular to the ray $\rho$. Since $M_\rho \cong \mathbb{Z}$ we denote the 
     image of $\Gamma(U_\rho,E)_u$ in $E_0$ by $E^\rho(i)$ with $i = (u,v_\rho)$. 
     Each ray $\rho \in{} \Sigma$ thus gives rise to an increasing filtration $\{E^\rho(i)\}$ of $E_0$. 
     Since $E_0$ is finite dimensional there is only a finite set of integers 
     $i$ for which a jump occurs, i.e., $E^\rho(i)$ strictly contains 
     $E^\rho(i-1)$. At all other steps the filtration remains constant. 
     Apart from that, each open affine subset $U_\sigma$ for $\sigma \in{} \Sigma$ induces a direct sum 
     decomposition of $E_0 = \oplus_{u \in{} M_\sigma}E^\sigma_u$ such that $E^\rho(i) = \sum_{(u,v_\rho) \leq i} E^\sigma_u$ 
     for each $\rho \in{} \sigma$ and $i \in{} \mathbb{Z}$. Observe that the lattice $M_\sigma$ 
     is defined analogously to the lattice $M_\rho$, i.e., it is the quotient lattice $M/M^\sigma$ where
     $M^\sigma$ denotes the intersection of $M$ with the vector space perpendicular to the cone $\sigma$."///,
     
     
     PARA{},"With the notation and conventions introduced above it is now possible to state the fundamental theorem of Klyachko which completely
     describes toric vector bundles in linear algebraic terms:",
     
     
     PARA{},TT "The category of toric vector bundles on the toric variety ",TEX ///$X$///, TT " is equivalent to the category of finite 
     dimensional ",TEX ///$k$///, TT"-vector spaces ",TEX ///$E_0$///, TT" with collections of increasing filtrations 
     ",TEX ///$\{E^{\rho}(i)| i \in{} \mathbb{Z}\}$///, TT", indexed by the rays of ",TEX ///$\Sigma$///, TT", satisfying the following 
     compatibility condition: For each cone ",TEX ///$\sigma \in{} \Sigma$///, TT" there is a decomposition ",TEX ///$E_0 = \oplus_{u \in{} M_\sigma} E_u$///, TT" 
     such that ",TEX ///$E^{\rho}(i) = \sum_{(u,v_\rho) \leq i} E_u$///, TT" for every ray ",TEX ///$\rho \in{} \sigma$///, TT" and 
     every ",TEX ///$i \in{} \mathbb{Z}$///,".",
               
       
     PARA{}, TEX ///"In contrast to the implementation of Kaneyama's description this one 
     works for every toric variety $X$ i.e., there are no restrictions on the fan $\Sigma$. 
     For each ray $\rho$ of the fan $\Sigma$ 
     there are two matrices comprising the necessary filtration data. The first one is  an invertible matrix $A(\rho) \in{} $ 
     GL("///,TT "k",",",TO QQ,TEX ///") whose columns contain a basis of the vector space $E_0$ which is associated to the 
     filtration corresponding to the ray $\rho$. The second one is a ",TT "1 x k"," integer matrix, the so 
     called filtration matrix. It determines at which step an element of the basis given in the first matrix actually contributes to 
     a certain subspace in the filtration, i.e., if the j-th entry of the filtration matrix is i then the j-th basis vector appears at the 
     i-th step in the filtration. Hence $E^{\rho}(i)$ is generated by all basis vectors listed
     in $A(\rho)$ whose corresponding entry in the filtration matrix is less or equal to $E_0$."///, 
          
     
     PARA{}, TEX ///"To link up to the description of Kaneyama we will also discuss the example of the cotangent bundle $\mathbf{\Omega}_X$
     of $X = \mathbb{P}^2$. Recall that $X$ can be given by the complete 
     fan with rays $\rho_1 = (1,0)$, $\rho_2 = (0,1)$, and $\rho_3 = (-1,-1)$. There are three maximal 
     cones, namely $\sigma_1$ spanned by $\rho_1,\rho_2$,  $\sigma_2$ spanned by 
     $\rho_2,\rho_3$, and $\sigma_3$ spanned by $\rho_3,\rho_1$.
     Each of them corresponds to a torus invariant affine chart $U_{\sigma_i}$. It follows that the $k[\sigma_1^v \cap M]$-module 
     $\Gamma(U_{\sigma_1},\Omega_X)$ is generated by $dx := d(x^{[1,0]})$, and $dy := d(x^{[0,1]})$, 
     and analogously for the remaining charts. We now fix a basis of $\Omega_0$ by evaluating the sections $dx,dy$ 
     at the unit $t_0$. This gives rise to filtrations $\Omega^\rho(i)$. We only consider the example $\rho = \rho_3$.
     The filtrations for the two other rays can be found by analogous calculations. 
     Now, $k[U_{\rho_3}] = k[x^{-1},x^{-1}y,xy^{-1}]$. Then, $\Gamma(U_{\rho_3},\Omega_X)$ is generated as a 
     $k[U_{\rho_3}]$-module by $-x^{-2}dx, -x^{-2}ydx + x^{-1}dy$. Thus, 
     $\Gamma(U_{\rho_3},\Omega_X)_{[1,0]} = 0$, $\Gamma(U_{\rho_3},\Omega_X)_{[0,0]}$ is generated by 
     $xy^{-1}(-x^{-2}ydx + x^{-1}dy)$, and $\Gamma(U_{\rho_3},\Omega_X)_{[-1,0]}$ 
     is two-dimensional. Since $[1,0], [0,0]$, and $[-1,0]$ pair with $v_{\rho_3}=(-1,-1)$
     to respectively $-1, 0$, and $1$, the filtration $\Omega^{\rho_3}(i)$ jumps at 
     $1$ and $0$ with corresponding basis vectors $(0,-1)$ and $(-1,1)$. 
     Since $\Omega_X$ already is a vector bundle we do not have to check the compatibility conditions."///,
               
         
     PARA{},"An instance of class ToricVectorBundleKlyachko, when displayed or printed, gives an overview 
     of the characteristics of the bundle:",     
     
     EXAMPLE {
	  " E = cotangentBundle(projectiveSpaceFan 2) "
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,". The data described above are stored in a single hash table. 
              In the example from above, the keys are the rays of the fan, and each of them comes with a base matrix and a filtration matrix:",
     
     EXAMPLE {
	  " details E"
	  },
     
     SeeAlso => {ToricVectorBundleKaneyama,
	  ToricVectorBundle}
     
     }

document {
     Key => {addBaseChange, (addBaseChange,ToricVectorBundleKaneyama,List)},
     Headline => "changing the transition matrices of a toric vector bundle",
     Usage => " F = addBaseChange(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama,
	  "L" => List  => {"with matrices over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "F" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, TT "addBaseChange"," replaces the transition matrices in ",TT "E"," by the matrices in 
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be in GL(",TEX///$k$///,",",TO ZZ,") or 
     GL(",TEX///$k$///,",",TO QQ,"), where ",TEX///$k$///," is the rank of the vector bundle ",TT "T",". 
     The list has to contain one matrix for each maximal dimensional cone of the underlying fan over 
     which ",TT "E"," is defined. The fan can be recovered with ",TO (fan,ToricVectorBundle),". 
     The vector bundle already has a list of pairs ",TEX///$(i,j)$///," denoting the codim 1 intersections 
     of two maximal cones with ",TEX///$i<j$///," and they are ordered in lexicographic order. The matrices 
     will be assigned to the pairs ",TEX///$(i,j)$///," in that order. To see which codimension 1 cone 
     corresponds to the pair ",TEX///$(i,j)$///," use ",TO (details,ToricVectorBundle),". The 
     matrix ",TEX///$A$///," assigned to ",TEX///$(i,j)$///," denotes the transition 
     ",TEX///$(e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)*A$///,". The matrices need not satisfy the regularity 
     or the cocycle condition. These can be checked with ",TO regCheck," and ",TO cocycleCheck,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E",
	  " F = addBaseChange(E,{matrix{{1,2},{0,1}},matrix{{1,0},{3,1}},matrix{{1,-2},{0,1}},matrix{{1,0},{-3,1}}})",
	  " details F",
	  " cocycleCheck F"
	  },
     
     SeeAlso => {addDegrees,regCheck,cocycleCheck}
     
     }

document {
     Key => {addBase, (addBase,ToricVectorBundleKlyachko,List)},
     Headline => "changing the basis matrices of a toric vector bundle in Klyachko's description",
     Usage => "F = addBase(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "L" => List => {"with matrices over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "F" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "addBase"," replaces the basis matrices in ",TT "E"," by the matrices in 
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be in GL(",TEX///$k,R$///,"), 
     where ",TEX///$k$///," is the rank of the vector bundle ",TT "E"," and ",TEX///$R$///," 
     is ",TO ZZ," or ",TO QQ,". The list has to contain one matrix for each ray of the 
     underlying fan over which ",TT "E"," is defined. Note that in ",TT "E"," the rays are 
     already sorted and that the basis matrices in ",TT "L"," will be assigned to the 
     rays in that order. To see the order use ",TO (rays,ToricVectorBundle),".",
     
     PARA{}, "The matrices need not satisfy the compatibility condition. This can 
     be checked with ",TO isVectorBundle,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " details E",
	  " F = addBase(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details F",
	  " isVectorBundle F"
	  },
     
     SeeAlso => {base,addFiltration,isVectorBundle}
     
     }

document {
     Key => {addDegrees, (addDegrees,ToricVectorBundleKaneyama,List)},
     Headline => "changing the degrees of a toric vector bundle",
     Usage => " F = addDegrees(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "F" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, TT "addDegrees"," replaces the degree matrices in ",TT "E"," by the matrices in 
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be ",TEX///$n$///," by ",TEX///$k$///," 
     matrices over ",TO ZZ,", where ",TEX///$k$///," is the rank of the vector bundle ",TT "E"," 
     and ",TEX///$n$///," is the dimension of the underlying toric variety. The list has to contain one 
     matrix for each maximal dimensional cone of the underlying fan over which ",TT "E"," is defined. 
     Note that in ",TT "E"," the top dimensional cones are already sorted and that the degree matrices 
     in ",TT "L"," will be assigned to the cones in that order. To find out the order use ",TO (maxCones,ToricVectorBundle),". 
     The matrices need not satisfy the regularity condition. This can be checked with ",TO regCheck,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E",
	  " F = addDegrees(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details F",
	  " regCheck F"
	  },
     
     SeeAlso => {addBaseChange,regCheck,cocycleCheck}
     
     }

document {
     Key => {addFiltration, (addFiltration,ToricVectorBundleKlyachko,List)},
     Headline => "changing the filtration matrices of a toric vector bundle in Klyachko's description",
     Usage => "F = addFiltration(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "F" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "addFiltration"," replaces the filtration matrices in ",TT "E"," by the matrices in 
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be ",TEX///$1$///," by ",TEX///$k$///," 
     matrices over ",TO ZZ,", where ",TEX///$k$///," is the rank of the vector bundle ",TT "E",". The 
     list has to contain one matrix for each ray of the underlying fan over which ",TT "E"," is defined. 
     Note that in ",TT "E"," the rays are already sorted and that the filtration matrices in ",TT "L"," 
     will be assigned to the rays in that order. To see the order, use ",TO (rays,ToricVectorBundle),".",
     
     PARA{}, TEX ///"The filtration on the vector bundle over a ray is given by the filtration matrix for this 
     ray in the following way: The first index $j$, such that the $i$-th basis 
     vector in the basis over this ray appears in the $j$-th step of the filtration, is the 
     $i$-th entry of the filtration matrix. OR in other words, the $j$-th step 
     step in the filtration is given by all columns of the basis matrix for which the corresponding entry 
     in the filtration matrix is less or equal to $j$."///,
     
     PARA{}, "The matrices need not satisfy the compatability condition. This can be checked 
     with ",TO isVectorBundle,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " details E",
	  " F = addFiltration(E,{matrix{{1,3}},matrix{{-1,3}},matrix{{2,-3}},matrix{{0,-1}}})",
	  " details F",
	  " isVectorBundle F"
	  },
     
     PARA{}, "This means that for example over the first ray the first basis vector of the filtration of ",TT "F"," 
     appears at the filtration step 1 and the second at 3.",
     
     SeeAlso => {filtration,addBase,isVectorBundle}
     
     }

document {
     Key => {areIsomorphic, (areIsomorphic,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => "checks if two vector bundles are isomorphic",
     Usage => " b = areIsomorphic(E,F)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "F" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "b" => Boolean => {"whether ", TT "E", " and ", TT "F", " are isomorphic"}
	  },     
     
     PARA{}, TT "E"," and ",TT "F"," must be vector bundles over the same fan. Two equivariant vector 
     bundles in Klyachko's description are isomorphic if there exists a simultaneous isomorphism for 
     the filtered vector spaces of all rays. The method then returns whether the bundles are 
     isomorphic.",     
     
     EXAMPLE {
	  " HF = hirzebruchFan 2",
	  " E = exteriorPower(2, cotangentBundle HF)",
	  " F = weilToCartier({-1,-1,-1,-1},HF)",
	  " areIsomorphic(E,F)"
	  },
     
     PARA{}, "To obtain the isomorphism, if two bundles are isomorphic use ",TO isomorphism,".",
     
     SeeAlso => {isomorphism,base,filtration,details}
     
     }

document {
     Key => {base, (base,ToricVectorBundleKlyachko)},
     Headline => " the basis matrices for the rays",
     Usage => " b = base E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "b" => HashTable
	  },
     
     PARA{}, "The basis of a toric vector bundle in Klyachko's description is given for each ray as a square 
     matrix of rank ",TEX///$k$///," of the bundle. The output is a ",TO HashTable," where the keys are the 
     rays of the fan given as one column matrices over ",TO ZZ,", and for each ray a ",TEX///$k$///," 
     by ",TEX///$k$///," matrix over ",TO QQ," and ",TEX///$k$///," is the rank of the bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " base E"
	  },
     
     SeeAlso => {addBase,filtration,isVectorBundle}
     
     }

document {
     Key => {cartierIndex, (cartierIndex,List,Fan)},
     Headline => " the Cartier index of a Weil divisor",
     Usage => " N = cartierIndex(L,F)",
     Inputs => {
	  "L" => List,
	  "F" => Fan => {"a pure and full dimensional fan"}
	  },
     Outputs => {
	  "N" => ZZ
	  },
     
     PARA{}, TT "L"," must be a list of weights, 
     exactly one for each ray of the fan. Then the Cartier index is the smallest strictly positive 
     natural number ",TEX ///$N$///," such that ",TEX ///$N$///," times the Weil divisor is Cartier. 
     If the Weil divisor defined by these weights is not ",TO QQ,"-Cartier, then ",TEX ///$N$///," would 
     be infinity. In this case ",TT "cartierIndex"," returns an error. Otherwise it returns ",TEX ///$N$///,".",
     
     EXAMPLE {
	  " F = fan posHull matrix {{1,5},{5,1}}",
	  " L = {2,2}",
	  " cartierIndex(L,F)"
	  },
     
     PARA{}, "If we change the Weil divisor we get a different Cartier index:",
     
     EXAMPLE {
	  " L = {3,3}",
	  " cartierIndex(L,F)"
	  },
     
     SeeAlso => {weilToCartier}
     
     }

document {
     Key => {charts, (charts,ToricVectorBundle)},
     Headline => " the number of maximal affine charts",
     Usage => " n = charts E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "n" => ZZ
	  },
     
     PARA{}, "The function ",TT "charts"," returns the number of maximal cones in the underlying 
     fan, i.e., the number of affine charts.",
     
     EXAMPLE {
	  " E = cotangentBundle pp1ProductFan 3",
	  " charts E"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",(fan,ToricVectorBundle)}
     
     }

document {
     Key => {cocycleCheck, (cocycleCheck,ToricVectorBundleKaneyama)},
     Headline => " checks if a toric vector bundle fulfills the cocycle condition",
     Usage => " b = cocycleCheck E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama 
	  },
     Outputs => {
	  "b" => Boolean => {"whether ", TT "E", " satisfies the cocyle condition"}
	  },
     
     PARA{}, "The transition matrices in ",TT "E"," define an equivariant toric vector bundle 
     if they satisfy the cocycle condition. I.e. in this implementation of complete fans this 
     means that for every codimension 2 cone of the fan the cycle of transition matrices of 
     codimension 1 cones containing the codimension 2 cone gives the identity when multiplied.",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E",
	  " A = matrix{{1,2},{0,1}};",
	  " B = matrix{{1,0},{3,1}};",
	  " C = matrix{{1,-2},{0,1}};",
	  " E1 = addBaseChange(E,{A,B,C,matrix{{1,0},{0,1}}})",
	  " cocycleCheck E1",
	  " D = inverse(B)*A*C",
	  " E1 = addBaseChange(E,{A,B,C,D})",
	  " cocycleCheck E1"
	  },
     
     SeeAlso => {addBaseChange,addDegrees,regCheck}
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle)},
     Headline => " the i-th cohomology group of a toric vector bundle",
     Usage => " c = HH^i E ",
     Inputs => {
	  "i" => ZZ,
	  "T" => ToricVectorBundle
	  },
     Outputs => {
	  "c" => Module
	  },
     
     PARA{}, "Computes the ",TEX///$i$///,"-th cohomology group of the toric vector bundle ",TEX///$E$///,". The 
     output is the ",TEX///$i$///,"-th cohomology group as a multigraded module. For this, it computes the 
     set of all degrees that can give non-zero cohomology (see ",TO deltaE,"). This set is finite if the 
     underlying toric variety is complete. If the toric variety is not complete then an error is returned.",
     
     PARA{},"The computation of the cohomology groups for a toric vector bundle given in terms of Kaneyama is done by 
     the usual Cech cohomology complex, again separately for every degree ",TEX///$u \in{} M$///,".",
     
     PARA{}, "If the option ",TT "Degree => 1"," is used then it displays the number of degrees for which 
     it computes the cohomology. ",TEX///$i$///," must be between ",TEX///$0$///," and the dimension of 
     the underlying toric variety.",
     
     EXAMPLE {
	  " E = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " HH^0 E",
	  " HH^0 (E,Degree => 1)"
	  },
     
     PARA{}, TEX ///"In case the toric vector bundle $E$ is given in Klyachko's description, there is a 
     special exact sequence of finite dimensional vector spaces for every weight $u \in{} M$ 
     whose cohomology groups in degree $i$ are isomorphic to $H^i(X,E)$. This exact
     sequence can be found in the Klyachko's paper listed on the main page of the documentation."///,
     
     PARA{}, "If the option ",TT "Degree => 1"," is used then it displays the number of degrees for which 
     it computes the cohomology. ",TEX///$i$///," must be between ",TEX///$0$///," and the dimension of 
     the underlying toric variety.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH^0 E",
	  " HH^0 (E,Degree => 1)"
	  },
     
     SeeAlso => {(ring,ToricVectorBundle),
	  deltaE,
	  (cohomology,ZZ,ToricVectorBundle,Matrix),
	  (cohomology,ZZ,ToricVectorBundle,List),
	  (hh,ZZ,ToricVectorBundle),
	  eulerChi}
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle,List)},
     Headline => " the i-th cohomology of a toric vector bundle for a given list of degrees",
     Usage => " c = HH_i^E L",
     Inputs => {
	  "i" => ZZ,
	  "E" => ToricVectorBundle,
	  "L" => List => {" containing weights of the form, one column matrix over ",TO ZZ}
	  },
     Outputs => {
	  "c" => List
	  },
     
     PARA{}, TEX ///"Computes the $i$-th cohomology of the toric vector bundle $E$ for a 
     given list of degrees. For this $i$ must be between $0$ and the rank of 
     the vector bundle. The entries of the list "///,TT "L"," must be one column matrices each defining a point 
     in the lattice of the fan over which ",TEX///$E$///," is defined",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH_0^E {matrix{{1},{0}},matrix{{-1},{0}}}"
	  },
     
     SeeAlso => {(ring,ToricVectorBundle),
	  deltaE,
	  (cohomology,ZZ,ToricVectorBundle),
	  (cohomology,ZZ,ToricVectorBundle,Matrix),
	  (hh,ZZ,ToricVectorBundle),
	  eulerChi}
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle,Matrix)},
     Headline => " the i-th cohomology of a toric vector bundle in a given degree",
     Usage => " c = HH_i^E u ",
     Inputs => {
	  "i" => ZZ,
	  "E" => ToricVectorBundle,
	  "u" => Matrix => {"over ",TO ZZ," with just one column, giving a weight in the lattice"}
	  },
     Outputs => {
	  "c" => Module
	  },
     
     PARA{}, TEX ///"Computes the $i$-th cohomology group of the toric vector bundle $E$ of 
     degree $u$ where $u$ must be a one-column matrix giving a point in the 
     lattice of the fan over which $E$ is defined and $i$ must be between $0$ 
     and the dimension of the underlying toric variety."///,
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH^0 (E,matrix{{1},{0}})"
	  },
     
     SeeAlso => {(ring,ToricVectorBundle),
	  deltaE,
	  (cohomology,ZZ,ToricVectorBundle),
	  (cohomology,ZZ,ToricVectorBundle,List),
	  (hh,ZZ,ToricVectorBundle),
	  eulerChi}
          
     }

document {
     Key => {(coker,ToricVectorBundleKlyachko,Matrix)},
     Headline => " the cokernel of a morphism to a vector bundle",
     Usage => " E1 = coker(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the target space is the space 
     of the bundle, i.e., the matrix must have ",TEX///$k$///," rows if the bundle has rank 
     ",TEX///$k$///,". Then the new bundle is given on each ray ",TEX///$\rho$///," by the following 
     filtration of coker(E,M)",TEX///${}^\rho = ( E^{\rho} ) / $///,"im(M) :",
     
     PARA{}, "coker(E,M)",TEX///${}^\rho(i) := E^{\rho}(i) / ( E^{\rho}(i) \cap $///," im(M) ).",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0},{0,1},{1,0},{0,1/1}}",
	  " E1 = coker(E,M)",
	  " details E1"
	  },
     
     SeeAlso => {(image,ToricVectorBundleKlyachko,Matrix),
	  (ker,ToricVectorBundleKlyachko,Matrix)}
     
     }

document {
     Key => {cotangentBundle, (cotangentBundle,Fan)},
     Headline => " the cotangent bundle on a toric variety",
     Usage => " E = cotangentBundle F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama," or ",ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "If the fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the 
     cotangent bundle of the toric variety given by ",TT "F",". If no further options are given then the 
     resulting bundle will be in Klyachko's description:",
     
     EXAMPLE {
	  " F = projectiveSpaceFan 2",
	  " E = tangentBundle F",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description:",
     
     EXAMPLE {
	  " F = projectiveSpaceFan 2",
	  " E = tangentBundle(F,\"Type\" => \"Kaneyama\")",
	  " details E"
	  },
     
     SeeAlso => {tangentBundle}
     
     }

document {
     Key => {deltaE, (deltaE,ToricVectorBundle)},
     Headline => " the polytope of possible degrees that give non zero cohomology",
     Usage => " P = deltaE E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "For a toric vector bundle over a complete toric variety there is a finite set of 
     degrees ",TEX///$u$///," such that the degree ",TEX///$u$///," part of the cohomology of the 
     vector bundle is non-zero. This function computes a polytope ",TEX///$\Delta_E$///,", such 
     that these degrees are contained in this polytope. If the underlying toric variety is not 
     complete then an error is returned.",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2, \"Type\" => \"Kaneyama\")",
	  " P = deltaE E",
	  " vertices P",
	  " E1 = tangentBundle projectiveSpaceFan 2",
	  " P1 = deltaE E1",
	  " vertices P1"
	  },
     
     SeeAlso => {eulerChi,
	  (cohomology,ZZ,ToricVectorBundle),
	  (hh,ZZ,ToricVectorBundle)}
     
     }

document {
     Key => {details, (details,ToricVectorBundle)},
     Headline => " the details of a toric vector bundle",
     Usage => " ht = details E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "ht" => Sequence => {" or ",TO HashTable," if the bundle is in Klyachko's description"}
	  },
     
     PARA{}, "For a toric vector bundle in Kaneyama's description, the sequence ",TT "ht"," contains 
     a hash table that assigns to each maximal cone ",TEX///$\sigma$///," of the underlying fan 
     its matrix of rays and its matrix of degrees, and a hash table giving a transition matrix for 
     every pair of maximal cones that intersect in a codimension 1 face.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  "details E"
	  },
     
     PARA{}, "For a toric vector bundle in Klyachko's description, the hash table ",TT "ht"," contains 
     the rays of the underlying fan and for each ray the basis of the bundle over this ray and the 
     filtration matrix.",
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  "details E"
	  }
     
     }

document {
     Key => (dual,ToricVectorBundle),
     Headline => " the dual bundle of a toric vector bundle",
     Usage => " Ed = dual E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "Ed" => ToricVectorBundle
	  },
     
     PARA{}, TT "dual"," computes the dual vector bundle of a toric vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " Ed = dual E",
	  " details Ed",
	  " Ed == cotangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")"
	  },
     
     EXAMPLE {
	  " E = tangentBundle projectiveSpaceFan 2",
	  " Ed = dual E",
	  " details Ed",
	  " Ed == cotangentBundle projectiveSpaceFan 2"
	  },
     
     SeeAlso => {tangentBundle,cotangentBundle}
     
     }

document {
     Key => {eulerChi, (eulerChi,ToricVectorBundle), (eulerChi,Matrix,ToricVectorBundle)},
     Headline => " the Euler characteristic of a toric vector bundle",
     Usage => " i = eulerChi E \neulerChi(u,E)",
     Inputs => {
	  "E" => ToricVectorBundle,
	  "u" => Matrix => {"with just one column over ",TO ZZ," representing a degree vector"}
	  },
     Outputs => {
	  "i" => ZZ
	  },
     
     PARA{}, "This function computes the Euler characteristic of a vector bundle if only the bundle is given 
     to the function. For this it first computes the set of all degrees that give non-zero cohomology 
     (see ",TO deltaE,") and then computes the Euler characteristic for each these degrees. If the underlying 
     variety is not complete then this set may not be finite. Thus, for a non-complete toric variety an error 
     is returned.",
     
     PARA{},  "If in addition a one-column matrix over ",TO ZZ,", representing a degree vector ",TT "u",", is given, it 
     computes the Euler characteristic of the degree ",TT "u","-part of the vector bundle ",TT "E",". For this the variety 
     need not be complete.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " u = matrix {{0},{0}}",
	  " eulerChi(u,E)",
	  " eulerChi E"
	  },
     
     EXAMPLE {
	  " E = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " u = matrix {{0},{0}}",
	  " eulerChi(u,E)",
	  " eulerChi E"
	  },
     
     SeeAlso => {deltaE,
	  (cohomology,ZZ,ToricVectorBundle),
	  (hh,ZZ,ToricVectorBundle)}
     
     }

document {
     Key => {existsDecomposition, (existsDecomposition,ToricVectorBundleKlyachko,List)},
     Headline => " checks if a list of matrices of weight vectors for each maximal cone admits a decomposition",
     Usage => " b = existsDecomposition(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "L" => List
	  },
     Outputs => {
	  "b" => Boolean => {"whether there exists a decomposition"}
	  },
     
     PARA{}, "The list ",TT "L"," must have one entry for each maximal cone ",TEX///$\sigma$///," in the 
     underlying fan ",TEX///$\Sigma$///," of ",TT "E",". If the rank of the bundle is ",TEX///$k$///," and 
     the ambient dimension of the variety is ",TEX///$n$///," then each entry must either be 
     an ",TEX///$n$///," by ",TEX///$k$///," matrix over ",TO ZZ," or a list of these. Then it checks for 
     each maximal cone in the fan (given in the order of ",TO (maxCones,ToricVectorBundle),") if 
     for any of the matrices in the corresponding entry in ",TT "L"," these weight vectors admit a decomposition 
     of the bundle into torus eigenspaces. See ",
     HREF("http://math.stanford.edu/~sampayne/", "Sam Payne's"), " ", EM "Moduli of toric vector bundles", ", 
     Compositio Math. 144, 2008. Lemma 3.5.",
     
     PARA{}, "One can for example use the output of the function ",TO findWeights,".",
     
     EXAMPLE{
	  " E = tangentBundle projectiveSpaceFan 3",
	  " L = findWeights E",
	  " existsDecomposition(E,L)"
	  },
     
     PARA{}, "Note that the data given in the description of ",TT "E"," defines an equivariant vector bundle 
     on the toric variety exactly if there exists a set of weight vectors for each maximal cone that admits a 
     decomposition. The function ",TO isVectorBundle," uses this.",
     
     SeeAlso => {findWeights,isVectorBundle,(maxCones,ToricVectorBundle)}
     
     }

document {
     Key => {(exteriorPower,ZZ,ToricVectorBundle)},
     Headline => " the 'l'-th exterior power of a toric vector bundle",
     Usage => " Ee = exteriorPower(l,E)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "Ee" => ToricVectorBundle
	  },
     
     PARA{}, TT "exteriorPower"," computes the ",TT "l","-th exterior power of a toric vector bundle in each 
     description. The resulting bundle will be given in the same description as the original bundle. 
     ",TT "l"," must be strictly positive and at most equal the rank of the bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " details E",
	  " Ee = exteriorPower(2,E)",
	  " details Ee"
	  },
     
     SeeAlso => {(symbol ++,ToricVectorBundle,ToricVectorBundle),
	  (tensor,ToricVectorBundle,ToricVectorBundle),
	  (symmetricPower,ZZ,ToricVectorBundle)}
     
     }

document {
     Key => {(fan,ToricVectorBundle)},
     Headline => " the underlying fan of a toric vector bundle",
     Usage => " F = fan E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Returns the fan of the underlying toric variety. This is an object of the package OldPolyhedra. 
     See also ",TO "OldPolyhedra::Fan",".",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " F = fan E",
	  " rays F"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",charts,(maxCones,ToricVectorBundle)}
     
     }

document {
     Key => {filtration, (filtration,ToricVectorBundleKlyachko)},
     Headline => " the filtration matrices of the vector bundle",
     Usage => " f = filtration E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "f" => HashTable
	  },
     
     PARA{}, "For each ray of the fan there is a filtration matrix. If the bundle has rank ",TEX///$k$///," 
     then this is a one row matrix over ",TO ZZ," with ",TEX///$k$///," entries. This defines the 
     filtration on the corresponding base matrix (see ",TO base,") such that the ",TEX///$j$///,"-th 
     filtration is generated by all columns of the base matrix for which the entry in the same column of the 
     filtration matrix is less or equal to ",TEX///$j$///,".",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " filtration E"
	  },
     
     PARA{}, "So in this example for each ray the first column of the basis appears at -1 and the second at 0.",
     
     SeeAlso => {addFiltration,base,isVectorBundle}
     
     }

document {
     Key => {findWeights, (findWeights,ToricVectorBundleKlyachko)},
     Headline => " finds the possible weight vectors for the maximal cones",
     Usage => " L = findWeights E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "The list ",TT "L"," contains a list for each maximal cone ",TEX///$\sigma$///," of the 
     underlying fan. For each maximal cone ",TEX///$\sigma$///," this list contains all matrices of 
     possible weight vectors, that induce the filtrations on the rays of this cone (modulo permutations, 
     but yet not all permutations). This means that for one of these matrices ",TEX///$M$///," multiplied 
     with the matrix ",TEX///$R$///," of rays of this cone (the rays are the rows) gives the matrix of 
     filtrations of these rays (where for each filtration the entries may be permuted).",
     
     EXAMPLE{
	  " E = tangentBundle projectiveSpaceFan 3",
	  " findWeights E"
	  },
     
     SeeAlso => {filtration,existsDecomposition,isVectorBundle}
     
     }

document {
     Key => {(ring,ToricVectorBundle)},
     Headline => " the graded ring of the bundle",
     Usage => " R = ring E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "R" => Ring
	  },
     
     PARA{}, "For a vector bundle in Kaneyama's description the graded ring is ",TO QQ," with degree 
     space the lattice of the underlying fan.",
     
     EXAMPLE{
	  " E = tangentBundle(projectiveSpaceFan 3,\"Type\" => \"Kaneyama\")",
	  " ring E"
	  },
     
     PARA{}, "For a vector bundle in Klyachko's description the graded ring is ",TO QQ," with degree 
     space the lattice of the underlying fan.",
     
     EXAMPLE{
	  " E = toricVectorBundle(1,projectiveSpaceFan 2, toList(3:matrix{{1/2}}),toList(3:matrix{{-1}}))",
	  " ring E"
	  },
     
     SeeAlso => {(cohomology,ZZ,ToricVectorBundle),
	  (cohomology,ZZ,ToricVectorBundle,Matrix),
	  (cohomology,ZZ,ToricVectorBundle,List)}
     
     }

document {
     Key => {(hh,ZZ,ToricVectorBundle)},
     Headline => " the rank of the i-th cohomology group of a toric vector bundle",
     Usage => " d = hh^i E \nd = hh^i (E,u)",
     Inputs => {
	  "i" => ZZ,
	  "E" => {"and an optional ",TT "u",", ",ofClass Matrix," over ",TO ZZ,", giving a point in the lattice of the fan"}
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, TT "hh^i"," computes the rank of the ",TEX///$i$///,"-th cohomology group. If no 
     further argument is given then it returns the rank of the complete cohomology group. For this 
     it computes the set of all degrees that can give non-zero cohomology (see ",TO deltaE,"). This 
     set is finite if the underlying toric variety is complete. If the toric variety is not complete, 
     then an error is returned.",
     
     PARA{}, "If in addition a one column matrix ",TEX///$u$///," over ",TO ZZ," is given it returns the 
     rank of the degree ",TEX///$u$///," part of the cohomology group. For this the variety need not be 
     complete.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " u = matrix{{0},{0}}",
	  " hh^0 (E,u)",
	  " hh^0 E"
	  },
     
     SeeAlso => {(cohomology,ZZ,ToricVectorBundle),
	  (cohomology,ZZ,ToricVectorBundle,Matrix),
	  (cohomology,ZZ,ToricVectorBundle,List),
	  deltaE}
     
     }

document {
     Key => {hirzebruchFan,(hirzebruchFan,ZZ)},
     Headline => "the fan of the n-th Hirzebruch surface",
     Usage => " F = hirzebruchFan n",
     Inputs => {
	  "n" => ZZ => {"positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of the ",TEX///$n$///,"-th Hirzebruch surface.",
     
     EXAMPLE {
	  " F = hirzebruchFan 3",
	  " rays F"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",
	  "OldPolyhedra::hirzebruch",
	  pp1ProductFan,
	  projectiveSpaceFan}
     
     }

document {
     Key => {(image,ToricVectorBundleKlyachko,Matrix)},
     Headline => " the image of a vector bundle under a morphism",
     Usage => " E1 = image(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the source space is the space 
     of the bundle, i.e., the matrix must have ",TEX///$k$///," columns if the bundle has rank ",TEX///$k$///,". 
     Then the new bundle is given on each ray ",TEX///$\rho$///," by the following filtration of 
     image",TEX///$(E,M)^\rho := M(E^\rho)$///," :",
     
     PARA{}, "image",TEX///$(E,M)^\rho(i) := M(E^\rho(i))$///,".",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0,1,0},{0,1,0,1/1}}",
	  " E1 = image(E,M)",
	  " details E1"
	  },
     
     SeeAlso => {(coker,ToricVectorBundleKlyachko,Matrix),
	  (ker,ToricVectorBundleKlyachko,Matrix)}
     
     }

document {
     Key => {isGeneral, (isGeneral,ToricVectorBundleKlyachko)},
     Headline => " checks whether a toric vector bundle is general",
     Usage => " b = isGeneral E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "b" => Boolean => {"whether ", TT "E", " is general"}
	  },
     
     PARA{}, TEX ///"A toric vector bundle in Klyachko's description is general if for every maximal cone 
     $\Sigma$ in the fan the following condition holds: Let $\rho_1,...,\rho_l$ be 
     the rays of $\sigma$. Then for every choice of filtration steps $i_1,...,i_l$ 
     for each ray, i.e., choose an integer for each ray where the filtration enlarges, the equation"///,
     
     PARA{}, "codim ",TEX///$(\cap E^{\rho_j} ( i_j )) = min \{ \sum ($///,"codim ",TEX///$E^{\rho_j} ( i_j )),rank E \}$///,
     
     PARA{}, "holds.",
     
     EXAMPLE {
	  " E = cotangentBundle hirzebruchFan 2",
	  " isGeneral E"
	  },
     
     SeeAlso => {filtration,base,randomDeformation}
     
     }

document {
     Key => {isomorphism, (isomorphism,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " the isomorphism if the two bundles are isomorphic",
     Usage => " M = isomorphism(E,F)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "F" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "M" => Matrix => {"over the ring over which the two bundles are defined"}
	  },
     
     PARA{}, "Two equivariant vector bundles in Klyachko's description are isomorphic if there exists a 
     simultaneous isomorphism for the filtered vector spaces of all rays. If the two bundles are isomorphic 
     (see ",TO areIsomorphic,") this function returns the isomorphism. For this, the two bundles must be 
     defined over the same fan.",
     
     EXAMPLE{
	  " HF = hirzebruchFan 2",
	  " E = exteriorPower(2, cotangentBundle HF)",
	  " F = weilToCartier({-1,-1,-1,-1},HF)",
	  " M = isomorphism(E,F)"
	  },
     
     SeeAlso => {areIsomorphic,base,filtration,details}
     
     }

document {
     Key => {isVectorBundle, (isVectorBundle,ToricVectorBundle)},
     Headline => " checks if the data does in fact define an equivariant toric vector bundle",
     Usage => " b = isVectorBundle E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "b" => Boolean => {"whether ",TT "E"," defines a toric vector bundle"}
	  },
     
     PARA{}, "If ",TT "E"," is in Klyachko's description then the data in ",TT "E"," defines an equivariant 
     toric vector on the toric variety if and only if for each maximal cone exists a decomposition into 
     torus eigenspaces of the bundle. See ",HREF("http://math.stanford.edu/~sampayne/", "Sam Payne's"), " ", 
     EM "Moduli of toric vector bundles", ", Compositio Math. 144, 2008. Section 2.3. This uses the two 
     functions ",TO findWeights," and ",TO existsDecomposition,".",
     
     EXAMPLE{
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " E = addBase(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " isVectorBundle E",
	  " F = toricVectorBundle(1,normalFan crossPolytope 3)",
	  " F = addFiltration(F,apply({2,1,1,2,2,1,1,2}, i -> matrix {{i}}))",
	  " isVectorBundle F"
	  },
     
     PARA{}, "If ",TT "E"," is in Kaneyama's description then data in ",TT "E"," defines an equivariant 
     toric vector bundle on the toric variety if and only if it satisfies the regularity and the cocycle 
     condition (See ",TO cocycleCheck," and ",TO regCheck,").",
     
     EXAMPLE{
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " isVectorBundle E",
	  " E = addBaseChange(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " isVectorBundle E"
	  },
     
     SeeAlso => {findWeights,
	  existsDecomposition,
	  addBase,
	  addFiltration,
	  cocycleCheck,
	  regCheck,
	  addBaseChange,
	  addDegrees,
	  details} 
     
     }

document {
     Key => {(ker,ToricVectorBundleKlyachko,Matrix)},
     Headline => " the kernel of a morphism to a vector bundle",
     Usage => " E1 = ker(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the source space is the space 
     of the bundle, i.e., the matrix must have ",TEX///$k$///," columns if the bundle has rank ",TEX///$k$///,". 
     Then the new bundle is given on each ray ",TEX///$\rho$///," by the following filtration of 
     ker",TEX///$(E,M)^\rho := $///," ker",TEX///$(M) \cap (E^\rho)$///," :",
     
     PARA{}, "ker",TEX///$(E,M)^\rho(i) := $///," ker",TEX///$(M) \cap E^\rho(i)$///,".",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0,1,0},{0,1,0,1/1}}",
	  " E1 = ker(E,M)",
	  " details E1"
	  },
     
     SeeAlso => {(coker,ToricVectorBundleKlyachko,Matrix),
	  (image,ToricVectorBundleKlyachko,Matrix)}
     
     }

document {
     Key => {(maxCones,ToricVectorBundle)},
     Headline => " the list of maximal cones of the underlying fan",
     Usage => " L = maxCones E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "L" => List => {" of cones"}
	  },
     
     PARA{}, "Returns the list of maximal cones of the underlying fan. These are the cones that 
     generate the fan, i.e., are not a face of another. See ",TO "OldPolyhedra::Fan",", ",TO "OldPolyhedra::maxCones"," 
     and ",TO "OldPolyhedra::Cone",".",
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  " L = maxCones E",
	  " apply(L,rays)"
	  },
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " L = maxCones E",
	  " apply(L,rays)"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",
	  "OldPolyhedra::maxCones",
	  "OldPolyhedra::Cone",
	  charts,
	  (fan,ToricVectorBundle),
	  (rays,ToricVectorBundle)}
     
     }

document {
     Key => (net,ToricVectorBundleKaneyama),
     Headline => "displays characteristics of a toric vector bundle",
     Usage => " net E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, "Displays an overview of the properties of a toric vector bundle, 
     the dimension of the variety, the number of affine charts, and the rank of the 
     vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\");",
	  " net E"
	  },
     
     SeeAlso => {(net,ToricVectorBundleKlyachko),
	  details}
     
     }

document {
     Key => (net,ToricVectorBundleKlyachko),
     Headline => "displays characteristics of a toric vector bundle in Klyachko's description",
     Usage => " net E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, "Displays an overview of the properties of a toric vector bundle, 
     the dimension of the variety, the number of affine charts, the number of rays of the fan, 
     and the rank of the vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3;",
	  " net E"
	  },
     
     SeeAlso => {(net,ToricVectorBundleKaneyama),
	  details}
     
     }

document {
     Key => {pp1ProductFan,(pp1ProductFan,ZZ)},
     Headline => "the fan of n products of PP^1",
     Usage => " F = pp1ProductFan n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of the product of ",TEX///$n$///," projective one-spaces. This is 
     the same as the normal fan of the ",TEX///$n$///," dimensional hypercube.",
     
     EXAMPLE {
	  " F = pp1ProductFan 2",
	  " apply(maxCones F, rays)"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",
	  hirzebruchFan,
	  projectiveSpaceFan}
     
     }

document {
     Key => {projectiveSpaceFan,(projectiveSpaceFan,ZZ)},
     Headline => "the fan of projective n space",
     Usage => " F = projectiveSpaceFan n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of projective ",TEX///$n$///,"-space.",
     
     EXAMPLE {
	  " F = projectiveSpaceFan 2",
	  " apply(maxCones F, rays)"
	  },
     
     SeeAlso => {"OldPolyhedra::Fan",
	  hirzebruchFan,
	  pp1ProductFan}
     
     }

document {
     Key => {randomDeformation, (randomDeformation,ToricVectorBundleKlyachko,ZZ), (randomDeformation,ToricVectorBundleKlyachko,ZZ,ZZ)},
     Headline => " a random deformation of a given toric vector bundle",
     Usage => " E1 = randomDeformation(E,h) \nE1 = randomDeformation(E,l,h)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "l" => ZZ => {"less than ",TT "h"},
	  "h" => ZZ
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TEX ///"For a bundle of rank $k$ the 
     function "///,TT "randomDeformation",TEX ///" replaces 
     each base matrix by a random $k$ by $k$ matrix with entries
     between $l$ and $h$. For this $h$ must be greater 
     than $l$. If $l$ is not given then the random entries are between $0$ 
     and $h$ and then $h$ must be strictly positive."///,
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  " details E",
	  " E1 = randomDeformation(E,-2,6)",
	  " details E1"
	  },
     
     SeeAlso => {base,filtration,details,isGeneral}
     }

document {
     Key => {(rank,ToricVectorBundle)},
     Headline => " the rank of the vector bundle",
     Usage => " k = rank E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "k" => ZZ
	  },
     
     PARA{}, "Returns the rank ",TEX///$k$///," of the toric vector bundle in Kaneyama's description.",
     
     EXAMPLE {
	  " E = tangentBundle projectiveSpaceFan 3",
	  " rank E"
	  },
     
     SeeAlso => {(rays,ToricVectorBundle),
	  (fan,ToricVectorBundle),
	  charts}
     
     }

document {
     Key => {(rays,ToricVectorBundle)},
     Headline => " the rays of the underlying fan",
     Usage => " L = rays E",
     Inputs => {
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "Returns the rays of the fan of the underlying toric variety as a list. Each ray is 
     given as a one column matrix.",
     
     EXAMPLE {
	  " E = cotangentBundle projectiveSpaceFan 2",
	  " rays E"
	  },
     
     SeeAlso => {(rank,ToricVectorBundle),
	  (fan,ToricVectorBundle),
	  charts}
     
     }

document {
     Key => {regCheck, (regCheck,ToricVectorBundleKaneyama)},
     Headline => " checking the regularity condition for a toric vector bundle",
     Usage => " b = regCheck E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     Outputs => {
	  "b" => Boolean => {"whether ", TT "E", " satisfies the regularity condition"}
	  },
     
     PARA{}, TEX ///"For a toric vector bundle in Kaneyama's description, the regularity condition means that 
     for every pair of maximal cones $\sigma_1,\sigma_2$intersecting in a common 
     codimension-one face, the two sets of degrees $d_1,d_2$ and the transition 
     matrix $A_{1,2}$ fulfil the regularity condition. I.e. for every 
     $i$ and $j$ we have that either the $(i,j)$ entry of the 
     matrix $A_{1,2}$ is $0$ or the difference of 
     the $i$-th degree vector of $d_1$ of $\sigma_1$ and 
     the $j$-th degree vector of $d_2$ of $\sigma_2$ is in the 
     dual cone of the intersection of $\sigma_1$ and $\sigma_2$."///,
     
     PARA{}, "Note that this is only necessary for toric vector bundles generated 'by hand' 
     using ",TO addBaseChange," and ",TO addDegrees,", since bundles generated for example by 
     ",TO tangentBundle," satisfy the condition automatically.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " regCheck E"
	  },
     
     SeeAlso => {addBaseChange,addDegrees,cocycleCheck,isVectorBundle}
     
     }

document {
     Key => {(symbol **,ToricVectorBundle,ToricVectorBundle)},
     Headline => " the tensor product of two toric vector bundles",
     Usage => " E = E1 ** E2",
     Inputs => {
	  "E1" => ToricVectorBundle,
	  "E2" => ToricVectorBundle
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "If ",TEX///$E_1$///," and ",TEX///$E_2$///," are defined over the same fan and in the same description, 
     then ",TT "tensor"," computes the tensor product of the two vector bundles in this description",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = E1 ** E2",
	  " details E"
	  },
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E2 = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E = E1 ** E2",
	  " details E"
	  },
     
     SeeAlso => {(tensor,ToricVectorBundle,ToricVectorBundle),
	  (symbol ++,ToricVectorBundle,ToricVectorBundle),
	  (exteriorPower,ZZ,ToricVectorBundle),
	  (symmetricPower,ZZ,ToricVectorBundle)}
     
     }

document {
     Key => {(symbol ++,ToricVectorBundle,ToricVectorBundle)},
     Headline => " the direct sum of two toric vector bundles",
     Usage => " E = E1 ++ E2",
     Inputs => {
	  "E1" => ToricVectorBundle,
	  "E2" => ToricVectorBundle
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "If ",TEX///$E_1$///," and ",TEX///$E_2$///," are defined over the same fan, then ",TT "directSum"," computes 
     the direct sum of the two vector bundles. The bundles must both be given in the same description 
     and the resulting bundle will be in this description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = E1 ++ E2",
	  " details E"
	  },
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E2 = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E = E1 ++ E2",
	  " details E"
	  },
     
     SeeAlso => {(symbol **,ToricVectorBundle,ToricVectorBundle),
	  (tensor,ToricVectorBundle,ToricVectorBundle),
	  (exteriorPower,ZZ,ToricVectorBundle),
	  (symmetricPower,ZZ,ToricVectorBundle)}
          
     }

document {
     Key => {(symbol ==,ToricVectorBundle,ToricVectorBundle)},
     Headline => " checks for equality",
     Usage => " b = E1 == E2",
     Inputs => {
	  "E1" => ToricVectorBundle,
	  "E2" => ToricVectorBundle
	  },
     Outputs => {
	  "E" => Boolean => {" whether the two toric vector bundles are equal"}
	  },
     
     PARA{}, "Checks if two toric vector bundles are identical. This only works if they are given in the same 
     description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E1 == E2"
	  },
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E2 = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " E1 == E2"
	  },
     
     SeeAlso => {areIsomorphic,
	  isomorphism}
     
     }

document {
     Key => {(symmetricPower,ZZ,ToricVectorBundle)},
     Headline => " the 'l'-th symmetric power of a toric vector bundle",
     Usage => " Es = symmetricPower(l,E)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "E" => ToricVectorBundle
	  },
     Outputs => {
	  "Es" => ToricVectorBundle
	  },
     
     PARA{}, TT "symmetricPower"," computes the ",TEX///$l$///,"-th symmetric power of a toric vector bundle 
     in each description. The resulting bundle will be given in the same description as the original 
     bundle. ",TEX///$l$///," must be strictly positive.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " details E",
	  " Es = symmetricPower(2,E)",
	  " details Es"
	  },
     
     SeeAlso => {(exteriorPower,ZZ,ToricVectorBundle),
	  (symbol ++,ToricVectorBundle,ToricVectorBundle),
	  (tensor,ToricVectorBundle,ToricVectorBundle)}
          
     }

document {
     Key => {tangentBundle, (tangentBundle,Fan)},
     Headline => " the tangent bundle on a toric variety",
     Usage => " E = tangentBundle F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "If the fan ",TT "F"," is pure, of full dimension and smooth, then the function 
     generates the tangent bundle of the toric variety given by ",TT "F",". If no further 
     options are given then the resulting bundle will be in Klyachko's description:",
     
     EXAMPLE {
	  " F = pp1ProductFan 2",
	  " E = tangentBundle F",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description:",
     
     EXAMPLE {
	  " F = pp1ProductFan 2",
	  " E = tangentBundle(F,\"Type\" => \"Kaneyama\")",
	  " details E"
	  },
     
     SeeAlso => {cotangentBundle}
     
     }

document {
     Key => {(tensor,ToricVectorBundle,ToricVectorBundle)},
     Headline => " the tensor product of two toric vector bundles",
     Usage => " E = tensor(E1,E2)",
     Inputs => {
	  "E1" => ToricVectorBundle,
	  "E2" => ToricVectorBundle
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "If ",TT "E1"," and ",TT "E2"," are defined over the same fan and are in the same description, 
     then ",TT "tensor"," computes the tensor product of the two vector bundles in this description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = tensor(E1,E2)",
	  " details E"
	  },
     
     SeeAlso => {(symbol **,ToricVectorBundle,ToricVectorBundle),
	  (symbol ++,ToricVectorBundle,ToricVectorBundle),
	  (exteriorPower,ZZ,ToricVectorBundle),
	  (symmetricPower,ZZ,ToricVectorBundle)}
          
     }

document {
     Key => {toricVectorBundle, (toricVectorBundle,ZZ,Fan)},
     Headline => " the trivial bundle of rank 'k' for a given fan",
     Usage => " E = toricVectorBundle(k,F)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"}
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed Fan ",TT "F"," the 
     function ",TT "toricVectorBundle"," generates the trivial toric vector bundle of rank ",TT "k",".",
     
     PARA{}, TEX ///"If no further options are given then the resulting bundle will be in Klyachko's description:
     The basis assigned to every ray is the standard basis of $\mathbb{Q}^k$ and the filtration 
     is given by $0$ for all $i<0$ and $\mathbb{Q}^k$ 
     for $i>=0$."///,
     
     EXAMPLE{
	  " E = toricVectorBundle(2,projectiveSpaceFan 2)",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description: The degree vectors of this bundle are all zero vectors and the transition matrices 
     are all the identity. Note that for Kaneyama's description only complete, pointed fans are implemented and 
     thus a non complete fan will produce an error.",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E"
	  },
     
     SeeAlso => {addBaseChange,
	  addDegrees,
	  addBase,
	  addFiltration,
	  details,
	  regCheck,
	  cocycleCheck,
	  isVectorBundle}
     
     } 

document {
     Key => (toricVectorBundle,ZZ,Fan,List,List),
     Headline => " a toric vector bundle of rank 'k' with given filtrations or degrees",
     Usage => " E = toricVectorBundle(k,F,L1,L2)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"},
	  "L1" => List,
	  "L2" => List
	  },
     Outputs => {
	  "E" => ToricVectorBundle
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed fan ",TT "F"," the 
     function ",TT "toricVectorBundle"," generates the toric vector bundle of rank ",TT "k"," given 
     by the data in the two lists ",TT "L1"," and ",TT "L2",".",
     
     PARA{}, "If no further options are given then the resulting bundle will be in Klyachko's description: 
     The first list ",TT "L1"," will give the basis matrices and the second list ",TT "L2"," will give 
     the filtration matrices. Then the resulting vector bundle will have these basis and filtration 
     matrices. The number of matrices in ",TT "L1"," must match the number of rays of the fan and they 
     must be in GL(",TT "k",",",TEX///$R$///,") for ",TEX///$R$///," being ",TO ZZ," or ",TO QQ,". They 
     will be assigned to the rays in the order they appear in ",TT "rays F",". The number of  matrices 
     in ",TT "L2"," must also match the number of rays, and they must be ",TEX///$1$///," times ",TT "k"," 
     matrices over ",TO ZZ,". The assignment order is the same as for the basis matrices.",
     
     PARA{}, "Note that the basis and filtration matrices that are given to the function need not 
     satisfy the compatability condition. This can by checked by using ",TO regCheck,".",
     
     EXAMPLE {
	  " L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}",
	  " L2 = {matrix {{-1,0}},matrix{{-2,-1}},matrix{{0,1}}}",
	  " E = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2)",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be 
     in Kaneyama's description; Note that this is only implemented for complete, pointed fans: The first 
     list ",TT "L1"," will give the degree matrices and the second list ",TT "L2"," will give the 
     transition matrices. The number of matrices in ",TT "L1"," must match the number of maximal cones of 
     the fan and they must be ",TEX///$n$///," times ",TT "k"," matrices over ",TO ZZ,". They will be 
     assigned to the cones in the order they appear in ",TT "maxCones F",". The number of matrices 
     in ",TT "L2"," must match the number of pairs of maximal cones that intersect in a common 
     codimension-one face and must all be in GL(",TT "k",",",TO QQ,"). They will be assigned to the 
     pairs ",TEX///$(i,j)$///," in lexicographic order.",
     
     PARA{}, "Note that the degrees and transition matrices that are given to the function need not 
     satisfy the regularity or the cocycle condition. These can be checked by 
     using ",TO regCheck," and ",TO cocycleCheck,".",
     
      EXAMPLE {
	  " L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}",
	  " L2 = {matrix {{-1,0},{0,-1}},matrix{{0,1},{1,0}},matrix{{0,-1},{-1,0}}}",
	  " E = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2,\"Type\" => \"Kaneyama\")",
	  " details E"
	  },
     
     SeeAlso => {addBaseChange,
	  addDegrees,
	  addBase,
	  addFiltration,
	  details,
	  regCheck,
	  cocycleCheck,
	  isVectorBundle}
     
     }

document {
     Key => {twist, (twist,ToricVectorBundleKlyachko,List)},
     Headline => " twists a toric vector bundle with a line bundle",
     Usage => " E1 = twist(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "L" => List
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "twist"," takes a toric vector bundle ",TEX///$E$///," in Klyachko's description and a list 
     of integers ",TT "L",". The list must contain one entry for each ray of the underlying fan. Then 
     it computes the twist of the vector bundle by the line bundle given by these integers 
     (see ",TO weilToCartier,").",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " L = {1,-2,3,-4}",
	  " E1 = twist(E,L)",
	  " details E1"
	  },
     
     SeeAlso => {weilToCartier,cartierIndex,details}
     
     }

document {
     Key => {weilToCartier, (weilToCartier,List,Fan)},
     Headline => " the line bundle given by a Cartier divisor",
     Usage => " E = weilToCartier(L,F)",
     Inputs => {
	  "L" => List,
	  "F" => Fan => {"a pure and full dimensional fan"}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, TT "L"," must a list of weights, exactly one for each ray of the fan. Then the list of weights 
     for each ray describes a Weil divisor on the toric variety. If the Weil divisor defined by these weights 
     defines in fact a Cartier divisor, then ",TT "weilToCartier"," computes the toric vector bundle associated 
     to the Cartier divisor.",
     
     PARA{}, "If no further options are given then the resulting bundle will be in Klyachko's description:",
     
     EXAMPLE {
	  " F = hirzebruchFan 3",
	  " E =weilToCartier({1,-3,4,-2},F)",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description:",
     
     EXAMPLE {
	  " F = hirzebruchFan 3",
	  " E =weilToCartier({1,-3,4,-2},F,\"Type\" => \"Kaneyama\")",
	  " details E"
	  },
     
     SeeAlso => {cartierIndex}
     
     }



---------------------------------------
-- TESTS
---------------------------------------


-- Test 0
-- Checking toricVectorBundle for Kaneyama type
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^2,QQ^2,1),(0,2) => map(QQ^2,QQ^2,1),(1,3) => map(QQ^2,QQ^2,1),(2,3) => map(QQ^2,QQ^2,1)})
assert(T#"degreeTable" === hashTable apply(maxCones pp1ProductFan 2, C -> C => map(ZZ^2,ZZ^2,0)))
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}
L2 = {matrix {{-1,0},{0,-1}},matrix{{0,1},{1,0}},matrix{{0,-1},{-1,0}}}
T = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix {{-1/1,0},{0,-1}},(0,2) => matrix{{0/1,1},{1,0}},(1,2) => matrix{{0/1,-1},{-1,0}}})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,-1},{0,-1}} => matrix{{-1,0},{-1,1}}, posHull matrix {{1,0},{0,1}} => matrix{{0,1},{1,0}},posHull matrix {{-1,0},{-1,1}} => matrix{{1,0},{0,1}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
///

-- Test 1
-- Checking toricVectorBundle for Klyachko type
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2);
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{0}} => map(ZZ^1,ZZ^2,0),matrix{{0},{-1}} => map(ZZ^1,ZZ^2,0),matrix{{1},{0}} => map(ZZ^1,ZZ^2,0),matrix{{0},{1}} => map(ZZ^1,ZZ^2,0)})
assert(T#"baseTable" === hashTable{matrix{{-1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{-1}} => map(QQ^2,QQ^2,1),matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => map(QQ^2,QQ^2,1)})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}
L2 = {matrix {{-1,0}},matrix{{-2,-1}},matrix{{0,1}}}
T = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2)
assert(T#"ring" === ZZ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1}} => matrix{{-1,0}},matrix{{0},{1}} => matrix{{-2,-1}},matrix{{1},{0}} => matrix{{0,1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1}} => matrix {{1,0},{0,1}},matrix{{0},{1}} => matrix{{0,1},{1,0}},matrix{{1},{0}} => matrix{{-1,0},{-1,1}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
///

-- Test 2
-- Checking addBaseChange and cocycleCheck
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
T1 = addBaseChange(T,{matrix{{1,2},{0,1}},matrix{{1,0},{3,1}},matrix{{1,-2},{0,1}},matrix{{1,0},{-3,1}}})
assert cocycleCheck T1
T1 = addBaseChange(T,{matrix{{1,2},{0,1}},matrix{{1,0},{3,1}},matrix{{1,-2},{0,1}},matrix{{1,0},{-2,1}}})
assert not cocycleCheck T1
///

-- Test 3
-- Checking regCheck
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
assert regCheck T
T1 = addDegrees(T,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})
assert not regCheck T1
T1 = addDegrees(T,{matrix{{-1,0},{-3,-1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{1,2},{3,1}}})
assert regCheck T1
///

-- Test 4
-- Checking tangentBundle for Kaneyama
TEST ///
T = tangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}}), (0,2) => map(QQ^2,QQ^2,{{-1, 0}, {0, 1}}), (1,3) => map(QQ^2,QQ^2,{{-1, 0}, {0, 1}}), (2,3) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}})})
assert(T#"degreeTable" === hashTable {posHull matrix {{-1,0},{0,1}} => matrix{{1,0},{0,-1}},posHull matrix {{-1,0},{0,-1}} => matrix{{1,0},{0,1}},posHull matrix {{1,0},{0,1}} => matrix{{-1,0},{0,-1}}, posHull matrix{{1,0},{0,-1}} => matrix{{-1,0},{0,1}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
T = tangentBundle(projectiveSpaceFan 3, "Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^3,QQ^3,{{1, -1, 0}, {0, -1, 0}, {0, -1, 1}}), (0,2) => map(QQ^3,QQ^3,{{-1, 0, 0}, {-1, 1, 0}, {-1, 0, 1}}), (1,2) => map(QQ^3,QQ^3,{{-1, 1, 0}, {-1, 0, 0}, {-1, 0, 1}}), (0,3) => map(QQ^3,QQ^3,{{1, 0, -1}, {0, 0, -1}, {0, 1, -1}}), (1,3) => map(QQ^3,QQ^3,{{1, 0, -1}, {0, 1, -1}, {0, 0, -1}}), (2,3) => map(QQ^3,QQ^3,{{0, 0, -1}, {1, 0, -1}, {0, 1, -1}})})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,0,0},{0,-1,0},{0,0,-1}},posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0},{0,0,-1},{1,1,1}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{1,1,1},{0,-1,0},{0,0,-1}}, posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0},{1,1,1},{0,0,-1}}})
assert(rank T == 3)
assert(T#"dimension of the variety" == 3)
///

-- Test 5
-- Checking tangentBundle for Klyachko
TEST ///
T = tangentBundle hirzebruchFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{-1,0}},matrix{{0},{-1}} => matrix{{-1,0}},matrix{{1},{0}} => matrix{{-1,0}},matrix{{0},{1}} => matrix{{-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{-1,1/3},{3,0}},matrix{{0},{-1}} => matrix{{0_QQ,1},{-1,0}},matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => matrix{{0_QQ,1},{1,0}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
T = tangentBundle pp1ProductFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{1},{0}} => matrix{{-1,0,0}}, matrix{{-1},{0},{0}} => matrix{{-1,0,0}},matrix{{1},{0},{0}} => matrix{{-1,0,0}}, matrix{{0},{0},{-1}} => matrix{{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{-1,0,0}}, matrix{{0},{-1},{0}} => matrix{{-1,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}, matrix{{-1},{0},{0}} => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{0},{-1}} => matrix{{0_QQ,1,0},{0,0,1},{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{0_QQ,1,0},{-1,0,0},{0,0,1}}})
assert(rank T == 3)
///

-- Test 6
-- Checking cotangentBundle for Kaneyama
TEST ///
T = cotangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}}), (0,2) => map(QQ^2,QQ^2,{{-1, 3}, {0, 1}}), (1,3) => map(QQ^2,QQ^2,{{-1, -3}, {0, 1}}), (2,3) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}})})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0},{0,-1}} => matrix{{1,0},{0,-1}},posHull matrix {{1,0},{0,1}} => matrix{{1,0},{0,1}},posHull matrix {{0,-1},{1,3}} => matrix{{-1,3},{0,1}}, posHull matrix{{0,-1},{-1,3}} => matrix{{-1,-3},{0,-1}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
T = cotangentBundle(pp1ProductFan 3, "Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(2,6) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (4,5) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (4,6) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (3,7) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (5,7) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (6,7) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (0,1) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (0,2) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (1,3) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (0,4) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (2,3) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (1,5) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{1,0,0},{0,1,0},{0,0,1}},posHull matrix{{-1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,0,0},{0,1,0},{0,0,1}},posHull matrix{{1,0,0},{0,-1,0},{0,0,1}} => matrix{{1,0,0},{0,-1,0},{0,0,1}},posHull matrix{{1,0,0},{0,1,0},{0,0,-1}} => matrix{{1,0,0},{0,1,0},{0,0,-1}},posHull matrix{{-1,0,0},{0,-1,0},{0,0,1}} => matrix{{-1,0,0},{0,-1,0},{0,0,1}},posHull matrix{{-1,0,0},{0,1,0},{0,0,-1}} => matrix{{-1,0,0},{0,1,0},{0,0,-1}},posHull matrix{{1,0,0},{0,-1,0},{0,0,-1}} => matrix{{1,0,0},{0,-1,0},{0,0,-1}},posHull matrix{{-1,0,0},{0,-1,0},{0,0,-1}} => matrix{{-1,0,0},{0,-1,0},{0,0,-1}}})
assert(rank T == 3)
assert(T#"dimension of the variety" == 3)
///

-- Test 7
-- Checking cotangentBundle for Klyachko
TEST ///
T = cotangentBundle hirzebruchFan 2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{1,0}},matrix{{0},{-1}} => matrix{{1,0}},matrix{{1},{0}} => matrix{{1,0}},matrix{{0},{1}} => matrix{{1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{0,2},{1/2,1}},matrix{{0},{-1}} => matrix{{0_QQ,1},{-1,0}},matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => matrix{{0_QQ,1},{1,0}}})
assert(rank T == 2)
assert(T#"dimension of the variety" == 2)
T = cotangentBundle pp1ProductFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{1},{0}} => matrix{{1,0,0}}, matrix{{-1},{0},{0}} => matrix{{1,0,0}},matrix{{1},{0},{0}} => matrix{{1,0,0}}, matrix{{0},{0},{-1}} => matrix{{1,0,0}}, matrix{{0},{0},{1}} => matrix{{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{1,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}, matrix{{-1},{0},{0}} => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{0},{-1}} => matrix{{0_QQ,1,0},{0,0,1},{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{0_QQ,1,0},{-1,0,0},{0,0,1}}})
assert(rank T == 3)
///

-- Test 8
-- Checking isVectorBundle
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2)
T1 = addBase(T,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})
assert isVectorBundle T1
T = toricVectorBundle(1,normalFan crossPolytope 3)
L = apply({2,1,1,2,2,1,1,2}, i -> matrix {{i}});
T = addFiltration(T,L)
assert not isVectorBundle T
///

-- Test 9
-- Checking deltaE for Kaneyama
TEST ///
T = toricVectorBundle(3,projectiveSpaceFan 2,"Type" => "Kaneyama")
assert(deltaE T == convexHull matrix{{0},{0}})
T = tangentBundle(projectiveSpaceFan 2,"Type" => "Kaneyama")
assert(deltaE T == convexHull matrix {{-1,1,0,1,0,-1},{0,0,-1,-1,1,1}})
T = cotangentBundle(pp1ProductFan 3,"Type" => "Kaneyama")
assert(deltaE T == convexHull matrix {{-1,1,0,0,0,0},{0,0,-1,1,0,0},{0,0,0,0,-1,1}})
///

-- Test 10
-- Checking deltaE for Klyachko
TEST ///
T = toricVectorBundle(3,projectiveSpaceFan 2)
assert(deltaE T == convexHull matrix{{0},{0}})
T = tangentBundle projectiveSpaceFan 2
assert(deltaE T == convexHull matrix {{-1, 1, 0, 1, 0, -1}, {0, 0, -1, -1, 1, 1}})
T = cotangentBundle pp1ProductFan 3
assert(deltaE T == convexHull matrix {{-1,1,0,0,0,0},{0,0,-1,1,0,0},{0,0,0,0,-1,1}})
///

-- Test 11
-- Checking cohomology for Kaneyama
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
assert(cohomology(0,T,matrix{{0},{0}}) == (ring T)^{{0,0},{0,0}})
assert(cohomology(0,T) == (ring T)^{{0,0},{0,0}})
assert(cohomology(1,T) == (ring T)^0)
assert(cohomology(2,T) == (ring T)^0)
T1 = tangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
assert(cohomology(0,T1,matrix{{0},{0}}) == (ring T1)^{{0,0},{0,0}})
assert(cohomology(0,T1,matrix{{1},{1}}) == (ring T1)^0)
assert(cohomology(0,T1) == (ring T1)^{{1,0},{0,1},{0,0},{0,0},{0,-1},{-1,0}})
assert(cohomology(1,T1) == (ring T1)^0)
assert(cohomology(2,T1) == (ring T1)^0)
T = tangentBundle(hirzebruchFan 3 * projectiveSpaceFan 1,"Type" => "Kaneyama")
assert(cohomology(0,T,{matrix {{2},{1},{0}}, matrix{{3},{1},{0}}}) == {(ring T)^{{-2,-1,0}},(ring T)^{{-3,-1,0}}})
assert(cohomology(1,T,{matrix {{-2},{-1},{0}}, matrix{{-1},{-1},{0}}}) == {(ring T)^{{2, 1, 0}},(ring T)^{{1, 1, 0}}})
assert(cohomology(2,T,matrix{{0},{0},{0}}) == (ring T)^0)
assert(cohomology(3,T,matrix{{0},{0},{0}}) == (ring T)^0)
///

-- Test 12
-- Checking cohomology for Klyachko
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2)
assert(cohomology(0,T,matrix{{0},{0}}) == (ring T)^{{0,0},{0,0}})
assert(cohomology(0,T) == (ring T)^{{0,0},{0,0}})
assert(cohomology(1,T) == (ring T)^0)
assert(cohomology(2,T) == (ring T)^0)
T1 = tangentBundle pp1ProductFan 2
assert(cohomology(0,T1,matrix{{0},{0}}) == (ring T1)^{{0,0},{0,0}})
assert(cohomology(0,T1,matrix{{1},{1}}) == (ring T1)^0)
assert(cohomology(0,T1) == (ring T1)^{{1,0},{0,1},{0,0},{0,0},{0,-1},{-1,0}})
assert(cohomology(1,T1) == (ring T1)^0)
assert(cohomology(2,T1) == (ring T1)^0)
T = tangentBundle(hirzebruchFan 3 * projectiveSpaceFan 1)
assert(cohomology(0,T) == (ring T)^{{0, 0, 1}, {1, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {-1, 0, 0}, {0, 0, -1}, {0, -1, 0}, {-1, -1, 0}, {-2, -1, 0}, {-3, -1, 0}})
assert(cohomology(1,T) == (ring T)^{{2, 1, 0}, {1, 1, 0}})
assert(cohomology(2,T) == (ring T)^0)
assert(cohomology(3,T) == (ring T)^0)
///

-- Test 13
-- Checking weilToCartier
TEST ///
T = weilToCartier({1,4,3,2},projectiveSpaceFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^1,QQ^1,1),(0,2) => map(QQ^1,QQ^1,1),(0,3) => map(QQ^1,QQ^1,1),(1,2) => map(QQ^1,QQ^1,1),(1,3) => map(QQ^1,QQ^1,1),(2,3) => map(QQ^1,QQ^1,1)})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{-2},{-3},{6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{8},{-3},{-4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{-2},{7},{-4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{-2},{-3},{-4}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 3)
T = weilToCartier({1,4,3,2},projectiveSpaceFan 3)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1}},matrix{{0},{0},{1}} => matrix{{-4}},matrix{{0},{1},{0}} => matrix{{-3}}, matrix{{1},{0},{0}} => matrix{{-2}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1_QQ}},matrix{{0},{0},{1}} => matrix{{1_QQ}},matrix{{0},{1},{0}} => matrix{{1_QQ}}, matrix{{1},{0},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 3)
///

-- Test 14
-- Checking directSum for Kaneyama
TEST ///
T1 = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T2 = weilToCartier({1,7,5,3},projectiveSpaceFan 3,"Type" => "Kaneyama")
T = T1 ++ T2
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix{{1_QQ,-1,0,0},{0,-1,0,0},{0,-1,1,0},{0,0,0,1}}, (0,2) => matrix{{-1_QQ,0,0,0},{-1,1,0,0},{-1,0,1,0},{0,0,0,1}}, (1,2) => matrix{{-1_QQ,1,0,0},{-1,0,0,0},{-1,0,1,0},{0,0,0,1}}, (0,3) => matrix{{1_QQ,0,-1,0},{0,0,-1,0},{0,1,-1,0},{0,0,0,1}}, (1,3) => matrix{{1_QQ,0,-1,0},{0,1,-1,0},{0,0,-1,0},{0,0,0,1}}, (2,3) => matrix{{0_QQ,0,-1,0},{1,0,-1,0},{0,1,-1,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0,-3},{0,0,-1,-5},{1,1,1,9}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{1,1,1,13},{0,-1,0,-5},{0,0,-1,-7}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0,-3},{1,1,1,11},{0,0,-1,-7}},posHull map(ZZ^3,ZZ^3,1) => matrix{{-1,0,0,-3},{0,-1,0,-5},{0,0,-1,-7}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 3)
assert(T == directSum {T1,T2})
T1 = cotangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
T2 = tangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
T = T1 ++ T2
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}, (0,2) => matrix{{-1_QQ,3,0,0},{0,1,0,0},{0,0,-1,0},{0,0,3,1}}, (1,3) => matrix{{-1_QQ,-3,0,0},{0,1,0,0},{0,0,-1,0},{0,0,-3,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0},{0,-1}} => matrix{{1,0,-1,0},{0,-1,0,1}}, posHull matrix{{0,-1},{1,3}} => matrix{{-1,3,1,-3},{0,1,0,-1}}, posHull matrix{{1,0},{0,1}} => matrix{{1,0,-1,0},{0,1,0,-1}}, posHull matrix{{0,-1},{-1,3}} => matrix {{-1,-3,1,3},{0,-1,0,1}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
///

--Test 15
-- Checking directSum for Klyachko
TEST ///
T1 = tangentBundle projectiveSpaceFan 3
T2 = weilToCartier({1,7,5,3},projectiveSpaceFan 3)
T = T1 ++ T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1,0,0,-1}},matrix{{0},{0},{1}} => matrix{{-1,0,0,-7}},matrix{{0},{1},{0}} => matrix{{-1,0,0,-5}}, matrix{{1},{0},{0}} => matrix{{-1,0,0,-3}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0,0},{-1,1,0,0},{-1,0,1,0},{0,0,0,1}},matrix{{0},{0},{1}} => matrix{{0_QQ,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0,0},{1,0,0,0},{0,0,1,0},{0,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 3)
assert(T == directSum {T1,T2})
T1 = cotangentBundle hirzebruchFan 3
T2 = tangentBundle hirzebruchFan 3
T = T1 ++ T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{1,0,-1,0}},matrix{{0},{-1}} => matrix{{1,0,-1,0}},matrix{{0},{1}} => matrix{{1,0,-1,0}}, matrix{{1},{0}} => matrix{{1,0,-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{0,3,0,0},{1/3,1,0,0},{0,0,-1,1/3},{0,0,3,0}},matrix{{0},{-1}} => matrix{{0_QQ,1,0,0},{-1,0,0,0},{0,0,0,1},{0,0,-1,0}},matrix{{0},{1}} => matrix{{0,1_QQ,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,0}}, matrix{{1},{0}} => map(QQ^4,QQ^4,1)})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
///

-- Test 16
-- Checking dual for Kaneyama
TEST ///
T = dual weilToCartier({1,4,3,2},projectiveSpaceFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ}},(0,2) => matrix{{1_QQ}}, (0,3) => matrix{{1_QQ}}, (1,2) => matrix{{1_QQ}},(1,3) => matrix{{1_QQ}},(2,3) => matrix{{1_QQ}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{2},{3},{-6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{-8},{3},{4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{2},{-7},{4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{2},{3},{4}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 3)
T1 = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = dual(T1 ++ T)
assert(T#"baseChangeTable" === hashTable{(0,2) => matrix{{-1_QQ,-1,-1,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},(0,1) => matrix{{1_QQ,0,0,0},{-1,-1,-1,0},{0,0,1,0},{0,0,0,1}}, (0,3) => matrix{{1_QQ,0,0,0},{-1,-1,-1,0},{0,1,0,0},{0,0,0,1}}, (1,2) => matrix{{0_QQ,1,0,0},{-1,-1,-1,0},{0,0,1,0},{0,0,0,1}},(1,3) => matrix{{1_QQ,0,0,0},{0,1,0,0},{-1,-1,-1,0},{0,0,0,1}},(2,3) => matrix{{-1_QQ,-1,-1,0},{1,0,0,0},{0,1,0,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,1,0,-2},{0,0,1,-3},{-1,-1,-1,6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{-1,-1,-1,8},{0,1,0,-3},{0,0,1,-4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,1,0,-2},{-1,-1,-1,7},{0,0,1,-4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{1,0,0,-2},{0,1,0,-3},{0,0,1,-4}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 3)
///

-- Test 17
-- Checking dual for Klyachko
TEST ///
T = dual weilToCartier({1,4,3,2},projectiveSpaceFan 3)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1}},matrix{{0},{0},{1}} => matrix{{4}},matrix{{0},{1},{0}} => matrix{{3}}, matrix{{1},{0},{0}} => matrix{{2}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1_QQ}},matrix{{0},{0},{1}} => matrix{{1_QQ}},matrix{{0},{1},{0}} => matrix{{1_QQ}}, matrix{{1},{0},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 3)
T1 = tangentBundle projectiveSpaceFan 3
T = dual(T1 ++ T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1,0,0,-1}},matrix{{0},{0},{1}} => matrix{{1,0,0,-1}},matrix{{0},{1},{0}} => matrix{{1,0,0,-1}}, matrix{{1},{0},{0}} => matrix{{1,0,0,-1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,-1,-1,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},matrix{{0},{0},{1}} => matrix{{0_QQ,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0,0},{1,0,0,0},{0,0,1,0},{0,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 3)
///

-- Test 18
-- Checking tensor for Kaneyama
TEST ///
T1 = tangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
T2 = cotangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
T = T1 ** T2
assert(T#"baseChangeTable" === hashTable{(0,2) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}},(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}, (1,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{-1,0},{0,1}} => matrix{{0,-1,1,0},{0,-1,1,0}},posHull matrix {{-1,0},{0,-1}} => matrix{{0,-1,1,0},{0,1,-1,0}},posHull matrix {{1,0},{0,-1}} => matrix{{0,1,-1,0},{0,1,-1,0}},posHull map(ZZ^2,ZZ^2,1) => matrix{{0,1,-1,0},{0,-1,1,0}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
T1 = tangentBundle(hirzebruchFan 2,"Type" => "Kaneyama")
T2 = weilToCartier({5,1,7,3},hirzebruchFan 2,"Type" => "Kaneyama")
T2 = T2 ++ T2
T = T1 ** T2
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}},(0,2) => matrix{{-1_QQ,0,0,0},{2,1,0,0},{0,0,-1,0},{0,0,2,1}}, (1,3) => matrix{{-1_QQ,0,0,0},{-2,1,0,0},{0,0,-1,0},{0,0,-2,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0},{0,1}} => matrix{{-4,-3,-4,-3},{-1,-2,-1,-2}},posHull matrix {{1,0},{0,-1}} => matrix{{-4,-3,-4,-3},{5,6,5,6}},posHull matrix {{0,-1},{-1,2}} => matrix{{18,19,18,19},{5,6,5,6}},posHull matrix{{0,-1},{1,2}} => matrix{{6,3,6,3},{-1,-2,-1,-2}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
///

-- Test 19
-- Checking tensor for Klyachko
TEST ///
T1 = tangentBundle pp1ProductFan 2
T2 = cotangentBundle pp1ProductFan 2
T = T1 ** T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{0}} => matrix{{0,-1,1,0}},matrix{{0},{-1}} => matrix{{0,-1,1,0}},matrix{{0},{1}} => matrix{{0,-1,1,0}}, matrix{{1},{0}} => matrix{{0,-1,1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{0}} => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}},matrix {{0},{-1}} => matrix{{0_QQ,0,0,1},{0,0,-1,0},{0,-1,0,0},{1,0,0,0}},matrix {{0},{1}} => matrix{{0_QQ,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}},matrix{{1},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
T1 = tangentBundle hirzebruchFan 2
T2 = weilToCartier({5,1,7,3},hirzebruchFan 2)
T2 = T2 ++ T2
T = T1 ** T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{-8,-8,-7,-7}},matrix{{0},{-1}} => matrix{{-6,-6,-5,-5}},matrix{{0},{1}} => matrix{{-2,-2,-1,-1}}, matrix{{1},{0}} => matrix{{-4,-4,-3,-3}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{-1,0,1/2,0},{0,-1,0,1/2},{2,0,0,0},{0,2,0,0}},matrix {{0},{-1}} => matrix{{0_QQ,0,1,0},{0,0,0,1},{-1,0,0,0},{0,-1,0,0}},matrix {{0},{1}} => matrix{{0_QQ,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0}},matrix{{1},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(T#"dimension of the variety" == 2)
///

-- Test 20
-- Checking symmetricPower for Kaneyama
TEST ///
T = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = symmetricPower(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ,-1,0,1,0,0},{0,-1,0,2,0,0},{0,-1,1,2,-1,0},{0,0,0,1,0,0},{0,0,0,2,-1,0},{0,0,0,1,-1,1}},(0,2) => matrix{{1_QQ,0,0,0,0,0},{2,-1,0,0,0,0},{2,0,-1,0,0,0},{1,-1,0,1,0,0},{2,-1,-1,0,1,0},{1,0,-1,0,0,1}}, (0,3) => matrix{{1_QQ,0,-1,0,0,1},{0,0,-1,0,0,2},{0,1,-1,0,-1,2},{0,0,0,0,0,1},{0,0,0,0,-1,2},{0,0,0,1,-1,1}}, (1,2) => matrix{{1_QQ,-1,0,1,0,0},{2,-1,0,0,0,0},{2,-1,-1,0,1,0},{1,0,0,0,0,0},{2,0,-1,0,0,0},{1,0,-1,0,0,1}}, (1,3) => matrix{{1_QQ,0,-1,0,0,1},{0,1,-1,0,-1,2},{0,0,-1,0,0,2},{0,0,0,1,-1,1},{0,0,0,0,-1,2},{0,0,0,0,0,1}},(2,3) => matrix{{0_QQ,0,0,0,0,1},{0,0,-1,0,0,2},{0,0,0,0,-1,2},{1,0,-1,0,0,1},{0,1,-1,0,-1,2},{0,0,0,1,-1,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0,-2,-1,0},{2,2,2,2,2,2},{0,0,-1,0,-1,-2}},posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-2,-1,-1,0,0,0},{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2}},posHull matrix{{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2},{2,2,2,2,2,2}},posHull matrix{{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{2,2,2,2,2,2},{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2}}})
assert(rank T == 6)
assert(T#"dimension of the variety" == 3)
///

-- Test 21
-- Checking symmetricPower for Klyachko
TEST ///
T = tangentBundle projectiveSpaceFan 3
T = symmetricPower(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{1},{0},{0}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{0},{1},{0}} => matrix{{-2,-1,-1,0,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,0,0,1,0,0},{0,0,0,0,1,0},{0,1,0,0,0,0},{0,0,0,0,0,1},{0,0,1,0,0,0},{1,0,0,0,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{1_QQ,0,0,0,0,0},{2,-1,0,0,0,0},{2,0,-1,0,0,0},{1,-1,0,1,0,0},{2,-1,-1,0,1,0},{1,0,-1,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}}, matrix{{0},{1},{0}} => matrix{{0_QQ,0,0,1,0,0},{0,1,0,0,0,0},{0,0,0,0,1,0},{1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,0,1}}})
assert(rank T == 6)
assert(T#"dimension of the variety" == 3)
///

-- Test 22
-- Checking exteriorPower for Kaneyama
TEST ///
T = cotangentBundle(hirzebruch 3,"Type" => "Kaneyama")
T = exteriorPower(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{-1_QQ}}, (0,2) => matrix{{-1_QQ}}, (1,3) => matrix{{-1_QQ}}, (2,3) => matrix{{-1_QQ}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,0},{0,1}} => matrix{{1},{1}},posHull matrix{{1,0},{0,-1}} => matrix{{1},{-1}},posHull matrix{{0,-1},{1,3}} => matrix {{2},{1}},posHull matrix{{0,-1},{-1,3}} => matrix {{-4},{-1}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 2)
T = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = exteriorPower(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{-1_QQ,0,0},{-1,1,-1},{0,0,-1}}, (0,2) => matrix{{-1_QQ,0,0},{0,-1,0},{1,-1,1}}, (0,3) => matrix{{0_QQ,-1,0},{1,-1,1},{0,0,1}}, (1,2) => matrix{{1_QQ,0,0},{1,-1,1},{0,-1,0}}, (1,3) => matrix{{1_QQ,-1,1},{0,-1,0},{0,0,-1}}, (2,3) => matrix{{0_QQ,1,0},{0,0,1},{1,-1,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{-1,0,-1},{2,2,2},{0,-1,-1}},posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,-1,0},{-1,0,-1},{0,-1,-1}},posHull matrix{{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{-1,0,-1},{0,-1,-1},{2,2,2}},posHull matrix{{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{2,2,2},{-1,0,-1},{0,-1,-1}}})
assert(rank T == 3)
assert(T#"dimension of the variety" == 3)
///

-- Test 23
-- Checking exteriorPower for Klyachko
TEST ///
T = cotangentBundle hirzebruch 3
T = exteriorPower(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{1}}, matrix{{0},{1}} => matrix{{1}}, matrix{{0},{-1}} => matrix{{1}}, matrix{{1},{0}} => matrix{{1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{-1_QQ}}, matrix{{0},{1}} => matrix{{-1_QQ}}, matrix{{0},{-1}} => matrix{{1_QQ}}, matrix{{1},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(T#"dimension of the variety" == 2)
T = tangentBundle projectiveSpaceFan 3
T = exteriorPower(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{-1,-1,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1,-1,0}}, matrix{{1},{0},{0}} => matrix{{-1,-1,0}}, matrix{{0},{1},{0}} => matrix{{-1,-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,0,1},{-1,0,0},{0,-1,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0},{0,-1,0},{1,-1,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{1},{0}} => matrix{{-1_QQ,0,0},{0,0,1},{0,1,0}}})
assert(rank T == 3)
assert(T#"dimension of the variety" == 3)
///

-- Test 24
-- Checking eulerChi
TEST ///
T = tangentBundle hirzebruchFan 3
u = matrix {{0},{0}}
assert(eulerChi(u,T) == 2)
assert(eulerChi T == 6)
T = cotangentBundle projectiveSpaceFan 4
assert(eulerChi T == -1)
T = tangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
u = matrix {{0},{0}}
assert(eulerChi(u,T) == 2)
assert(eulerChi T == 6)
///

-- Test 25
-- Checking coker
TEST ///
T = tangentBundle hirzebruchFan 2
T = T ** T
M = matrix {{1,0},{0,1},{1,0},{0,1/1}}
T1 = coker(T,M)
assert(T1#"ring" === QQ)
assert(T1#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{-2,-1}}, matrix{{0},{-1}} => matrix{{-2,-1}}, matrix{{0},{1}} => matrix{{-2,-1}}, matrix{{1},{0}} => matrix{{-2,-1}}})
assert(T1#"baseTable" === new HashTable from {map(ZZ^2,ZZ^1,{{0}, {-1}}) => map(QQ^2,QQ^2,{{0, 1}, {1, 0}}), map(ZZ^2,ZZ^1,{{1}, {0}}) => map(QQ^2,QQ^2,{{1,
      0}, {0, 1}}), map(ZZ^2,ZZ^1,{{-1}, {2}}) => map(QQ^2,QQ^2,{{-1/2, 1/2}, {1, 0}}), map(ZZ^2,ZZ^1,{{0}, {1}}) => map(QQ^2,QQ^2,{{0,
      1}, {1, 0}})})
assert(rank T1 == 2)
assert(T1#"dimension of the variety" == 2)
///

-- Test 26
-- Checking image
TEST ///
T = tangentBundle hirzebruchFan 2
T = T ** T
M = matrix {{1,-1,1,-1},{0,-1,0,1/1}}
T1 = image(T,M)
assert(T1#"ring" === QQ)
assert(T1#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{-2,-1}}, matrix{{0},{-1}} => matrix{{-2,-1}}, matrix{{0},{1}} => matrix{{-2,-1}}, matrix{{1},{0}} => matrix{{-2,-1}}})
assert(T1#"baseTable" === new HashTable from {map(ZZ^2,ZZ^1,{{0}, {-1}}) => map(QQ^2,QQ^2,{{-1, 1}, {1, 0}}), map(ZZ^2,ZZ^1,{{1}, {0}}) => map(QQ^2,QQ^2,{{1,
     0}, {0, 1}}), map(ZZ^2,ZZ^1,{{-1}, {2}}) => map(QQ^2,QQ^2,{{-1/2, 1/2}, {1, 0}}), map(ZZ^2,ZZ^1,{{0}, {1}}) => map(QQ^2,QQ^2,{{-1,
     1}, {1, 0}})})
assert(rank T1 == 2)
assert(T1#"dimension of the variety" == 2)
///

-- Test 27
-- Checking ker
TEST ///
T = tangentBundle hirzebruchFan 2
T = T ** T
M = matrix {{1,0,1,0},{0,1,0,1/1}}
T1 = ker(T,M)
assert(T1#"ring" === QQ)
assert(T1#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{-1,0}}, matrix{{0},{-1}} => matrix{{-1,0}}, matrix{{0},{1}} => matrix{{-1,0}}, matrix{{1},{0}} => matrix{{-1,0}}})
assert(T1#"baseTable" === new HashTable from {map(ZZ^2,ZZ^1,{{0}, {-1}}) => map(QQ^2,QQ^2,{{0, 1}, {1, 0}}), map(ZZ^2,ZZ^1,{{1}, {0}}) => map(QQ^2,QQ^2,{{1,
      0}, {0, 1}}), map(ZZ^2,ZZ^1,{{-1}, {2}}) => map(QQ^2,QQ^2,{{-1/2, 1/2}, {1, 0}}), map(ZZ^2,ZZ^1,{{0}, {1}}) => map(QQ^2,QQ^2,{{0,
      1}, {1, 0}})})
assert(rank T1 == 2)
assert(T1#"dimension of the variety" == 2)
///

-- Test 28
-- Checking twist
TEST ///
T = tangentBundle projectiveSpaceFan 3
L = {1,-4,3,-2}
T = twist(T,L)
assert(T#"ring" === QQ)
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0},{-1,1,0},{-1,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}})
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{3,4,4}}, matrix{{-1},{-1},{-1}} => matrix{{-2,-1,-1}}, matrix{{1},{0},{0}} => matrix{{1, 2, 2}}, matrix{{0},{1},{0}} => matrix{{-4,-3,-3}}})
assert(rank T == 3)
assert(T#"dimension of the variety" == 3)
///

-- Test 29
-- Checking isGeneral
TEST ///
T = tangentBundle pp1ProductFan 3
assert isGeneral T
L1 = {matrix {{1,0},{0,1}},matrix{{1,1},{0,1}},matrix{{-1,0},{0,1}},matrix{{-1,1},{0,-1}}}
L2 = {matrix {{-1,0}},matrix{{-1,0}},matrix{{-1,0}},matrix{{1,1}}}
T = toricVectorBundle(2,hirzebruchFan 3,L1,L2)
assert not isGeneral T
///

-- Test 30
-- Checking cartierIndex
TEST ///
C=posHull matrix {{1,2},{2,1}}
C1=posHull matrix {{1,-1},{2,-1}}
C2=posHull matrix {{2,-1},{1,-1}}
F=fan{C,C1,C2}
assert(cartierIndex({1,1,1},F) == 3)
assert(cartierIndex({3,3,3},F) == 1)
///


end


---------------------------------------
-- END OF FILE
---------------------------------------
uninstallPackage "ToricVectorBundles"
installPackage "ToricVectorBundles"
check "ToricVectorBundles"
restart

loadPackage "ToricVectorBundles";
P1 = convexHull matrix {{1,2,3,3,2,1,0,0},{0,0,1,2,3,3,2,1}};
F1 = normalFan P1;
T1 = tangentBundle F1
HH^1(T1)
HH^2(T1)
P2 = convexHull matrix {
     {1,0,0,-1,0,-1,0,1}, 
     {0,1,0,-1,0,0,-1,1}, 
     {0,0,1,0,-1,0,0,0}};
F2 = faceFan P2;
T2 = tangentBundle F2
HH^1(T2)
Omega = cotangentBundle F2
Omega == dual T2
Endo = T2 ** Omega
HH^1(Endo)
K = weilToCartier({-1,-1,-1,-1,-1,-1,-1,-1},F2)
areIsomorphic(K,exteriorPower(3,Omega))
restart
