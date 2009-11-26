needsPackage "Polyhedra"
---------------------------------------------------------------------------
-- PURPOSE: Computations with vector bundles on toric varieties 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : November 2008, November 2009
---------------------------------------------------------------------------
newPackage("ToricVectorBundles",
    Headline => "A package for computations with vector bundles on toric varieties",
    Version => "1.0",
    Date => "November 25, 2009",
    Authors => {
         {Name => "René Birkner",
	  HomePage => "http://page.mi.fu-berlin.de/rbirkner/index.htm",
	  Email => "rbirkner@math.fu-berlin.de"},
         {Name => "Nathan Ilten",
	  HomePage => "http://people.cs.uchicago.edu/~nilten/",
	  Email => "nilten@cs.uchicago.edu"},
         {Name => "Lars Petersen",
	  Email => "petersen@math.fu-berlin.de"}},
    DebuggingMode => true,
    Configuration => {}
    )

export {ToricVectorBundleKaneyama, 
        ToricVectorBundleKlyachko,
        toricVectorBundle, 
	addBase, 
	addBaseChange, 
	addDegrees, 
	addFiltration, 
	areIsomorphic, 
	base, 
	charts,
	cocycleCheck, 
	cotangentBundle,
	deltaE, 
	details,  
	dsum, 
	eulerChi, 
	existsDecomposition, 
	extPower, 
	filtration, 
	findWeights, 
	gradedRing, 
	isGeneral, 
	isomorphism, 
	isVectorBundle, 
	randomDeformation,
	regCheck, 
	symmProd, 
	tangentBundle, 
	tproduct, 
	twist, 
	weilToCartier, 
	hirzebruchFan,
	pp1ProductFan, 
	projectiveSpaceFan}

needsPackage "Polyhedra"

---------------------------------------------------------------------------
-- DEFINING NEW TYPES
---------------------------------------------------------------------------

-- Defining the new type ToricVectorBundleKaneyama
ToricVectorBundleKaneyama = new Type of HashTable
ToricVectorBundleKaneyama.synonym = "vector bundle on a toric variety (Kaneyama's description)"
globalAssignment ToricVectorBundleKaneyama



-- Defining the new type ToricVectorBundle
ToricVectorBundleKlyachko = new Type of HashTable
ToricVectorBundleKlyachko.synonym = "vector bundle on a toric variety (Klyachko's description)"
globalAssignment ToricVectorBundleKlyachko



-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net ToricVectorBundleKaneyama := tvb -> ( horizontalJoin flatten ( 
	  "{", 
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"dimension of the variety",
			                      "rank of the vector bundle",
					      "number of affine charts"}, key -> (net key, " => ", net tvb#key))),
	  "}" ))


-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net ToricVectorBundleKlyachko := tvb -> ( horizontalJoin flatten ( 
	  "{", 
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"dimension of the variety",
					      "rank of the vector bundle",
					      "number of affine charts",
					      "number of rays"}, key -> (net key, " => ", net tvb#key))),
	  "}" ))


---------------------------------------------------------------
-- FUNCTIONS TO CONSTRUCT VECTOR BUNDLES AND MODIFY THEM
---------------------------------------------------------------


-- PURPOSE : Building a Vector Bundle of rank 'k' on the Toric Variety given by the Fan 'F'
toricVectorBundle = method(Options => true)

--   INPUT : '(k,F)',  a strictly positive integer 'k' and a pure and full dimensional Fan 'F' 
--  OUTPUT : A ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
toricVectorBundle (ZZ,Fan) := {"Type"=>"Klyachko"} >> opts -> (k,F) -> (
     if opts#"Type" == "Kaneyama" then makeVBKaneyama(k,F) else if opts#"Type" == "Klyachko" then makeVBKlyachko(k,F) else error("Type must be Klyachko or Kaneyama"))

--   INPUT : '(k,F,L1,L2)',  a strictly positive integer 'k',a pure and full dimensional Fan 'F', and two lists 'L1' and 'L2'
--  OUTPUT : A ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko where the base matrices are given in the first list and the 
--     	     filtration matrices are given in the second list, 
--     	     if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama where the degree matrices are given in the first list and the
--     	     transition matrices are given in the second list.
toricVectorBundle (ZZ,Fan,List,List) := {"Type"=>"Klyachko"} >> opts -> (k,F,L1,L2) -> (
     if opts#"Type" == "Kaneyama" then makeVBKaneyama(k,F,L1,L2) else if opts#"Type" == "Klyachko" then makeVBKlyachko(k,F,L1,L2) else error("Type must be Klyachko or Kaneyama"))



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
     R := toList (tvb#"ToricVariety")#"rays";
     -- Checking for input errors
     if n != #L then error("Number of matrices must match number of rays of the fan");
     if any(L, l -> not instance(l,Matrix)) then error("The bases must be given as matrices");
     P := unique apply(L,ring);
     if #P != 1 then (
	  if P === {QQ,ZZ} or P === {ZZ,QQ} then (
	       L = apply(L, l -> promote(l,QQ));
	       P = {QQ})
	  else error("The bases must all be over the same ring"));
     -- Creating the table of bases for the rays
     baseTable := hashTable apply(n, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if numColumns M != k or numRows M != k then error("The base change matrices must be a rank times rank matrices");
	       if det M == 0 then error("The bases must have full rank");
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
     if #pairlist != #L then error("Number of matrices must match number of codim 1 Cones");
     baseChangeTable := hashTable apply(#pairlist, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if not instance(M,Matrix) then error("The transition matrices must be given as rank times rank matrices");
	       if numColumns M != k or numRows M != k then error("The base change matrices must be k by k matrices");
	       if det M == 0 then error("The base change matrices must be invertible");
	       R := ring source M;
	       M = if R === ZZ or R === QQ then promote(M,QQ) else error("expected base change over ZZ or QQ");
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
     if #tCT != #L then error("Number of degree matrices must match the number of top dim cones");
     degreeTable := hashTable apply(#tCT, i -> ( 
	       M := L#i;
	       -- Checking for more input errors
	       if not instance(M,Matrix) then error("The degrees must be given as dimension times rank matrices");
	       if ring M =!= ZZ then error("The degrees must be in the ZZ lattice");
	       if numColumns M != k then error("The number of degrees must match the vector bundle rank");
	       if numRows M != n then error("The degrees must have the dimension of the underlying toric variety");
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
     if n != #L then error("Number of matrices must match number of rays of the fan");
     if any(L, l -> not instance(l,Matrix)) then error("The filtrations must be given as matrices");
     if any(L, l -> ring l =!= ZZ) then error("The filtrations must be given as matrices over ZZ");
     if any(L, l -> numColumns l != k or numRows l != 1) then error("The filtrations must be given as 1 times n matrices");
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
charts = method(TypicalValue => ZZ)

--   INPUT : 'tvb', a ToricVectorBundleKaneyama
--  OUTPUT : 'ZZ',  the number of affine charts
charts ToricVectorBundleKaneyama := tvb -> tvb#"number of affine charts"

--   INPUT : 'tvb', a ToricVectorBundleKlyachko
--  OUTPUT : 'ZZ',  the number of affine charts
charts ToricVectorBundleKlyachko := tvb -> tvb#"number of affine charts"
	       


-- PURPOSE : Checking if the ToricVectorBundleKaneyama fulfills the cocycle condition
--   INPUT : 'tvb',  a ToricVectorBundleKaneyama 
--  OUTPUT : 'true' or 'false' 
cocycleCheck = method(TypicalValue => Boolean)
cocycleCheck ToricVectorBundleKaneyama := tvb -> (
     if not tvb.cache.?cocycle then (
     	  -- Extracting data out of tvb
     	  n := tvb#"dimension of the variety";
     	  k := tvb#"rank of the vector bundle";
     	  bCT := tvb#"baseChangeTable";
     	  topCones := sort keys tvb#"topConeTable";
     	  L := hashTable {};
     	  -- For each codim 2 Cone computing the list of topCones which have this Cone as a face
     	  -- and save the list of indices of these topCones as an element in L
     	  for i from 0 to #topCones - 1  do L = merge(hashTable apply(faces(2,topCones#i), C -> C => {i}),L,(a,b) -> sort join(a,b));
	  --scan(#topCones, i -> L = merge(hashTable apply(faces(2,topCones#i), C -> C => {i}),L,(a,b) -> sort join(a,b)));
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
     	  tvb.cache.cocycle = all(L, l -> product apply(reverse l, e -> if e#0 > e#1 then inverse(bCT#(e#1,e#0)) else bCT#e) == map(QQ^k,QQ^k,1)));
     tvb.cache.cocycle)



-- PURPOSE : Presenting some details of the given ToricVectorBundle
details = method()

--   INPUT : 'tvb',  a ToricVectorBundleKaneyama
--  OUTPUT : '(A,C)',	 where 'A' is a hashTable giving the enumeration of the maximal cones with their rays and degree matrix, 
--     	    	      	 and 'B' gives the transition matrices for the codim 1 pairs
-- COMMENT : This function gives the posibillity to have a quick overview on the main properties of a ToricVectorBundleKaneyama
details ToricVectorBundleKaneyama := tvb -> (hashTable apply(pairs(tvb#"topConeTable"), p -> ( p#1 => (rays(p#0),(tvb#"degreeTable")#(p#0)))),tvb#"baseChangeTable")


--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : 'T',	 a HashTable that gives for the enumeration of the rays the corresponding ray, basis and filtration matrix
-- COMMENT : This function gives the posibillity to have a quick overview on the main properties of a ToricVectorBundleKlyachko
details ToricVectorBundleKlyachko := tvb -> hashTable apply(rays tvb, r -> r => ((tvb#"baseTable")#r,(tvb#"filtrationMatricesTable")#r))



-- PURPOSE : Checking if a ToricVectorBundleKaneyama satisfies the regularity conditions of the degrees
--   INPUT : 'tvb', a ToricVectorBundleKaneyama
--  OUTPUT : 'true' or 'false'
-- COMMENT : This function is for checking ToricVectorBundles whose degrees and matrices 
--     	     are inserted by hand. Those generated for example by tangentBundle fulfill the 
--     	     conditions automatically.
regCheck = method(TypicalValue => Boolean)
regCheck ToricVectorBundleKaneyama := tvb -> (
     if not tvb.cache.?regCheck then (
     	  -- Extracting the neccesary data
     	  tCT := sort keys tvb#"topConeTable";
     	  c1T := tvb#"codim1Table";
     	  bCT := tvb#"baseChangeTable";
     	  dT := tvb#"degreeTable";
     	  k := tvb#"rank of the vector bundle";
     	  tvb.cache.regCheck = all(keys bCT, p -> (
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
				   	(if A^{i}_{j} != 0 then contains(C,rj-ri) else true) and (if A^{j}_{i} != 0 then contains(C,ri-rj) else true))))))));
     tvb.cache.regCheck)



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
		    if sort fMT1#r0 != sort fMT2#r0 then false
		    else (
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

--   INPUT : '(T,u)',  where 'T' is a ToricVectorBundleKlyachko and 'u' is a one column matrix over ZZ giving a degree vector
--  OUTPUT : The Euler characteristic of the Cech complex at degree 'u'
eulerChi (ToricVectorBundleKlyachko,Matrix) := (T,u) -> (
     if not T.cache.?eulerChi then T.cache.eulerChi = new MutableHashTable;
     if not T.cache.eulerChi#?u then (
	  n := T#"dimension of the variety";
	  -- Compute the Cech complex and compute the alternating sum of the dimensions
	  T.cache.eulerChi#u = sum apply(n+1, i -> (-1)^i * sum values (cechComplex(i,T,u))#1));
     T.cache.eulerChi#u)

--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : The Euler characteristic of the bundle
eulerChi ToricVectorBundleKlyachko := T -> (
     -- Compute the set of degrees with possible cohomology
     L := latticePoints deltaE T;
     -- Sum up their characteristics
     sum apply(L, l -> eulerChi(T,l)))

--   INPUT : '(T,u)',  where 'T' is a ToricVectorBundleKlyachko and 'u' is a one column matrix over ZZ giving a degree vector
--  OUTPUT : The Euler characteristic of the Cech complex at degree 'u'
eulerChi (ToricVectorBundleKaneyama,Matrix) := (T,u) -> (
     if not T.cache.?eulerChi then T.cache.eulerChi = new MutableHashTable;
     if not T.cache.eulerChi#?u then (
	  n := T#"dimension of the variety";
	  -- Compute the Cech complex and compute the alternating sum of the dimensions
	  T.cache.eulerChi#u = sum apply(n+2, i -> (-1)^i * numColumns (cechComplex(i,T,u))#1));
     T.cache.eulerChi#u)

--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : The Euler characteristic of the bundle
eulerChi ToricVectorBundleKaneyama := T -> (
     -- Compute the set of degrees with possible cohomology
     L := latticePoints deltaE T;
     -- Sum up their characteristics
     sum apply(L, l -> eulerChi(T,l)))


-- PURPOSE : Returning the table of codimension 1 cones of the underlying fan
--   INPUT : 'T',  a ToricVectorBundleKaneyama
--  OUTPUT : a HashTable
codim1Table = method(TypicalValue => HashTable)
codim1Table ToricVectorBundleKaneyama := T -> T#"codim1Table"     



-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKaneyama
--   INPUT : '(i,T,weight)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKaneyama, and 'weight' the degree
--  OUTPUT : 'ZZ',	     the graded module of the degree 'weight' part of the 'i'th cohomology group of 'T'
cohomology(ZZ,ToricVectorBundleKaneyama,Matrix) := opts -> (i,T,weight) -> cohom(i,T,weight)

-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKlyachko
--   INPUT : '(i,T,weight)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKlyachko, and 'weight' the degree
--  OUTPUT : 'ZZ',	     the graded module of the degree 'weight' part of the 'i'th cohomology group of 'T'
cohomology(ZZ,ToricVectorBundleKlyachko,Matrix) := opts -> (i,T,weight) -> cohom(i,T,weight)

-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKaneyama
--   INPUT : '(i,T,P)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKaneyama, and 'P' a list of degrees
--  OUTPUT : 'List',	     the list of the graded modules of the corresponding degree parts of the cohomology group which are non zero
cohomology(ZZ,ToricVectorBundleKaneyama,List) := opts -> (i,T,P)-> (
	if opts.Degree == 1 then print ("Number of degrees to calculate: "|(toString(#P)));
	for j in P list (
	     if opts.Degree == 1 then << "." << flush;
	     j = cohomology(i,T,j);
	     if j != 0 then j else continue))

-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKlyachko
--   INPUT : '(i,T,P)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKlyachko, and 'P' a list of degrees
--  OUTPUT : 'List',	     the list of the graded modules of the corresponding degree parts of the cohomology group which are non zero
cohomology(ZZ,ToricVectorBundleKlyachko,List) := opts -> (i,T,P)-> (
	if opts.Degree == 1 then print ("Number of degrees to calculate: "|(toString(#P)));
	for j in P list (
	     if opts.Degree == 1 then << "." << flush;
	     j = cohomology(i,T,j);
	     if j != 0 then j else continue))

-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKaneyama
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKaneyama
--  OUTPUT : the group as a graded module where the generators have the corresponding degree of the weight vector
-- COMMENT : if the option "Degree" => 1 is given then it displays the number of degrees to calculate
cohomology(ZZ,ToricVectorBundleKaneyama) := opts -> (i,T)-> (
     L := cohomology(i,T,latticePoints deltaE T,Degree => opts.Degree);
     if L == {} then (gradedRing T)^0 else directSum L)

-- PURPOSE : Computing the cohomology group of a given ToricVectorBundleKlyachko
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKlyachko
--  OUTPUT : the group as a graded module where the generators have the corresponding degree of the weight vector
-- COMMENT : if the option "Degree" => 1 is given then it displays the number of degrees to calculate
cohomology(ZZ,ToricVectorBundleKlyachko) := opts -> (i,T)-> (
     L := cohomology(i,T,latticePoints deltaE T,Degree => opts.Degree);
     if L == {} then (gradedRing T)^0 else directSum L)


    
-- PURPOSE : Computing the rank of the cohomology group of a given ToricVectorBundleKlyachko or ToricVectorBundleKaneyama
--   INPUT : '(i,S)',  'i' for the 'i'th cohomology group, 'S' a Sequence of ToricVectorBundleKlyachko or a ToricVectorBundleKaneyama, 
--     	    	      and weight vector
--  OUTPUT : 'ZZ',	     the rank of the degree 'weight' part of the 'i'th cohomology group of the bundle
hh(ZZ,Sequence) := (i,S) -> (
     -- Checking for input errors
     if #S != 2 then error("The Sequence must contain a toric vector bundle and a weight vector");
     if not instance(S#1,Matrix) then error("The second argument must be a weight vector given by a matrix");
     if not instance(S#0,ToricVectorBundleKaneyama) and not instance(S#0,ToricVectorBundleKlyachko) then error("The first argument must be a toric vector bundle");
     (T,u) := S;
     rank cohomology(i,T,u))

-- PURPOSE : Computing the rank of the cohomology group of a given ToricVectorBundleKlyachko
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKlyachko
--  OUTPUT : 'ZZ',  the rank of the 'i'th cohomology group
hh(ZZ,ToricVectorBundleKlyachko) := ZZ => (i,T) -> rank cohomology(i,T)

-- PURPOSE : Computing the rank of the cohomology group of a given ToricVectorBundleKaneyama
--   INPUT : '(i,T)',  'i' for the 'i'th cohomology group, 'T' a ToricVectorBundleKaneyama
--  OUTPUT : 'ZZ' or 'List',  the rank of the 'i'th cohomology group
hh(ZZ,ToricVectorBundleKaneyama) := ZZ => (i,T) -> rank cohomology(i,T)



-- PURPOSE : Computing the coker bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as target
--  OUTPUT : The bundle given by the cokernels of the filtrations
coker (ToricVectorBundleKlyachko,Matrix) := (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numRows M then error("The source of the matrix must be vector bundle");
     if tRing =!= ring M then error("Matrix and bundle must be over the same ring"); 
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
			 cols = cols | (fT#j)#i;
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
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
cotangentBundle Fan := opts -> F -> (
     if opts#"Type" == "Klyachko" then dual tangentBundleKlyachko F else if opts#"Type" == "Kaneyama" then cotangentBundleKaneyama F else error("Type must be Klyachko or Kaneyama"))



--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama 
cotangentBundleKaneyama = F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety must be smooth");
     if (not isPure F) or dim F != ambDim F then error("The Toric Variety must be pure and full dimensional");
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



-- PURPOSE : Computing the polytope deltaE in the degree space such that outside this polytope
--     	     every cohomology is 0 
deltaE = method()

--   INPUT : 'tvb',  a ToricVectorBundleKaneyama
--  OUTPUT : a Polyhedron
deltaE ToricVectorBundleKaneyama := tvb -> (
     if not tvb.cache.?deltaE then (
     	  -- Extracting neccesary data
     	  raylist := rays tvb;
     	  l := #raylist;
     	  k := tvb#"rank of the vector bundle";
     	  n := tvb#"dimension of the variety";
     	  tCT := sort keys tvb#"topConeTable";
     	  dT := tvb#"degreeTable";
     	  -- Creating an index table, for each ray the first top cone containing it
     	  raytCTindex := hashTable apply(#raylist, r -> r => position(tCT, C -> contains(C,raylist#r)));
     	  raylist = transpose matrix {raylist};
     	  -- Get the subsets of 'n' elements in 'l'
     	  sset := subsets(l,n);
     	  jList := {{}};
     	  -- Get all different combinations of choices of variety dimension many degree vectors
     	  for i from 0 to n-1 do jList = flatten apply(jList, l -> apply(k, j -> l|{j}));
     	  M := map(QQ^1,QQ^n,0);
     	  v := map(QQ^1,QQ^1,0);
     	  -- For every 'n' in 'l' subset and any combination in jList get the intersection of the dual cones
     	  -- of the corresponding rays. If this is a non-empty compact polytope then add the vertices to the
     	  -- list L
     	  L := unique flatten apply(sset, s -> (
	       	    unique for j in jList list (
		    	 N := matrix apply(n, i -> {raylist^{s#i},raylist^{s#i} * ((dT#(tCT#(raytCTindex#(s#i))))_{j#i})});
		    	 w := N_{n};
		    	 N = submatrix'(N,{n});
		    	 P := intersection(M,v,N,w);
		    	 if isCompact P and (not isEmpty P) then vertices P else continue)));
     	  -- Make a matrix of all the vertices in L
     	  M = matrix {L};
     	  tvb.cache.deltaE = convexHull M);
     tvb.cache.deltaE)


--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : a Polyhedron
deltaE ToricVectorBundleKlyachko := tvb -> (
     if not tvb.cache.?deltaE then (
	  -- Extracting neccesary data
	  rayTable := tvb#"rayTable";
	  l := #rayTable;
	  n := tvb#"dimension of the variety";
	  fMT := hashTable apply(pairs tvb#"filtrationMatricesTable", (i,j) -> (j = flatten entries j; i => matrix{{-(min j),max j}}));
	  sset := select(subsets(rays tvb,n), s -> rank matrix {s} == n);
	  tvb.cache.deltaE = convexHull matrix {apply(sset, s -> (
			 M := transpose matrix {apply(s, r -> (-r | r) || (fMT#r))};
			 vertices intersection(M_{0..n-1},M_{n})))});
     tvb.cache.deltaE)


-- PURPOSE : Returning the dimension of the underlying toric variety
--   INPUT : 'T',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
--  OUTPUT : the dimension in ZZ
dim ToricVectorBundleKaneyama := T -> dim T#"ToricVariety"
dim ToricVectorBundleKlyachko := T -> dim T#"ToricVariety"
     
     

-- PURPOSE : Computing the direct sum of two ToricVectorBundles over the same Fan
dsum = method(TypicalValue => ToricVectorBundleKaneyama)

--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKaneyama over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama which is the direct sum
dsum (ToricVectorBundleKaneyama,ToricVectorBundleKaneyama) := (tvb1,tvb2) -> (
	  -- Checking for input errors
	  if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("The bundles must be over the same toric variety!");
	  -- Extracting data out of tvb1 and tvb2
	  k1 := tvb1#"rank of the vector bundle";
	  k2 := tvb2#"rank of the vector bundle";
	  -- Generating the trivial bundle of dimension k1+k2
	  tvb := makeVBKaneyama(k1 + k2,tvb1#"ToricVariety");
	  -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
	  E := new ToricVectorBundleKaneyama from {
	       "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> a|b),
	       "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> a++b),
	       "ToricVariety" => tvb#"ToricVariety",
	       "number of affine charts" => tvb#"number of affine charts",
	       "dimension of the variety" => tvb#"dimension of the variety",
	       "rank of the vector bundle" => k1 + k2,
	       "codim1Table" => tvb#"codim1Table",
	       "topConeTable" => tvb#"topConeTable",
	       symbol cache => new CacheTable};
	  if (tvb1.cache.?regCheck and tvb2.cache.?regCheck and tvb1.cache.regCheck and tvb2.cache.regCheck and (
		    tvb1.cache.?cocycle and tvb2.cache.?cocycle and tvb1.cache.cocycle and tvb2.cache.cocycle)) then (
	       E.cache.regCheck = true;
	       E.cache.cocycle = true);
	  E)
     
     
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKlyachko over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko which is the direct sum
dsum (ToricVectorBundleKlyachko,ToricVectorBundleKlyachko) := (tvb1,tvb2) -> (
	  -- Extracting data out of tvb1 and tvb2
	  k1 := (tvb1#"rank of the vector bundle");
	  k2 := (tvb2#"rank of the vector bundle");
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

     
ToricVectorBundleKaneyama ++ ToricVectorBundleKaneyama := dsum --(tvb1,tvb2) -> dsum(tvb1,tvb2)
ToricVectorBundleKlyachko ++ ToricVectorBundleKlyachko := dsum --(tvb1,tvb2) -> dsum(tvb1,tvb2)


-- PURPOSE : Computing the dual bundle to a given ToricVectorBundleKaneyama
--   INPUT : 'tvb',  a ToricVectorBundleKaneyama 
--  OUTPUT : the dual ToricVectorBundleKaneyama 
dual ToricVectorBundleKaneyama := {} >> opts -> tvb -> (
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



-- PURPOSE : Computing the dual bundle to a given ToricVectorBundleKlyachko
--   INPUT : 'tvb',  a ToricVectorBundleKlyachko
--  OUTPUT : the dual ToricVectorBundleKlyachko
dual ToricVectorBundleKlyachko := {} >> opts -> tvb -> (
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
     T)
	  
	  
-- PURPOSE : Checking if a given List of possible degree vectors admits a Decomposition in torus eigenspaces that give the filtration
--   INPUT : '(T,L)',  where 'T' is a ToricVectorBundleKlyachko and 'L' is a List where the i-th entry is either a matrix or a List of 
--     	    	       matrices of possible degree vectors for the i-th cone in maxCones
--  OUTPUT : 'true' if a selection of degrees for each maximal cone admits a decomposition, 'false' otherwise
existsDecomposition = method()
existsDecomposition (ToricVectorBundleKlyachko,List) := (T,L) -> (
     -- Checking if the list contains only matrices and lists and converting the former into a list with this matrix
     L = apply(L, l -> if instance(l,List) then l else if instance(l,Matrix) then {l} else error("The elements of the list must be either matrices or lists of them"));
     if not T.cache.?degreesList then T.cache.degreesList = {};
     mC := maxCones T;
     mC = apply(mC, C -> (C = C#"rays"; apply(numColumns C, i -> C_{i})));
     -- Checking for input errors
     if #mC != #L then error("There must be a degree matrix or list of degree matrices for each maximal cone of the fan");
     -- Check if any combination of matrices in L has already been checked and thus saved in the cache
     if any(T.cache.degreesList, dl -> all(toList(0..#dl-1), i -> (set(L#i))#?(dl#i))) then true 
     -- otherwise for each maximal cone check the decomposition criterion
     else (
	  -- Add to each Cone the list of possible degrees
     	  mC = apply(#mC, i -> (mC#i,L#i));
     	  allRaysTable := tableForAllRays T;
     	  n := dim T;
     	  k := rank T;
     	  R := T#"ring";
	  -- Recursive function that runs through all possible combinations of filtration steps for the rays of a cone
     	  recursiveCheck := (fList,Es,D) -> (
	       -- if there is still a list of filtration steps, call recursiveCheck again for each entry
	       if fList != {} then (
	       	    Lr := (fList#0)#1;
	       	    r := (fList#0)#0;
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
	       T.cache.degreesList = T.cache.degreesList|{L};
	       true)
	  else false))
	  
	       

-- PURPOSE : Computing the 'l'-th exterior power of a ToricVectorBundleKlyachko
extPower = method()

--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundleKlyachko
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko which is the 'l'-th exterior power
extPower (ZZ,ToricVectorBundleKlyachko) := (l,tvb) -> (
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     if l > k then error("The power must not be greater than the rank of the bundle");
     -- Extracting data
     baseTable := tvb#"baseTable";
     filtrationTable := tvb#"filtrationMatricesTable";
     Rs := rays tvb;
     R := tvb#"ring";
     F := tvb#"ToricVariety";
     -- Generating the list of 'l'-tuples of 0..k-1 and the corresponding index table
     ind := subsets(k,l);
     indtable := hashTable apply(#ind, i -> ind#i => i);
     -- Computing the 'l'-th exterior powers of the base matrices
     baseTable = apply(Rs, r -> (
	       B := baseTable#r;
	       M := mutableMatrix(R,#ind,#ind);
	       for j in ind do for k in ind do M_(indtable#k,indtable#j) = det(B^k_j);
	       --scan(ind, j -> (
			-- scan(ind, k -> (
				--   M_(indtable#k,indtable#j) = det(B^k_j)))));
	       matrix M));
     -- Computing the 'l'-th exterior power of the filtration matrices
     filtrationTable = apply(Rs, r -> (
	       filt := filtrationTable#r;
	       matrix {apply(ind, j -> ( sum flatten entries filt_j))}));
     T := makeVBKlyachko(#ind,F,baseTable,filtrationTable);
     if tvb.cache.?isVB and tvb.cache.isVB then T.cache.isVB = true;
     T)

--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a ToricVectorBundleKaneyama
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama which is the 'l'-th exterior power
extPower (ZZ,ToricVectorBundleKaneyama) := (l,tvb) -> (
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     if l > k then error("The power must not be greater than the rank of the bundle");
     -- Generating the list of 'l'-tuples of 0..k-1 and the corresponding index table
     ind := subsets(k,l);
     indtable := hashTable apply(#ind, i -> ind#i => i);
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
     E)



-- PURPOSE : Returning the underlying fan of a toric vector bundle
--   INPUT : 'T',  a ToricVectorBundleKaneyama
--  OUTPUT : a Fan
fan ToricVectorBundleKaneyama := T -> T#"ToricVariety"

-- PURPOSE : Returning the underlying fan of a toric vector bundle
--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : a Fan
fan ToricVectorBundleKlyachko := T -> T#"ToricVariety"



-- PURPOSE : Finding all possible sets of weight vectors for each maximal cone in the fan that admit the 
--           filtration steps on the rays
--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : a List,  where the i-th entry is the list of possible weight matrices for the i-th cone in maxCones T
findWeights = method(TypicalValue => List)
findWeights ToricVectorBundleKlyachko := T -> (
     if not T.cache.?weights then (
     	  -- Get the maximal cones and save their rays
	  mC := maxCones T;
     	  mC = apply(mC, C -> (C = C#"rays"; apply(numColumns C, i -> C_{i})));
     	  n := dim T;
     	  k := rank T;
	  -- Recursive function that goes through the rays and checks for the current ray which filtration steps are possible and for 
	  -- these calls itself again
	  -- E is the intersection of filtrations of the rays considered so far, L is the list of remaining rays with filtration steps not choosen so far, 
	  -- R is the list of filtration steps not choosen before for rays already handled (these are the possible steps for the next column and newColumn 
	  -- is the already created part of the new column
     	  recursiveColumnsConstructer := (E,L,R,newColumn) -> (
	       if L != {} then (
	       	    l := L#0;
	       	    L = drop(L,1);
	       	    lunique = unique l;
	       	    flatten for e in lunique list (
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
	       Lnew := recursiveColumnsConstructer((Elist#0)#1,L,{},{(Elist#0)#0});
	       if #(L#0) != 1 then flatten apply(Lnew, ln -> recursiveMatricesConstructer(drop(Elist,1),ln#0,M|{ln#1}))
	       else apply(Lnew, ln -> M|{ln#1}));
     	  fMT := T#"filtrationMatricesTable";
     	  bT := T#"baseTable";
     	  bundleRing := T#"ring";
	  allRaysTable := tableForAllRays T;
     	  T.cache.weights = apply(mC, C -> (
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
			      else continue)))));
     T.cache.weights)



-- PURPOSE : Generating the graded Ring for thew cohomology groups
gradedRing = method()

--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : the ring of the bundle with degree space the lattice of the variety
gradedRing ToricVectorBundleKlyachko := T -> (
     if not T.cache.?gradedRing then T.cache.gradedRing = (T#"ring")[DegreeRank => dim T];
     T.cache.gradedRing)

--   INPUT : 'T',  a ToricVectorBundleKaneyama
--  OUTPUT : QQ with degree space the lattice of the variety
gradedRing ToricVectorBundleKaneyama := T -> (
     if not T.cache.?gradedRing then T.cache.gradedRing = QQ[DegreeRank => dim T];
     T.cache.gradedRing)



-- PURPOSE : Computing the image bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as its source
--  OUTPUT : The bundle given by the images of the filtrations
image (ToricVectorBundleKlyachko,Matrix) := (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numColumns M then error("The source of the matrix must be vector bundle");
     if tRing =!= ring M then error("Matrix and bundle must be over the same ring"); 
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
			 cols = cols | (fT#j)#i;
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
isGeneral ToricVectorBundleKlyachko := tvb -> (
     if not tvb.cache.?isGeneral then (
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
     	  tvb.cache.isGeneral = all(F, C -> (
	       	    C = C#"rays";
	       	    C = apply(numColumns C, i -> C_{i});
	       	    recursiveCheck(apply(C, r -> L#r),{}))));
     tvb.cache.isGeneral)


-- PURPOSE : Checking if the data in T in fact defines a vectorbundle, i.e. satisfies the decomposition condition or
--     	     regularity and cocycle condition
--   INPUT : 'T',  a ToricVectorBundleKlyachko or ToricVectorBundleKaneyama
--  OUTPUT : 'true' if if 'T' is fact a bundle, 'false' otherwise
isVectorBundle = method()
isVectorBundle ToricVectorBundleKlyachko := T -> (
     if not T.cache.?isVB then (
	  L := findWeights T;
	  T.cache.isVB = if any(L, l -> l == {}) then false else existsDecomposition(T,L));
     T.cache.isVB)

isVectorBundle ToricVectorBundleKaneyama := T -> (
     if not T.cache.?isVB then T.cache.isVB = regCheck T and cocycleCheck T;
     T.cache.isVB)



-- PURPOSE : Computing the kernel bundle of a toric vector bundle
--   INPUT : '(T,M)', where 'T' is a ToricVectorBundleKlyachko and 'M' a matrix with the bundle space as source
--  OUTPUT : The bundle given by the kernels of the filtrations
ker (ToricVectorBundleKlyachko,Matrix) := opts -> (T,M) -> (
     k := T#"rank of the vector bundle";
     tRing := T#"ring";
     -- Checking for input errors
     if k != numColumns M then error("The source of the matrix must be vector bundle");
     if tRing =!= ring M then error("Matrix and bundle must be over the same ring"); 
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
			 cols = cols | (fT#j)#i;
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
     


-- PURPOSE : Returning the maximal cones of the fan
--   INPUT : 'T',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
--  OUTPUT : a List of Cones
maxCones ToricVectorBundleKaneyama := T -> sort maxCones T#"ToricVariety"
maxCones ToricVectorBundleKlyachko := T -> sort maxCones T#"ToricVariety"



-- PURPOSE : Compute a random deformation of a ToricVectorBundleKlyachko
randomDeformation = method(TypicalValue => ToricVectorBundleKlyachko)

--   INPUT : '(tvb,l,h)',  where 'tvb' is a ToricVectorBundleKlyachko, 'l' and 'h' are integers
--  OUTPUT : a ToricVectorBundleKlyachko, a random deformation
-- COMMENT : Simply replaces the base matrices by random matrices of full rank with entries between 
--     	     'l' and 'h'
randomDeformation (ToricVectorBundleKlyachko,ZZ,ZZ) := (tvb,l,h) -> (
     -- Checking for input errors
     if l == h then error("The two integers must be different");
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
--   INPUT : 'T',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
rank ToricVectorBundleKaneyama := T -> T#"rank of the vector bundle"
rank ToricVectorBundleKlyachko := T -> T#"rank of the vector bundle"



-- PURPOSE : Giving the rays of the underlying Fan of a toric vector bundle
--   INPUT : 'tvb',  a TorcVectorBundleK
--  OUTPUT : 'L',  a List containing the rays of the Fan underlying the bundle
rays ToricVectorBundleKlyachko := tvb -> toList (tvb#"ToricVariety")#"rays"

--   INPUT : 'tvb',  a TorcVectorBundleK
--  OUTPUT : 'L',  a List containing the rays of the Fan underlying the bundle
rays ToricVectorBundleKaneyama := tvb -> toList (tvb#"ToricVariety")#"rays"



-- PURPOSE : Computing the 'l'-th symmetric product of a ToricVectorBundleKlyachko
--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundleKlyachko
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko which is the 'l'-th exterior power
symmProd = method()
symmProd(ZZ,ToricVectorBundleKlyachko) := (l,tvb) -> (
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     -- Extracting data
     k := tvb#"rank of the vector bundle";
     baseTable := tvb#"baseTable";
     filtrationTable := tvb#"filtrationMatricesTable";
     Rs := rays tvb;
     R := tvb#"ring";
     F := tvb#"ToricVariety";
     -- Generating the list of 'l'-tuples of 0..k-1 with duplicates and the corresponding index table
     ind := toList(0..k-1);
     allind := sort unique subsets(flatten toList(l:ind),l);
     ind = sort unique subsets(sort flatten toList(l:ind),l);
     indtable = hashTable apply(#ind, i -> ind#i => i);
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
     T)

--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundle
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama which is the 'l'-th exterior power
symmProd(ZZ,ToricVectorBundleKaneyama) := (l,tvb) -> (
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     -- Extracting data
     k := tvb#"rank of the vector bundle";
     -- Generating the list of 'l'-tuples of 0..k-1 with duplicates and the corresponding index table
     ind := toList(0..k-1);
     allind := sort unique subsets(flatten toList(l:ind),l);
     ind = sort unique subsets(sort flatten toList(l:ind),l);
     indtable := hashTable apply(#ind, i -> ind#i => i);
     -- Computing the 'l'-th symmetric product of the transition matrices
     baseChangeTable := hashTable apply(pairs tvb#"baseChangeTable", p -> (
	       B := p#1;
	       M := mutableMatrix(QQ,#ind,#ind);
	       for i1 in ind do (
		    Bi := B_(i1);
		    for j in allind do M_(indtable#(sort j),indtable#i1) = M_(indtable#(sort j),indtable#i1) + product apply(#j, j1 -> Bi_(j#j1,j1)));
	       M = matrix M;
	       p#0 => M));
     -- Computing the 'l'-th symmetric products of the degrees
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
     E)



-- PURPOSE : Computing the tangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
tangentBundle = method(Options => {"Type" => "Klyachko"})
tangentBundle Fan := opts -> F -> (
     if opts#"Type" == "Klyachko" then tangentBundleKlyachko F else if opts#"Type" == "Kaneyama" then dual cotangentBundleKaneyama F else error("Type must be Klyachko or Kaneyama"))


-- PURPOSE : Computing the tangent bundle (Klyachko) on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko 
tangentBundleKlyachko = F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety must be smooth");
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



-- PURPOSE : Checking if the two ToricVectorBundleKaneyama are equal
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKaneyama
--  OUTPUT : 'true' or 'false' 
ToricVectorBundleKaneyama == ToricVectorBundleKaneyama := (tvb1,tvb2) -> tvb1 === tvb2

-- PURPOSE : Checking if the two ToricVectorBundleKlyachko are equal
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKlyachko
--  OUTPUT : 'true' or 'false' 
ToricVectorBundleKlyachko == ToricVectorBundleKlyachko := (tvb1,tvb2) -> tvb1 === tvb2

      

-- PURPOSE : Computing the tensor product of two ToricVectorBundles over the same Fan
tproduct = method()

--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKaneyama over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama which is the tensor product
tproduct (ToricVectorBundleKaneyama,ToricVectorBundleKaneyama) := ToricVectorBundleKaneyama => (tvb1,tvb2) -> (
     -- Checking for input errors
     if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("The bundles must be over the same toric variety!");
     -- Extracting data out of tvb1 and tvb2
     k1 := tvb1#"rank of the vector bundle";
     k2 := tvb2#"rank of the vector bundle";
     -- Generating the trivial bundle of dimension k1+k2
     tvb := makeVBKaneyama(k1 * k2,tvb1#"ToricVariety");
     -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
     E := new ToricVectorBundleKaneyama from {
	  "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> matrix {flatten apply(k2, j -> apply(k1, i -> a_{i}+b_{j}))}),
	  "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> (
		    matrix flatten apply(k2, j -> apply(k1, i -> flatten apply(k2, j' -> apply(k1, i' -> a_(i,i') * b_(j,j'))))))),
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => k1 + k2,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable",
	  symbol cache => new CacheTable};
     if (tvb1.cache.?regCheck and tvb2.cache.?regCheck and tvb1.cache.regCheck and tvb2.cache.regCheck and (
	       tvb1.cache.?cocycle and tvb2.cache.?cocycle and tvb1.cache.cocycle and tvb2.cache.cocycle)) then (
	  E.cache.regCheck = true;
	  E.cache.cocycle = true);
     E)


--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleKlyachko over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleKlyachko which is the tensor product
tproduct (ToricVectorBundleKlyachko,ToricVectorBundleKlyachko) := ToricVectorBundleKlyachko => (tvb1,tvb2) -> (
     -- Extracting data out of tvb1 and tvb2
     k1 := tvb1#"rank of the vector bundle";
     k2 := tvb2#"rank of the vector bundle";
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

     
ToricVectorBundleKaneyama ** ToricVectorBundleKaneyama := tproduct
ToricVectorBundleKlyachko ** ToricVectorBundleKlyachko := tproduct



-- PURPOSE : Computing the twist of a Toric Vector Bundle by a divisor line bundle
--   INPUT : '(T,d)',  where 'T' is a toricVectorBundleKlyachko and 'd' a list of integers one for each ray of the fan
--  OUTPUT : a ToricVectorBundleKlyachko
-- COMMENT : If d={d_1,..d_l} then this corresponds to the line bundle which is the d_i twist on the i-th ray
twist = method(TypicalValue => ToricVectorBundleKlyachko)
twist (ToricVectorBundleKlyachko,List) := (T,d) -> (
     k := T#"rank of the vector bundle";
     fT := T#"filtrationMatricesTable";
     -- Checking for input errors
     if #d != #fT then error("The number of twists must much the number of rays of the fan");
     R := rays T;
     fT = apply(#R, i -> fT#(R#i) + matrix{toList(k:-(d#i))});
     addFiltration(T,fT))


-- PURPOSE : Generating the Vector Bundle given by a divisor
weilToCartier = method(Options => {"Type" => "Klyachko"})

--   INPUT : '(L,F)',  a list 'L' of weight vectors, one for each ray of the Fan 'F'
--  OUTPUT : 'tvb',  a ToricVectorBundleKaneyama or ToricVectorBundleKlyachko
-- COMMENT : If no option is given the function will return a ToricVectorBundleKlyachko, if "Type" => "Kaneyama" is given it returns a ToricVectorBundleKaneyama
weilToCartier (List,Fan) := opts -> (L,F) -> (
     rl := toList F#"rays";
     -- Checking for input errors
     if #L != #rl then error("The number of weights must equal the number of rays.");
     n := ambDim F;
     if opts#"Type" == "Kaneyama" then (
	  if not isPure F or ambDim F != dim F then error("The Fan must be pure of maximal dimension.");
     	  -- Creating 0 matrices to compute interssection of hyperplanes to  compute the degrees
	  Mfull := matrix {toList(n:0)};
	  vfull := matrix {{0}};
	  -- Checking for further errors and assigning the weights to the rays
	  L = hashTable apply(#rl, i -> (if class L#i =!= ZZ then error("The weights must be in ZZ."); rl#i => L#i));
	  -- Keeping track of the lowest common multiple of denominators of the degrees,
	  -- to check wether the divisor itself is Cartier or which multiple
	  denom := 1;
	  --lcm := (a,b) -> (substitute((a*b)/gcd(a,b),ZZ));
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
			 if (transpose rC)*w != v then error("The weights do not define a Cartier divisor."));
		    -- Check if w is QQ-Cartier
		    scan(flatten entries w, e -> denom = lcm(denominator e ,denom));
		    w));
	  -- If the divisor is only QQ Cartier, then its replaced by its first Cartier multiple
	  if denom != 1 then print(toString(denom)|" times the divisor is Cartier, which is the output.");
	  gC = apply(gC, e -> substitute(denom*e,ZZ));
	  -- Construct the actual line bundle
	  addDegrees(tvb,gC))
     else if opts#"Type" == "Klyachko" then (
	  if any(L, l -> not instance(l,ZZ)) then error("The weights must be in ZZ.");
	  T := makeVBKlyachko(1,F,apply(L, l -> matrix{{1_QQ}}),apply(L, l -> matrix{{-l}}));
	  if not isVectorBundle T then error("The weights do not define a Cartier divisor.") else T)
     else error("Type must be Klyachko or Kaneyama"))



-- PURPOSE : Constructing the fan of projective n-space
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The fan of projective n-space
projectiveSpaceFan = method(TypicalValue => Fan)
projectiveSpaceFan ZZ := n -> (
     if n < 1 then error("The dimension must be strictly positive");
     normalFan convexHull (map(ZZ^n,ZZ^n,1)|map(ZZ^n,ZZ^1,0)))


-- PURPOSE : Constructing the fan of the product of n projective 1-spaces
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The fan of the product of n projective 1-spaces
pp1ProductFan = method(TypicalValue => Fan)
pp1ProductFan ZZ := n -> (
     if n < 1 then error("The number of PP^1's must be strictly positive");
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
     if numRows u != T#"dimension of the variety" or numColumns u != 1 then error("The degree matrix must be a vector of variety dimension");
     if ring u =!= ZZ then error("The degree must be an integer vector");
     if k < -1 or T#"dimension of the variety"+1 < k then error("k must be between 0 and the variety dimension for the k-th cohomolgy");
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
				   Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#2)), j -> (j <= e#1)),e#2));
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
     if numRows u != tvb#"dimension of the variety" or numColumns u != 1 then error("The degree matrix must be a vector of variety dimension");
     if ring u =!= ZZ then error("The degree must be an integer vector");
     if k < 0 or tvb#"dimension of the variety"+1 < k then error("k must be between 0 and the variety dimension for the k-th cohomolgy");
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
	       M20 = hashTable apply(subsets(l,k+1), cl -> (
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
	  M21 = tvb.cache.cech#(k,u);
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


-- PURPOSE : Computing the cohomology of a given ToricVectorBundleKaneyama
cohom = method()


--   INPUT : '(k,tvb,u)',  'k' for the 'k'th cohomology group, 'tvb' a ToricVectorBundleKaneyama, and 'u' the degree
--  OUTPUT : 'ZZ',	     the dimension of the degree 'u' part of the 'k'th cohomology group of 'tvb'
cohom (ZZ,ToricVectorBundleKaneyama,Matrix,List) := (k,tvb,u,Ladsfg) -> (
     if not tvb.cache.?HH then tvb.cache.HH = new MutableHashTable;
     if not tvb.cache.HH#?(k,u) then (
	  -- Checking for input errors
     	  if numRows u != tvb#"dimension of the variety" or numColumns u != 1 then error("The degree matrix must be a vector of variety dimension");
     	  if ring u =!= ZZ then error("The degree must be an integer vector");
     	  if k < 0 or tvb#"dimension of the variety" < k then error("k must be between 0 and the variety dimension for the k-th cohomolgy");
     	  -- Extracting the neccesary data
     	  rk := tvb#"rank of the vector bundle";
     	  l := tvb#"number of affine charts";
     	  tCT := sort keys tvb#"topConeTable";
     	  bCT := tvb#"baseChangeTable";
     	  dT := tvb#"degreeTable";
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
     	  local M1;
     	  local M2;
     	  -- For the 'k'th cohomology group one needs all intersections of 'k', of 'k+1', and of 'k+2' charts of the covering
     	  -- given by the maximal dimensional cones which are 'l' many, if 'k' is strictly positive. 
     	  if k > 0 then (
	       -- Collecting all intersections of 'k' top cones together with the degrees 'd' of the first cone which satisfy
	       -- u-d in the dual cone of the intersection
	       M1 = subsets(l,k);
	       M1 = hashTable apply(M1, cl -> (
		    	 C := intersection apply(cl, i -> tCT#i);
		    	 degs := dT#(tCT#(cl#0));
		    	 L := select(toList(0..rk-1), i -> contains(dualCone C,u - degs_{i}));
		    	 cl => (L,C)));
	       -- Collecting all intersections of 'k+1' top cones together with the degrees 'd' of the first cone which satisfy
	       -- u-d in the dual cone of the intersection by taking each intersection of M1 and adding one cone
	       M2 = {};
	       for p in pairs M1 do (
		    L := select(toList(0..rk-1), i -> not member(i,p#1#0));
		    for i from last(p#0)+1 to l-1 do (
			 cl := append(p#0,i);
			 C := intersection(p#1#1,tCT#i);
			 degs := dT#(tCT#(cl#0));
			 M2 = append(M2,cl => (sort unique join(p#1#0,select(L, i -> contains(dualCone C,u- degs_{i}))),C))));
	       M2 = hashTable M2;
	       M1 = hashTable apply(pairs M1, p -> p#0 => p#1#0))
     	  -- Otherwise take only the 'k+1' and 'k+2' intersections
     	  else (
	       -- The same as above
	       M2 = subsets(l,k+1);
	       M2 = hashTable apply(M2, cl -> (
		    	 C := intersection apply(cl, i -> tCT#i);
		    	 degs := dT#(tCT#(cl#0));
		    	 L := select(toList(0..rk-1), i -> contains(dualCone C,u - degs_{i}));
		    	 cl => (L,C))));
     	  -- Collecting all intersections of 'k+2' top cones together with the degrees 'd' of the first cone which satisfy
     	  -- u-d in the dual cone of the intersection by taking each intersection of M2 and adding one cone
     	  M3 := {};
     	  for p in pairs M2 do (
	       L := select(toList(0..rk-1), i -> not member(i,p#1#0));
	       for i from last(p#0)+1 to l-1 do (
		    cl :=append(p#0,i);
		    C := intersection(p#1#1,tCT#i);
		    degs := dT#(tCT#(cl#0));
		    M3 = append(M3,cl => sort unique join(p#1#0,select(toList(0..rk-1), i -> contains(dualCone C,u - degs_{i}))))));
     	  M3 = hashTable M3;  
     	  M2 = hashTable apply(pairs M2, p -> p#0 => p#1#0);
     	  -- Constructing the zero map over QQ
     	  d1 := map(QQ^0,QQ^0,0);
     	  -- Constructing the matrix of the sequence for the cohomology
     	  if k > 0 then (
	       scan(pairs M1, (a,b) -> (
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
	       		 else d1 = d1 | A)));
     	  -- constructing d2 in the same way as d1
     	  d2 := map(QQ^0,QQ^0,0);
     	  scan(pairs M2, (a,b) -> (
	       	    A := map(QQ^0,QQ^(#b),0);
	       	    scan(pairs M3, (c,d) -> (
			      if isSubset(a,c) then (
			      	   signum := (-1)^(#c - position(c, e -> not member(e,a))-1);
			      	   i := a#0;
			      	   j := c#0;
			      	   if i == j then A = A || (signum * (map(QQ^rk,QQ^rk,1))_b)
			      	   else (
				   	mpath := findpath(i,j,keys bCT);
				   	if #mpath == 1 then (
					     if i < j then A = A || (signum * (bCT#(i,j))_b)
					     else A = A || (signum * (inverse bCT#(j,i))_b))
				   	else (
					     A1 := map(QQ^rk,QQ^rk,1);
					     for p in mpath do (
						  if p#0 < p#1 then A1 = bCT#p * A1
						  else A1 = (inverse(bCT#(p#1,p#0)))*A1);
					     if  i < j  then bCT = hashTable join(apply(pairs bCT, ps -> ps#0 => ps#1), {(i,j) => A1})
					     else bCT = hashTable join(apply(pairs bCT, ps -> ps#0 => ps#1), {(j,i) => inverse A1});
					     A = A || (signum * A1_b))))
			      else A = A || map(QQ^rk,QQ^(#b),0)));
	       	    if d2 == map(QQ^0,QQ^0,0) then d2 = A
	       	    else d2 = d2 | A));
     	  d := if k == 0 then rank ker d2 else (rank ker d2 - rank image d1);
     	  tvb.cache.HH#(k,u) = (gradedRing tvb)^(toList(d:flatten entries(-u))));
     tvb.cache.HH#(k,u))

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
	  T.cache.HH#(k,u) = (gradedRing T)^(toList(d:flatten entries(-u))));
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
     	  T.cache.HH#(k,u) = (gradedRing T)^(toList(d:flatten entries(-u))));
     T.cache.HH#(k,u))



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
--   INPUT : '(k,F)',  a strictly psoitive integer 'k' and a pure and full dimensional
--                     Fan 'F' 
--  OUTPUT : The ToricVectorBundleKaneyama 'VB'
makeVBKaneyama = method(TypicalValue => ToricVectorBundleKaneyama)
makeVBKaneyama (ZZ,Fan) := (k,F) -> (
     -- Checking for input errors
     if k < 1 then error("The vector bundle must have a positive rank");
     if not isPure F then error("Fan must be of pure dimension");
     if dim F != ambDim F then error("Fan must be of full dimension");
     if not isPointed F then error("The Fan must be pointed");
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
--   INPUT : '(k,F)',  a strictly psoitive integer 'k' and a pure and full dimensional Fan 'F' 
--  OUTPUT : The ToricVectorBundleKlyachko 'VB'
makeVBKlyachko = method(TypicalValue => ToricVectorBundleKlyachko)
makeVBKlyachko (ZZ,Fan) := (k,F) -> (
     -- Checking for input errors
     if k < 1 then error("The vector bundle must have a positive rank");
     if not isPointed F then error("The Fan must be pointed");
     -- Writing the table of rays
     rT := toList F#"rays";
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
     Rmatrix * (matrix apply(numRows R1, i -> F1#i / R1_(i,i)) || map(QQ^(numColumns R1 - numRows R1),QQ^(#(F1#0)),0)))


-- PURPOSE : Generating the table of all rays together with their filtration 
--   INPUT : 'T',  a ToricVectorBundleKlyachko
--  OUTPUT : a hashTable,  with keys the rays of the variety and for each ray a list of pairs (the filtration step, the filtration)
tableForAllRays = method(TypicalValue => HashTable)
tableForAllRays ToricVectorBundleKlyachko := T -> (
     if not T.cache.?allRaysTable then (
	  fMT := T#"filtrationMatricesTable";
     	  bT := T#"baseTable";
	  T.cache.allRaysTable = hashTable apply(rays T, r -> (
		    fT := flatten entries fMT#r;
	       	    r => apply(fT, e -> (e,(bT#r)_(positions(fT, i -> i <= e)))))));
     T.cache.allRaysTable)



---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     	Key => ToricVectorBundles,
	Headline => "for cohomology computations of equivariant vector bundles on toric varieties",
	
	"Using the descriptions of Kaneyama and Klyachko this package implements the construction of
	equivariant vector bundles on toric varieties.",
	
	PARA{}, "Note that this package only implements vector bundles in Kaneyama's description over 
	pure and full dimensional fans.",
	
	PARA{}, TT "ToricVectorBundles", " uses the ", TO Polyhedra, " package by ", 
	HREF("http://page.mi.fu-berlin.de/rbirkner/", "René Birkner"), ". At least version 1.1 
	of ",TO Polyhedra," must be installed via ",TO installPackage," to use ",TT "ToricVectorBundles",".",
	
	PARA{}, "Each vector bundle is saved either in the description of Kaneyama or the one of Klyachko. The 
	first description gives the multidegrees (in the dual lattice of the fan) of the generators of the bundle 
	over each full dimensional cone, and for each codim 1 cone a transition matrix 
	(See ",TO ToricVectorBundleKaneyama,"). The description by Klyachko has for each ray of the fan a 
	filtration of the vector bundle (See ",TO ToricVectorBundleKlyachko,").",
	
	PARA{}, "For the mathematical background see ",
	
	UL {
	     {EM "Tamafumi Kaneyama, On equivariant vector bundles on an almost homogeneous variety", ", Nagoya Math. J. 57, 1975 and "},
	     {EM "A. A. Klyachko, Equivariant bundles over toral varieties", ", Izv. Akad. Nauk SSSR Ser. Mat., 53, 1989."}
	   }
	
	}
   
document {     
     Key => ToricVectorBundleKaneyama,
     Headline => "the class of all toric vector bundles in Kaneyama's description",
     
     "A toric vector bundle of rank ",TT "k"," in Kaneyama's description is given in the following way: 
     First since only bundles for pure and full dimensional fans are implemented, the maximal dimensional cones 
     are the generating cones of the fan. So if the dimension of the fan is ",TT "n"," then there is an ",TT "n"," 
     times ",TT "k"," matrix over ",TO ZZ," assigned to each of these cones, which gives ",TT "k"," degree vectors 
     in the lattice of the fan, one for each generator of the bundle. Additionally, 
     for every pair of maximal cones intersecting in a common codimension 1 face, there is a matrix in 
     GL(",TT "k",",",TO QQ,"), representing the base change between these two affine charts. The output of 
     a ToricVectorBundleKaneyama gives an overview of the characteristics of the bundle:",
     
     EXAMPLE {
	  " E = cotangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")"
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,".",
     
     EXAMPLE {
	  " details E"
	  }
     
     }

document {     
     Key => ToricVectorBundleKlyachko,
     Headline => "the class of all toric vector bundles in Klyachko's description",
     
     "A toric vector bundle of rank ",TT "k"," in Klyachko's description is given in the following way: 
     There is  a matrix in GL(",TT "k",",R), where R is ",TO QQ," or another field, assigned to each of the rays, which gives 
     the basis of the filtered vector space associated to this ray. Additionally, for every ray there is a ",TT "1"," times ",TT "k"," matrix 
     over ",TO ZZ,", the filtration matrix, that determines the filtration on the basis given before. I.e. if the j-th entry 
     of the filtration matrix is i then the j-th basis vector appears in the filtration at the i-th step. The output of a 
     ToricVectorBundleKlyachko gives an overview of the characteristics of the bundle:",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3"
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,".",
     
     EXAMPLE {
	  " details E"
	  }
     
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
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be in GL(k,",TO ZZ,") or GL(k,",TO QQ,"), 
     where k is the rank of the vector bundle ",TT "T",". The list has to contain one matrix for each maximal 
     dimensional cone of the underlying Fan over which ",TT "E"," is defined (The fan can be recovered 
     with ",TO (fan,ToricVectorBundleKaneyama),"). The vector bundle already has a 
     list of pairs (i,j) denoting the codim 1 intersections of two maximal cones with i<j and they are ordered in 
     lexicographic order. The matrices will be assigned to the pairs (i,j) in that order. To see which codimension 1 cone 
     corresponds to the pair (i,j) use ",TO (codim1Table,ToricVectorBundleKaneyama),". The matrix A assigned to 
     (i,j) denotes the transition (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)*A. The matrices must not satisfy the regularity 
     or the cocycle condition. These can be checked with ",TO regCheck," and ",TO cocycleCheck,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E",
	  " F = addBaseChange(E,{matrix{{1,2},{0,1}},matrix{{1,0},{3,1}},matrix{{1,-2},{0,1}},matrix{{1,0},{-3,1}}})",
	  " details F",
	  " cocycleCheck F"
	  }
     
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
     
     PARA{}, TT "addDegrees"," replaces the degree matrices in ",TT "E"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be ",TT "n"," by ",TT "k"," matrices over ",TO ZZ,", where ",TT "k"," is the rank of the vector 
     bundle ",TT "E"," and ",TT "n"," is the dimension of the underlying toric variety. The list has to contain one matrix for 
     each maximal dimensional cone of the underlying Fan over which ",TT "E"," is defined. Note that in ",TT "E"," the top 
     dimensional Cones are already sorted and that the degree matrices in ",TT "L"," will be assigned to the Cones in that 
     order. To find out the order use ",TO maxCones,". The matrices must not satisfy the regularity condition. This can be 
     checked with ",TO regCheck,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E",
	  " F = addDegrees(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details F",
	  " regCheck F"
	  }
     
     }

document {
     Key => {addBase, (addBase,ToricVectorBundleKlyachko,List)},
     Headline => "changing the basis matrices of a toric vector bundle in Klyachko's description",
     Usage => "F = addBase(E,L)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "F" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "addBase"," replaces the basis matrices in ",TT "E"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be in GL(",TT "k",",R), where ",TT "k"," is the rank of the vector bundle ",TT "E",". The list has 
     to contain one matrix for each ray of the underlying Fan over which ",TT "E"," 
     is defined. Note that in ",TT "E"," the rays are already sorted and that the basis matrices in ",TT "L"," will be assigned to the 
     rays in that order. To see the order use ",TT "rays E",".",
     
     PARA{}, "The matrices need not necessarily satisfy the compatability condition. This can be checked with ",TO isVectorBundle,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " details E",
	  " F = addBase(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details F",
	  " isVectorBundle F"
	  }
     
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
     
     PARA{}, TT "addFiltration"," replaces the filtration matrices in ",TT "E"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be ",TT "1"," by ",TT "k"," matrices over ",TO ZZ,", where ",TT "k"," is the rank of the vector 
     bundle ",TT "E",". The list has to contain one matrix for each ray of the underlying Fan over which ",TT "E"," is defined. 
     Note that in ",TT "E"," the rays are already sorted and that the filtration matrices in ",TT "L"," will be assigned to the 
     rays in that order. To see the order, use ",TT "rays E",".",
     
     PARA{}, "The filtration on the vector bundle over a ray is given by the filtration matrix for this ray in the following way. The 
     first index j such that the i-th basis vector in the basis over this ray appears in the j-th filtration is the i-th entry of 
     the filtration matrix. The matrices must not satisfy the compatability condition. This can be checked with ",TO isVectorBundle,".",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " details E",
	  " F = addFiltration(E,{matrix{{1,3}},matrix{{-1,3}},matrix{{2,-3}},matrix{{0,-1}}})",
	  " details F",
	  " isVectorBundle F"
	  },
     
     PARA{}, "This means that for example over the first ray the first basis vector of ",TT "F"," appears at 1 and the second at 3"
     
     }

document {
     Key => {base, (base,ToricVectorBundleKlyachko)},
     Headline => " returns the basis matrices for the rays",
     Usage => " b = base E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "b" => HashTable
	  },
     
     PARA{}, "The basis of the toric vector bundle in Klyachko's description is given for each ray as matrix square 
     matrix of rank of the bundle. The output is a ",TO HashTable," where the keys are the rays of the fan given as one 
     column matrices over ",TO ZZ,", and for each ray a k by k matrix over R where R is the field over which the bundle 
     is defined and k is the rank of the bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " base E"
	  }
     
     }

document {
     Key => {cocycleCheck, (cocycleCheck,ToricVectorBundleKaneyama)},
     Headline => " checks if the toric vector bundle fulfills the cocycle condition",
     Usage => " b = cocycleCheck E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama 
	  },
     Outputs => {
	  "b" => Boolean
	  },
     
     PARA{}, "The transition matrices in ",TT "E"," define an equivariant toric vector bundle if they satisfy the cocycle condition. 
     I.e. for every codimension 2 cone of the fan the cycle of transition matrices of codimension 1 cones containing the codimension 2 
     cone gives the identity when multiplied.",
     
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
	  }
     
     }

document {
     Key => {details, (details,ToricVectorBundleKaneyama), (details,ToricVectorBundleKlyachko)},
     Headline => " returns the details of a toric vector bundle (for both descriptions)",
     Usage => " ht = details E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "ht" => Sequence => {" or ",TO HashTable," if the bundle is in Klyachko's description"}
	  },
     
     PARA{}, "For a toric vector bundle in Kaneyama's description, the sequence ",TT "ht"," contains a hash table that assigns to each maximal 
     cone its matrix of rays and its matrix of degrees, and a hash table giving a transition matrix for every pair of maximal cones 
     that intersect in a codimension 1 face.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  "details E"
	  },
     
     PARA{}, "For a toric vector bundle in Klyachko's description, the hash table ",TT "ht"," contains the rays of the 
     fan and for each ray the basis of the bundle over this ray and the filtration matrix.",
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  "details E"
	  }
     
     }

document {
     Key => {filtration, (filtration,ToricVectorBundleKlyachko)},
     Headline => " returns the filtration matrices of the vector bundle",
     Usage => " f = filtration E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "f" => HashTable
	  },
     
     PARA{}, "For each ray of the fan there is a filtration matrix. If the bundle has rank k then this 
     is a one row matrix over ZZ with k entries. This defines the filtration on the corresponding base matrix 
     (see ",TO base,") such that the j-th filtration is generated by all columns of the base matrix for which 
     the entry in the same column of the matrix less or equal to j is.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " filtration E"
	  },
     
     PARA{}, "So in this example for each ray the first column of the basis appears at -1 and the second at 0." 
     
     }

document {
     Key => {toricVectorBundle, (toricVectorBundle,ZZ,Fan)},
     Headline => " generates the trivial bundle of rank k for a given fan",
     Usage => " E = toricVectorBundle(k,F)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed Fan ",TT "F"," the function ",TT "toricVectorBundle"," generates the trivial toric vector 
     bundle of rank ",TT "k",".",
     
     PARA{}, "If no further options are given then the resuting bundle will be in Klyachko's description:
     The basis assigned to every ray is the standard basis of ",TO QQ,"^k and the filtration is given by ",TT "0"," for all 
     ",TT "i<0"," and ",TO QQ,"^k for ",TT "i>=0",".",
     
     EXAMPLE{
	  " E = toricVectorBundle(2,projectiveSpaceFan 2)",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description: The degree vectors of this bundle are all zero vectors and the transition matrices are all the identity.",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " details E"
	  
	  }
     
     } 

document {
     Key => (toricVectorBundle,ZZ,Fan,List,List),
     Headline => " generates the trivial bundle of rank 'k' for a given fan in Klyachko's description",
     Usage => " E = toricVectorBundle(k,F,L1,L2)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"},
	  "L1" => List,
	  "L2" => List
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed Fan ",TT "F"," the function ",TT "toricVectorBundle"," generates the toric vector bundle 
     of rank ",TT "k"," given by the data in the two lists ",TT "L1"," and ",TT "L2",".",
     
     PARA{}, "If no further options are given then the resuting bundle will be in Klyachko's description: The first list ",TT "L1"," will give the 
     basis matrices and the second list ",TT "L2"," will give the filtration matrices. Then the resulting vector bundle will have these basis and 
     filtration matrices. The number of matrices in ",TT "L1"," must match the number of rays of the Fan and they must be in GL(",TT "k",",R) for 
     R=",TO ZZ," or ",TO QQ,". They will be assigned to the rays in the order they appear in ",TT "rays F",". The number of  matrices in ",TT "L2"," 
     must also match the number of rays , and they must be ",TT "1"," times ",TT "k"," matrices over ",TO ZZ,". The assignment order is the same as 
     for the basis matrices. Note that the basis and filtration matrices that are given to the function must not satisfy the compatability condition. 
     This can by checked by using ",TO regCheck,".",
     
     EXAMPLE {
	  " L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}",
	  " L2 = {matrix {{-1,0}},matrix{{-2,-1}},matrix{{0,1}}}",
	  " E = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2)",
	  " details E"
	  },
     
     PARA{}, "If the option ",TT "\"Type\" => \"Kaneyama\""," is given then the resulting bundle will be in 
     Kaneyama's description: The first list ",TT "L1"," will give the degree matrices and the second list ",TT "L2"," will give the transition 
     matrices. The number of matrices in ",TT "L1"," must match the number of maximal cones of the Fan and they must be ",TT "n"," times ",TT "k"," 
     matrices over ",TO ZZ,". They will be assigned to the cones in the order they appear in ",TT "maxCones F",". The number of 
     matrices in ",TT "L2"," must match the number of pairs of maximal cones that intersect in a common codimension 1 face and must all be in 
     GL(",TT "k",",",TO QQ,"). They will be assigned to the pairs (i,j) in lexicographic order. Note that the degrees and transition matrices that are 
     given to the function must not satisfy the regularity or the cocycle condition. These can by checked by using ",TO regCheck," 
     and ",TO cocycleCheck,".",
     
      EXAMPLE {
	  " L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}",
	  " L2 = {matrix {{-1,0},{0,-1}},matrix{{0,1},{1,0}},matrix{{0,-1},{-1,0}}}",
	  " E = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2,\"Type\" => \"Kaneyama\")",
	  " details E"
	  }
     
     } 

document {
     Key => {regCheck, (regCheck,ToricVectorBundleKaneyama)},
     Headline => " checking the regularity condition for a toric vector bundle",
     Usage => " b = regCheck E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     Outputs => {
	  "b" => Boolean
	  },
     
     PARA{}, "For a toric vector bundle in Kaneyama's description, the regularity condition means that for every pair of maximal cones 
     intersecting in a common codimension 1 face, the two sets of degrees and the transition matrix fulfill the regularity condition. I.e. 
     for every i and j we have that the (i,j) entry of the matrix is 0 or the difference of the i-th degree vector of one cone and the j-th 
     degree vector of the other cone is in the dual cone of the intersection. Note that this is only necessary for toric vector bundles 
     generated 'by hand' using ",TO addBaseChange," and ",TO addDegrees," since bundles generated for example by ",TO tangentBundle," satisfy 
     the condition autmatically.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " regCheck E"
	  }
     
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
	  "b" => Boolean
	  },

     PARA{}, TT "E"," and ",TT "F"," must be vector bundles over the same fan. The method then returns 
     whether or not the bundles are isomorphic.",

     EXAMPLE {
	  " HF = hirzebruchFan 2",
	  " E = extPower(2, cotangentBundle HF)",
	  " F = weilToCartier({-1,-1,-1,-1},HF)",
	  " areIsomorphic(E,F)"
	  },
     
     PARA{}, "to obtain the isomorphism, if two bundles are isomorphic use ",TO isomorphism,"."
     
     }

document {
     Key => {charts, (charts,ToricVectorBundleKaneyama), (charts,ToricVectorBundleKlyachko)},
     Headline => " returning the number of maximal affine charts",
     Usage => " n = charts E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "n" => ZZ
	  },
     
     PARA{}, TT "charts"," returns the number of maximal cones in the fan, i.e. the number of affine charts.",
     
     EXAMPLE {
	  " E = cotangentBundle pp1ProductFan 3",
	  " charts E"
	  }
     
     }

document {
     Key => {cotangentBundle, (cotangentBundle,Fan)},
     Headline => " generates the cotangent bundle",
     Usage => " E = cotangentBundle F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, "If the Fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the cotangent bundle of the 
     toric variety given by ",TT "F",". If no further options are given then the resuting bundle will be in Klyachko's description:",
     
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
	  }
     
     }

document {
     Key => {deltaE, (deltaE,ToricVectorBundleKaneyama), (deltaE,ToricVectorBundleKlyachko)},
     Headline => " computes the polytope of possible degrees that give non zero cohomology",
     Usage => " P = deltaE E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "For a toric vector bundle there is a finite set of degrees 'u' such that the degree 'u' part of the cohomology of the 
     vector bundle is not zero. This function computes a polytope, called ",TT "deltaE",", such that these degrees are contained in this 
     polytope.",
     
     EXAMPLE {
	  " E = toricVectorBundle(2,pp1ProductFan 2, \"Type\" => \"Kaneyama\")",
	  " P = deltaE E",
	  " vertices P",
	  " E1 = tangentBundle projectiveSpaceFan 2",
	  " P1 = deltaE E1",
	  " vertices P1"
	  }
     
     }

document {
     Key => {(dim,ToricVectorBundleKaneyama), (dim,ToricVectorBundleKlyachko)},
     Headline => " returns the dimension of the underlying toric variety",
     Usage => " d = dim E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, "Returns the dimension of the underlying toric variety.",
     
     EXAMPLE {
	  " E = weilToCartier({1,2,3,4},projectiveSpaceFan 3)",
	  " dim E"
	  }
     }

document {
     Key => {dsum, (dsum,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama), (dsum,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " computes the direct sum of two vector bundles",
     Usage => " E = dsum(E1,E2)",
     Inputs => {
	  "E1" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "E2" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, TT "dsum"," computes the direct sum of two toric vector bundles if they are defined over the same fan. Note that the two 
     vector bundles must either both be in Kaneyama's description or both in Klyachko's description.",
     
     PARA{}, "You can also use ",TO (symbol ++,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama)," 
     and ",TO (symbol ++,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko),".",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = dsum(E1,E2)",
	  " details E"
	  }
     
     }

document {
     Key => (dual,ToricVectorBundleKaneyama),
     Headline => " computes the dual bundle of a toric vector bundle",
     Usage => " Ed = dual E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     Outputs => {
	  "Ed" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, TT "dual"," computes the dual vector bundle of a toric vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " Ed = dual E",
	  " Ed == cotangentBundle(pp1ProductFan 2,\"Type\" => \"Kaneyama\")"
	  }
     
     }

document {
     Key => (dual,ToricVectorBundleKlyachko),
     Headline => " computes the dual bundle of a toric vector bundle",
     Usage => " Ed = dual E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "Ed" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "dual"," computes the dual vector bundle of a toric vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle projectiveSpaceFan 2",
	  " Ed = dual E",
	  " Ed == cotangentBundle projectiveSpaceFan 2"
	  }
     
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
	  "b" => Boolean => {TT "true"," if there exists a decomposition ",TT "false"," otherwise"}
	  },
     
     PARA{}, "The list ",TT "L"," must have one entry for each maximal cone in the underlying fan of ",TT "E",". If the rank of 
     the bundle is k and the ambient dimension of the variety is n then each entry must either be a n by k matrix over ",TO ZZ," 
     or a list of these. Then it checks for each maximal cone in the fan (given in the order 
     of ",TO (maxCones,ToricVectorBundleKlyachko),") if any of the matrices in the corresponding entry in ",TT "L"," these 
     weight vectors admit a decomposition of the bundle into torus eigenspaces. See ",
     HREF("http://math.stanford.edu/~sampayne/", "Sam Payne's"), " ", EM "Moduli of toric vector bundles", ", 
     Compositio Math. 144, 2008. Lemma 3.5.",
     
     PARA{}, "One can for example use the output of the function ",TO findWeights,".",
     
     EXAMPLE{
	  " E = tangentBundle projectiveSpaceFan 3",
	  " L = findWeights E",
	  " existsDecomposition(E,L)"
	  },
     
     PARA{}, "Note that the data given in the description of ",TT "E"," defines an equivariant vector bundle on the toric variety 
     exactly if there exists a set of weight vectors for each maximal cone that admits a decomposition. The function ",TO isVectorBundle," 
     uses this."
     
     }  
     

document {
     Key => {extPower, (extPower,ZZ,ToricVectorBundleKaneyama), (extPower,ZZ,ToricVectorBundleKlyachko)},
     Headline => " computes the l-th exterior power of a toric vector bundle",
     Usage => " Ee = extPower(l,E)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "Ee" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, TT "extPower"," computes the ",TT "l","-th exterior power of a toric vector bundle in each description. The resulting 
     bundle will be given in the same description as the original bundle. ",TT "l"," must be strictly positive and at most the rank 
     of the bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " details E",
	  " Ee = extPower(2,E)",
	  " details Ee"
	  }
     
     }

document {
     Key => {(fan,ToricVectorBundleKaneyama), (fan,ToricVectorBundleKlyachko)},
     Headline => " returns the underlying fan of the toric vector bundle",
     Usage => " F = fan E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Returns the fan of the underlying toric variety. This is an object of the package Polyhedra.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " F = fan E",
	  " rays F"
	  }
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
     
     PARA{}, "The list ",TT "L"," contains a list for each maximal cone of the underlying fan. For each 
     maximal cone this list contains all matrices of possible weight vectors, that induce the filtrations 
     on the rays of this cone (modulo permutations, but yet not all permutations). This means that for one of these 
     matrices M multplied with the matrix of rays of this cone R (the rays are the rows) gives the matrix of 
     filtrations of these rays (where for each filtration the entries may be permuted).",
     
     EXAMPLE{
	  " E = tangentBundle projectiveSpaceFan 3",
	  " findWeights E"
	  }
     }

document {
     Key => {gradedRing, (gradedRing,ToricVectorBundleKaneyama), (gradedRing,ToricVectorBundleKlyachko)},
     Headline => " returns the graded ring of the bundle",
     Usage => " R = gradedRing E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "R" => Ring
	  },
     
     PARA{}, "For a vector bundle in Kaneyama's description the graded ring is ",TO QQ," with degree space the 
     lattice of the underlying fan.",
     
     EXAMPLE{
	  " E = tangentBundle(projectiveSpaceFan 3,\"Type\" => \"Kaneyama\")",
	  " gradedRing E"
	  },
     
     PARA{}, "for a vector bundle in Klyachko's description the graded ring is the ring over which the 
     bundle is defined with degree space the lattice of the underlying fan.",
     
     EXAMPLE{
	  " E = toricVectorBundle(1,projectiveSpaceFan 2, toList(3:matrix{{1.}}),toList(3:matrix{{-1}}))",
	  " gradedRing E"
	  }
     }

document {
     Key => {(hh,ZZ,ToricVectorBundleKaneyama), (hh,ZZ,ToricVectorBundleKlyachko)},
     Headline => " computes the rank of the i-th cohomology group",
     Usage => " d = hh^i E \nd = hh^i (E,u)",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "u" => {ofClass Matrix ,", over ",TO ZZ,", giving a point in the lattice of the fan"}
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, TT "hh^i"," computes the rank of the i-th cohomology group. If no further argument is given then it returns the rank of 
     the complete cohomology group. If in addition a one column matrix ",TT "u"," over ",TO ZZ," is given it returns the rank of the 
     degree ",TT "u"," part of the cohomology group.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " u = matrix{{0},{0}}",
	  " hh^0 (E,u)",
	  " hh^0 E"
	  }
     
     }
     

document {
     Key => {isGeneral, (isGeneral,ToricVectorBundleKlyachko)},
     Headline => " checks whether a toric vector bundle is general",
     Usage => " b = isGeneral E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "b" => Boolean
	  },
     
     PARA{}, "A toric vector bundle in Klyachko's description is general if for every maximal cone C in the fan the following condition holds: Let 
     r1,...,rl be the rays of C. Then for every choice of filtration steps i_1,...,i_l for each ray, i.e. choose an integer for each ray where 
     the filtration enlarges, the equation",
     
     PARA{}, "codim \\bigcap E^rj(i_j) = min {\\sum codim E^rj(i_j),rank E}",
     
     PARA{}, "holds.",
     
     EXAMPLE {
	  " E = cotangentBundle hirzebruchFan 2",
	  " isGeneral E"
	  }
     }

document {
     Key => {isomorphism, (isomorphism,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " returns the isomorphism if the two bundles are isomorphic",
     Usage => " M = isomorphism(E,F)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "F" => ToricVectorBundleKlyachko
	  },
     Outputs => {
	  "M" => Matrix => {"over the ring over which the two bundles are defined"}
	  },
     
     PARA{}, "If the two bundles are isomorphic this function returns the isomorphism.",
     
     EXAMPLE{
	  " HF = hirzebruchFan 2",
	  " E = extPower(2, cotangentBundle HF)",
	  " F = weilToCartier({-1,-1,-1,-1},HF)",
	  " M = isomorphism(E,F)"
	  }
     }

document {
     Key => {isVectorBundle, (isVectorBundle,ToricVectorBundleKlyachko), (isVectorBundle,ToricVectorBundleKaneyama)},
     Headline => " checks if the data in fact defines an equivariant toric vector bundle",
     Usage => " b = isVectorBundle E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "b" => Boolean => {TT "true"," if ",TT "E"," defines a bundle, ",TT "false"," otherwise"}
	  },
     
     PARA{}, "If ",TT "E"," is in Klyachko's description then the data in ",TT "E"," defines an equivariant toric vector on 
     the toric variety if and only if for each maximal cone exists a decomposition into torus eigenspaces of the bundle. See ",
     HREF("http://math.stanford.edu/~sampayne/", "Sam Payne's"), " ", EM "Moduli of toric vector bundles", ", 
     Compositio Math. 144, 2008. Section 2.3. This uses the two functions ",TO findWeights," and ",TO existsDecomposition,".",
     
     EXAMPLE{
	  " E = toricVectorBundle(2,pp1ProductFan 2)",
	  " E = addBase(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " isVectorBundle E",
	  " F = toricVectorBundle(1,normalFan crossPolytope 3)",
	  " F = addFiltration(F,apply({2,1,1,2,2,1,1,2}, i -> matrix {{i}}))",
	  " isVectorBundle F"
	  },
     
     PARA{}, "If ",TT "E"," is in Kaneyama's description then data in ",TT "E"," defines an equivariant toric vector bundle on 
     the toric variety if and only if i satisfies the regularity and the cocycle condition (See ",TO cocycleCheck," and ",TO regCheck,").",
     
     EXAMPLE{
	  " E = toricVectorBundle(2,pp1ProductFan 2,\"Type\" => \"Kaneyama\")",
	  " isVectorBundle E",
	  " E = addBaseChange(E,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " isVectorBundle E"
	  }

     }

document {
     Key => {(maxCones,ToricVectorBundleKaneyama), (maxCones,ToricVectorBundleKlyachko)},
     Headline => " returns the list of maximal cones of the underlying fan",
     Usage => " L = maxCones E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "L" => List => {" of cones"}
	  },
     
     PARA{}, "Returns the list of maximal cones of the underlying fan. These are the cones that generate the fan, i.e. are not 
     a face of one another.",
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  " L = maxCones E",
	  " apply(L,rays)"
	  }
     }

document {
     Key => {randomDeformation, (randomDeformation,ToricVectorBundleKlyachko,ZZ), (randomDeformation,ToricVectorBundleKlyachko,ZZ,ZZ)},
     Headline => " generates a random deformation of a given toric vector bundle",
     Usage => " E1 = randomDeformation(E,h) \nE1 = randomDeformation(E,l,h)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "l" => ZZ => {"less than ",TT "h"},
	  "h" => ZZ
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, "For a bundle of rank k the function ",TT "randomDeformation"," replaces each base matrix by a random k by k matrix with entries
     between ",TT "l"," and ",TT "h",". ",TT "h", " must be greater than ",TT "l",". If ",TT "l"," is not given then the entries are between 0 
     and ",TT "h"," and then ",TT "h"," must be strictly positive.",
     
     EXAMPLE {
	  " E = tangentBundle pp1ProductFan 2",
	  " details E",
	  " E1 = randomDeformation(E,-2,6)",
	  " details E1"
	  }
     }

document {
     Key => {(rank,ToricVectorBundleKaneyama), (rank,ToricVectorBundleKlyachko)},
     Headline => " returns the rank of the vector bundle",
     Usage => " k = rank E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "k" => ZZ
	  },
     
     PARA{}, "Returns the rank of the toric vectro bundle.",
     
     EXAMPLE {
	  " E = tangentBundle projectiveSpaceFan 3",
	  " rank E"
	  }
     }

document {
     Key => {(rays,ToricVectorBundleKaneyama), (rays,ToricVectorBundleKlyachko)},
     Headline => " returns the rays of the underlying fan",
     Usage => " L = rays E",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "Returns the rays of the fan of the underlying toric variety as a list. Each ray is given as a one column matrix.",
     
     EXAMPLE {
	  " E = cotangentBundle projectiveSpaceFan 2",
	  " rays E"
	  },
     }
     

document {
     Key => {symmProd, (symmProd,ZZ,ToricVectorBundleKaneyama), (symmProd,ZZ,ToricVectorBundleKlyachko)},
     Headline => " computes the l-th symmetric product of a toric vector bundle",
     Usage => " Es = symmProd(l,E)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "Es" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, TT "symmProd"," computes the ",TT "l","-th symmetric product of a toric vector bundle in each description. The resulting 
     bundle will be given in the same description as the original bundle. ",TT "l"," must be strictly positive.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " details E",
	  " Es = symmProd(2,E)",
	  " details Es"
	  }
     
     }

document {
     Key => {tangentBundle, (tangentBundle,Fan)},
     Headline => " generates the tangent bundle",
     Usage => " E = tangentBundle F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "If the Fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the tangent bundle of the 
     toric variety given by ",TT "F",". If no further options are given then the resuting bundle will be in Klyachko's description:",
     
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
	  }
     
     }

document {
     Key => {tproduct, (tproduct,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama), (tproduct,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " computes the tensor product of two toric vector bundles",
     Usage => " E = tproduct(E1,E2)",
     Inputs => {
	  "E1" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "E2" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "If ",TT "E1"," and ",TT "E2"," are defined over the same fan and are in the same description, then ",TT "tproduct"," computes 
     the tensor product of the two vector bundles in this description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = tproduct(E1,E2)",
	  " details E"
	  }	  
     
     }

document {
     Key => {weilToCartier, (weilToCartier,List,Fan)},
     Headline => " generates the line bundle given by a Cartier divisor",
     Usage => " E = weilToCartier(L,F)",
     Inputs => {
	  "L" => List,
	  "F" => Fan
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, TT "F"," must be a pure and full dimensional Fan and ",TT "L"," must a list of weights, exactly one for each ray of the 
     fan. If the Weil divisor defined by these weights defines in fact a Cartier divisor, then ",TT "weilToCartier"," computes the 
     toric vector bundle associated to the Cartier divisor.",
     
     PARA{}, "If no further options are given then the resuting bundle will be in Klyachko's description:",
     
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
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundleKaneyama), (cohomology,ZZ,ToricVectorBundleKlyachko)},
     Headline => " computes the i-th cohomology group of a toric vector bundle",
     Usage => " c = HH^i E ",
     Inputs => {
	  "i" => ZZ,
	  "T" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "c" => Module
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology group of the toric vector bundle ",TT "E",". The output is the i-th cohomology group 
     as a multigraded module. If the option ",TT "Degree => 1"," it displays the number of degrees for which it computes the 
     cohomology. ",TT "i"," must be between 0 and the dimension of the underlying toric variety.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH^0 E",
	  " HH^0 (E,Degree => 1)"
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundleKaneyama,Matrix), (cohomology,ZZ,ToricVectorBundleKlyachko,Matrix)},
     Headline => " computes the i-th cohomology of a toric vector bundle in a given degree",
     Usage => " c = HH_i^E u ",
     Inputs => {
	  "i" => ZZ,
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "u" => Matrix => {"over ",TO ZZ," with just one column, giving a weight in the lattice"}
	  },
     Outputs => {
	  "c" => Module
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology group of the toric vector bundle ",TT "E"," of degree ",TT "u"," where ",TT "u"," must be a 
     one column matrix giving a point in the lattice of the Fan over which ",TT "E"," is defined and ",TT "i"," must be between 0 and 
     the dimension of the underlying toric variety.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH^0 (E,matrix{{1},{0}})"
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundleKaneyama,List), (cohomology,ZZ,ToricVectorBundleKlyachko,List)},
     Headline => " computes the i-th cohomology of a toric vector bundle for a given list of degrees",
     Usage => " c = HH_i^E L",
     Inputs => {
	  "i" => ZZ,
	  "E" => ToricVectorBundleKaneyama,
	  "L" => List => {" containing weights of the form one column matrix over ",TO ZZ}
	  },
     Outputs => {
	  "c" => List
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology of the toric vector bundle ",TT "E"," for a given list of degrees. ",TT "i"," must 
     be between 0 and the rank of the vector bundle. The entries of the list ",TT "L"," must be one column matrices each defining a point 
     in the lattice of the Fan over which ",TT "E"," is defined",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " HH_0^E {matrix{{1},{0}},matrix{{-1},{0}}}"
	  }
     
     }

document {
     Key => {eulerChi, (eulerChi,ToricVectorBundleKlyachko), (eulerChi,ToricVectorBundleKlyachko,Matrix),
	        (eulerChi,ToricVectorBundleKaneyama), (eulerChi,ToricVectorBundleKaneyama,Matrix)},
     Headline => " computes the Euler characteristic",
     Usage => " i = eulerChi E \neulerChi(E,u)",
     Inputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "u" => Matrix => {"with just one column over ",TO ZZ," representing a degree vector"}
	  },
     Outputs => {
	  "i" => ZZ
	  },
     
     PARA{}, "This function computes the Euler characteristic of a vector bundle if only the bundle is given. If in 
     addition a one column matrix over ",TO ZZ,", representing a degree vector, is given, it computes the Euler characteristic 
     of the vector bundle of that degree.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3",
	  " u = matrix {{0},{0}}",
	  " eulerChi(E,u)",
	  " eulerChi E"
	  },
     
     EXAMPLE {
	  " E = tangentBundle(hirzebruchFan 3,\"Type\" => \"Kaneyama\")",
	  " u = matrix {{0},{0}}",
	  " eulerChi(E,u)",
	  " eulerChi E"
	  }
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
     
     PARA{}, TT "twist"," takes a toric vector bundle ",TT "E"," in Klyachko's description and a list of integers ",TT "L",". The list must 
     contain one entry for each ray of the underlying fan. Then it computes the twist of the vector bundle by the line bundle given by these 
     integers (see ",TO weilToCartier,").",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " L = {1,-2,3,-4}",
	  " E1 = twist(E,L)",
	  " details E1"
	  }
     }

document {
     Key => {(coker,ToricVectorBundleKlyachko,Matrix)},
     Headline => " computes the cokernel of a morphism to a vector bundle",
     Usage => " E1 = coker(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the target space is the space of the bundle, i.e. the matrix must have k 
     rows if the bundle has rank k. Then the new bundle is given on each ray r by the following filtration of coker(E,M) = (E^r)/im(M) : 
     coker(E,M)(i) := E^r(i) / (E^r(i)\\cap im(M)).",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0},{0,1},{1,0},{0,1/1}}",
	  " E1 = coker(E,M)",
	  " details E1"
	  }
     }

document {
     Key => {(image,ToricVectorBundleKlyachko,Matrix)},
     Headline => " computes the image of a vector bundle under a morphism",
     Usage => " E1 = image(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the source space is the space of the bundle, i.e. the matrix must have k 
     columns if the bundle has rank k. Then the new bundle is given on each ray r by the following filtration of image(E,M) = M(E) : 
     image(E,M)(i) := M(E^r(i)).",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0,1,0},{0,1,0,1/1}}",
	  " E1 = image(E,M)",
	  " details E1"
	  }
     }

document {
     Key => {(ker,ToricVectorBundleKlyachko,Matrix)},
     Headline => " computes the kernel of a morphism to a vector bundle",
     Usage => " E1 = ker(E,M)",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko,
	  "M" => Matrix => {"over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "E1" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, TT "M"," must be a matrix over ",TO ZZ," or ",TO QQ," where the source space is the space of the bundle, i.e. the matrix must have k 
     columns if the bundle has rank k. Then the new bundle is given on each ray r by the following filtration of ker(E,M) = ker(M) \\cap (E) : 
     ker(E,M)(i) := ker(M) \\cap E^r(i).",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 2",
	  " E = E ** E",
	  " M = matrix {{1,0,1,0},{0,1,0,1/1}}",
	  " E1 = ker(E,M)",
	  " details E1"
	  }
     }

document {
     Key => {(symbol **,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama), (symbol **,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " computes the tensor product of two toric vector bundles",
     Usage => " E = E1 ** E2",
     Inputs => {
	  "E1" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "E2" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "If ",TT "E1"," and ",TT "E2"," are defined over the same fan and in the same description, then ",TT "tproduct"," computes the 
     tensor product of the two vector bundles in this description",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = E1 ** E2",
	  " details E"
	  },
     
     PARA{}, " See also ",TO tproduct,"."
     
     }

document {
     Key => {(symbol ++,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama), (symbol ++,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " computes the direct sum of two toric vector bundles",
     Usage => " E = E1 ++ E2",
     Inputs => {
	  "E1" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "E2" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "E" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     
     PARA{}, "If ",TT "E1"," and ",TT "E2"," are defined over the same fan, then ",TT "dsum"," computes the direct sum of the 
     two vector bundles. The bundles must both be given in the same description and the resulting bundle will be in this description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E = E1 ++ E2",
	  " details E"
	  },
     
     PARA{}, " See also ",TO tproduct,"."
     
     }

document {
     Key => {(symbol ==,ToricVectorBundleKaneyama,ToricVectorBundleKaneyama), (symbol ==,ToricVectorBundleKlyachko,ToricVectorBundleKlyachko)},
     Headline => " checks for equality",
     Usage => " b = E1 == E2",
     Inputs => {
	  "E1" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko},
	  "E2" => {ofClass ToricVectorBundleKaneyama ," or ", ofClass ToricVectorBundleKlyachko}
	  },
     Outputs => {
	  "E" => Boolean
	  },
     
     PARA{}, "Checks if two toric vector bundles are the same. This only works if they are given in the same 
     description.",
     
     EXAMPLE {
	  " E1 = toricVectorBundle(2,hirzebruchFan 3)",
	  " E2 = tangentBundle hirzebruchFan 3",
	  " E1 == E2"
	  }
     
     }

document {
     Key => (net,ToricVectorBundleKaneyama),
     Headline => "displays characteristics of a toric vector bundle",
     Usage => " net E",
     Inputs => {
	  "E" => ToricVectorBundleKaneyama
	  },
     
     PARA{}, "Displays an overview of the properties of the toric vector bundle, 
     the dimension of the variety, the number of affine charts, and the rank of the 
     vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3;",
	  " net E"
	  }
     
     }

document {
     Key => (net,ToricVectorBundleKlyachko),
     Headline => "displays characteristics of a toric vector bundle in Klyachko's description",
     Usage => " net E",
     Inputs => {
	  "E" => ToricVectorBundleKlyachko
	  },
     
     PARA{}, "Displays an overview of the properties of the toric vector bundle, 
     the dimension of the variety, the number of affine charts, the number of rays of the fan, 
     and the rank of the vector bundle.",
     
     EXAMPLE {
	  " E = tangentBundle hirzebruchFan 3;",
	  " net E"
	  }
     
     }

document {
     Key => {hirzebruchFan,(hirzebruchFan,ZZ)},
     Headline => "generates the fan of the n-th Hirzebruch surface",
     Usage => " F = hirzebruchFan n",
     Inputs => {
	  "n" => ZZ => {"positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of the n-th Hirzebruch surface.",
     
     EXAMPLE {
	  " F = hirzebruchFan 3",
	  " rays F"
	  }
     
     }

document {
     Key => {pp1ProductFan,(pp1ProductFan,ZZ)},
     Headline => "generates the fan of n products of PP^1",
     Usage => " F = pp1ProductFan n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of the product of n projective 1-spaces. This is the same as the 
     normal fan of the n dimensional hypercube.",
     
     EXAMPLE {
	  " F = pp1ProductFan 2",
	  " apply(maxCones F, rays)"
	  }
     
     }


document {
     Key => {projectiveSpaceFan,(projectiveSpaceFan,ZZ)},
     Headline => "generates the fan of projective n space",
     Usage => " F = projectiveSpaceFan n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Generates the fan of projective n space.",
     
     EXAMPLE {
	  " F = projectiveSpaceFan 2",
	  " apply(maxCones F, rays)"
	  }
     
     }



-- Test 0
-- Checking toricVectorBundle for Kaneyama type
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^2,QQ^2,1),(0,2) => map(QQ^2,QQ^2,1),(1,3) => map(QQ^2,QQ^2,1),(2,3) => map(QQ^2,QQ^2,1)})
assert(T#"degreeTable" === hashTable apply(maxCones pp1ProductFan 2, C -> C => map(ZZ^2,ZZ^2,0)))
assert(rank T == 2)
assert(dim T == 2)
L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}
L2 = {matrix {{-1,0},{0,-1}},matrix{{0,1},{1,0}},matrix{{0,-1},{-1,0}}}
T = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix {{-1/1,0},{0,-1}},(0,2) => matrix{{0/1,1},{1,0}},(1,2) => matrix{{0/1,-1},{-1,0}}})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,-1},{0,-1}} => matrix{{-1,0},{-1,1}}, posHull matrix {{1,0},{0,1}} => matrix{{0,1},{1,0}},posHull matrix {{-1,0},{-1,1}} => matrix{{1,0},{0,1}}})
assert(rank T == 2)
assert(dim T == 2)
///

-- Test 1
-- Checking toricVectorBundle for Klyachko type
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2);
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{0}} => map(ZZ^1,ZZ^2,0),matrix{{0},{-1}} => map(ZZ^1,ZZ^2,0),matrix{{1},{0}} => map(ZZ^1,ZZ^2,0),matrix{{0},{1}} => map(ZZ^1,ZZ^2,0)})
assert(T#"baseTable" === hashTable{matrix{{-1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{-1}} => map(QQ^2,QQ^2,1),matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => map(QQ^2,QQ^2,1)})
assert(rank T == 2)
assert(dim T == 2)
L1 = {matrix {{1,0},{0,1}},matrix{{0,1},{1,0}},matrix{{-1,0},{-1,1}}}
L2 = {matrix {{-1,0}},matrix{{-2,-1}},matrix{{0,1}}}
T = toricVectorBundle(2,projectiveSpaceFan 2,L1,L2)
assert(T#"ring" === ZZ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1}} => matrix{{-1,0}},matrix{{0},{1}} => matrix{{-2,-1}},matrix{{1},{0}} => matrix{{0,1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1}} => matrix {{1,0},{0,1}},matrix{{0},{1}} => matrix{{0,1},{1,0}},matrix{{1},{0}} => matrix{{-1,0},{-1,1}}})
assert(rank T == 2)
assert(dim T == 2)
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
assert(dim T == 2)
T = tangentBundle(projectiveSpaceFan 3, "Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^3,QQ^3,{{1, -1, 0}, {0, -1, 0}, {0, -1, 1}}), (0,2) => map(QQ^3,QQ^3,{{-1, 0, 0}, {-1, 1, 0}, {-1, 0, 1}}), (1,2) => map(QQ^3,QQ^3,{{-1, 1, 0}, {-1, 0, 0}, {-1, 0, 1}}), (0,3) => map(QQ^3,QQ^3,{{1, 0, -1}, {0, 0, -1}, {0, 1, -1}}), (1,3) => map(QQ^3,QQ^3,{{1, 0, -1}, {0, 1, -1}, {0, 0, -1}}), (2,3) => map(QQ^3,QQ^3,{{0, 0, -1}, {1, 0, -1}, {0, 1, -1}})})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,0,0},{0,-1,0},{0,0,-1}},posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0},{0,0,-1},{1,1,1}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{1,1,1},{0,-1,0},{0,0,-1}}, posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0},{1,1,1},{0,0,-1}}})
assert(rank T == 3)
assert(dim T == 3)
///

-- Test 5
-- Checking tangentBundle for Klyachko
TEST ///
T = tangentBundle hirzebruchFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{-1,0}},matrix{{0},{-1}} => matrix{{-1,0}},matrix{{1},{0}} => matrix{{-1,0}},matrix{{0},{1}} => matrix{{-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{-1,1/3},{3,0}},matrix{{0},{-1}} => matrix{{0_QQ,1},{-1,0}},matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => matrix{{0_QQ,1},{1,0}}})
assert(rank T == 2)
assert(dim T == 2)
T = tangentBundle pp1ProductFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{1},{0}} => matrix{{-1,0,0}}, matrix{{-1},{0},{0}} => matrix{{-1,0,0}},matrix{{1},{0},{0}} => matrix{{-1,0,0}}, matrix{{0},{0},{-1}} => matrix{{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{-1,0,0}}, matrix{{0},{-1},{0}} => matrix{{-1,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}, matrix{{-1},{0},{0}} => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{0},{-1}} => matrix{{0_QQ,1,0},{0,0,1},{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{0_QQ,1,0},{-1,0,0},{0,0,1}}})
assert(rank T == 3)
assert(rank T == 3)
///

-- Test 6
-- Checking cotangentBundle for Kaneyama
TEST ///
T = cotangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}}), (0,2) => map(QQ^2,QQ^2,{{-1, 3}, {0, 1}}), (1,3) => map(QQ^2,QQ^2,{{-1, -3}, {0, 1}}), (2,3) => map(QQ^2,QQ^2,{{1, 0}, {0, -1}})})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0},{0,-1}} => matrix{{1,0},{0,-1}},posHull matrix {{1,0},{0,1}} => matrix{{1,0},{0,1}},posHull matrix {{0,-1},{1,3}} => matrix{{-1,3},{0,1}}, posHull matrix{{0,-1},{-1,3}} => matrix{{-1,-3},{0,-1}}})
assert(rank T == 2)
assert(dim T == 2)
T = cotangentBundle(pp1ProductFan 3, "Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(2,6) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (4,5) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (4,6) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (3,7) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (5,7) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (6,7) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (0,1) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (0,2) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (1,3) => matrix{{1_QQ,0,0},{0,-1,0},{0,0,1}}, (0,4) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}, (2,3) => matrix{{1_QQ,0,0},{0,1,0},{0,0,-1}}, (1,5) => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}}})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{1,0,0},{0,1,0},{0,0,1}},posHull matrix{{-1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,0,0},{0,1,0},{0,0,1}},posHull matrix{{1,0,0},{0,-1,0},{0,0,1}} => matrix{{1,0,0},{0,-1,0},{0,0,1}},posHull matrix{{1,0,0},{0,1,0},{0,0,-1}} => matrix{{1,0,0},{0,1,0},{0,0,-1}},posHull matrix{{-1,0,0},{0,-1,0},{0,0,1}} => matrix{{-1,0,0},{0,-1,0},{0,0,1}},posHull matrix{{-1,0,0},{0,1,0},{0,0,-1}} => matrix{{-1,0,0},{0,1,0},{0,0,-1}},posHull matrix{{1,0,0},{0,-1,0},{0,0,-1}} => matrix{{1,0,0},{0,-1,0},{0,0,-1}},posHull matrix{{-1,0,0},{0,-1,0},{0,0,-1}} => matrix{{-1,0,0},{0,-1,0},{0,0,-1}}})
assert(rank T == 3)
assert(dim T == 3)
///

-- Test 7
-- Checking cotangentBundle for Klyachko
TEST ///
T = cotangentBundle hirzebruchFan 2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{1,0}},matrix{{0},{-1}} => matrix{{1,0}},matrix{{1},{0}} => matrix{{1,0}},matrix{{0},{1}} => matrix{{1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{0,2},{1/2,1}},matrix{{0},{-1}} => matrix{{0_QQ,1},{-1,0}},matrix{{1},{0}} => map(QQ^2,QQ^2,1),matrix{{0},{1}} => matrix{{0_QQ,1},{1,0}}})
assert(rank T == 2)
assert(dim T == 2)
T = cotangentBundle pp1ProductFan 3
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{1},{0}} => matrix{{1,0,0}}, matrix{{-1},{0},{0}} => matrix{{1,0,0}},matrix{{1},{0},{0}} => matrix{{1,0,0}}, matrix{{0},{0},{-1}} => matrix{{1,0,0}}, matrix{{0},{0},{1}} => matrix{{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{1,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}, matrix{{-1},{0},{0}} => matrix{{-1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{0},{-1}} => matrix{{0_QQ,1,0},{0,0,1},{-1,0,0}}, matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{0},{-1},{0}} => matrix{{0_QQ,1,0},{-1,0,0},{0,0,1}}})
assert(rank T == 3)
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
assert(deltaE T == convexHull matrix {{-1,2,-1},{-1,-1,2}})
T = cotangentBundle(pp1ProductFan 3,"Type" => "Kaneyama")
assert(deltaE T == convexHull matrix {{-1,-1,-1,-1,1,1,1,1},{-1,-1,1,1,-1,-1,1,1},{-1,1,-1,1,-1,1,-1,1}})
///

-- Test 10
-- Checking deltaE for Klyachko
TEST ///
T = toricVectorBundle(3,projectiveSpaceFan 2)
assert(deltaE T == convexHull matrix{{0},{0}})
T = tangentBundle projectiveSpaceFan 2
assert(deltaE T == convexHull matrix {{-1,2,-1},{-1,-1,2}})
T = cotangentBundle pp1ProductFan 3
assert(deltaE T == convexHull matrix {{-1,-1,-1,-1,1,1,1,1},{-1,-1,1,1,-1,-1,1,1},{-1,1,-1,1,-1,1,-1,1}})
///

-- Test 11
-- Checking cohomology for Kaneyama
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2,"Type" => "Kaneyama")
assert(cohomology(0,T,matrix{{0},{0}}) == (gradedRing T)^{{0,0},{0,0}})
assert(cohomology(0,T) == (gradedRing T)^{{0,0},{0,0}})
assert(cohomology(1,T) == (gradedRing T)^0)
assert(cohomology(2,T) == (gradedRing T)^0)
T1 = tangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
assert(cohomology(0,T1,matrix{{0},{0}}) == (gradedRing T1)^{{0,0},{0,0}})
assert(cohomology(0,T1,matrix{{1},{1}}) == (gradedRing T1)^0)
assert(cohomology(0,T1) == (gradedRing T1)^{{1,0},{0,1},{0,0},{0,0},{0,-1},{-1,0}})
assert(cohomology(1,T1) == (gradedRing T1)^0)
assert(cohomology(2,T1) == (gradedRing T1)^0)
T = tangentBundle(hirzebruchFan 3 * projectiveSpaceFan 1,"Type" => "Kaneyama")
assert(cohomology(0,T,{matrix {{2},{1},{0}}, matrix{{3},{1},{0}}}) == {(gradedRing T)^{{-2,-1,0}},(gradedRing T)^{{-3,-1,0}}})
assert(cohomology(1,T,{matrix {{-2},{-1},{0}}, matrix{{-1},{-1},{0}}}) == {(gradedRing T)^{{2, 1, 0}},(gradedRing T)^{{1, 1, 0}}})
assert(cohomology(2,T,matrix{{0},{0},{0}}) == (gradedRing T)^0)
assert(cohomology(3,T,matrix{{0},{0},{0}}) == (gradedRing T)^0)
///

-- Test 12
-- Checking cohomology for Klyachko
TEST ///
T = toricVectorBundle(2,pp1ProductFan 2)
assert(cohomology(0,T,matrix{{0},{0}}) == (gradedRing T)^{{0,0},{0,0}})
assert(cohomology(0,T) == (gradedRing T)^{{0,0},{0,0}})
assert(cohomology(1,T) == (gradedRing T)^0)
assert(cohomology(2,T) == (gradedRing T)^0)
T1 = tangentBundle pp1ProductFan 2
assert(cohomology(0,T1,matrix{{0},{0}}) == (gradedRing T1)^{{0,0},{0,0}})
assert(cohomology(0,T1,matrix{{1},{1}}) == (gradedRing T1)^0)
assert(cohomology(0,T1) == (gradedRing T1)^{{1,0},{0,1},{0,0},{0,0},{0,-1},{-1,0}})
assert(cohomology(1,T1) == (gradedRing T1)^0)
assert(cohomology(2,T1) == (gradedRing T1)^0)
T = tangentBundle(hirzebruchFan 3 * projectiveSpaceFan 1)
assert(cohomology(0,T) == (gradedRing T)^{{0, 0, 1}, {1, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {-1, 0, 0}, {0, 0, -1}, {0, -1, 0}, {-1, -1, 0}, {-2, -1, 0}, {-3, -1, 0}})
assert(cohomology(1,T) == (gradedRing T)^{{2, 1, 0}, {1, 1, 0}})
assert(cohomology(2,T) == (gradedRing T)^0)
assert(cohomology(3,T) == (gradedRing T)^0)
///

-- Test 13
-- Checking weilToCartier
TEST ///
T = weilToCartier({1,2,3,4},projectiveSpaceFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable {(0,1) => map(QQ^1,QQ^1,1),(0,2) => map(QQ^1,QQ^1,1),(0,3) => map(QQ^1,QQ^1,1),(1,2) => map(QQ^1,QQ^1,1),(1,3) => map(QQ^1,QQ^1,1),(2,3) => map(QQ^1,QQ^1,1)})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{-2},{-3},{6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{8},{-3},{-4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{-2},{7},{-4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{-2},{-3},{-4}}})
assert(rank T == 1)
assert(dim T == 3)
T = weilToCartier({1,2,3,4},projectiveSpaceFan 3)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1}},matrix{{0},{0},{1}} => matrix{{-4}},matrix{{0},{1},{0}} => matrix{{-3}}, matrix{{1},{0},{0}} => matrix{{-2}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1_QQ}},matrix{{0},{0},{1}} => matrix{{1_QQ}},matrix{{0},{1},{0}} => matrix{{1_QQ}}, matrix{{1},{0},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(dim T == 3)
///

-- Test 14
-- Checking dsum for Kaneyama
TEST ///
T1 = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T2 = weilToCartier({1,3,5,7},projectiveSpaceFan 3,"Type" => "Kaneyama")
T = T1 ++ T2
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix{{1_QQ,-1,0,0},{0,-1,0,0},{0,-1,1,0},{0,0,0,1}}, (0,2) => matrix{{-1_QQ,0,0,0},{-1,1,0,0},{-1,0,1,0},{0,0,0,1}}, (1,2) => matrix{{-1_QQ,1,0,0},{-1,0,0,0},{-1,0,1,0},{0,0,0,1}}, (0,3) => matrix{{1_QQ,0,-1,0},{0,0,-1,0},{0,1,-1,0},{0,0,0,1}}, (1,3) => matrix{{1_QQ,0,-1,0},{0,1,-1,0},{0,0,-1,0},{0,0,0,1}}, (2,3) => matrix{{0_QQ,0,-1,0},{1,0,-1,0},{0,1,-1,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable {posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0,-3},{0,0,-1,-5},{1,1,1,9}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{1,1,1,13},{0,-1,0,-5},{0,0,-1,-7}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0,-3},{1,1,1,11},{0,0,-1,-7}},posHull map(ZZ^3,ZZ^3,1) => matrix{{-1,0,0,-3},{0,-1,0,-5},{0,0,-1,-7}}})
assert(rank T == 4)
assert(dim T == 3)
T1 = cotangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
T2 = tangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
T = T1 ++ T2
assert(T#"baseChangeTable" === hashTable {(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}, (0,2) => matrix{{-1_QQ,3,0,0},{0,1,0,0},{0,0,-1,0},{0,0,3,1}}, (1,3) => matrix{{-1_QQ,-3,0,0},{0,1,0,0},{0,0,-1,0},{0,0,-3,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}})
assert(T#"degreeTable" === hashTable {posHull matrix{{1,0},{0,-1}} => matrix{{1,0,-1,0},{0,-1,0,1}}, posHull matrix{{0,-1},{1,3}} => matrix{{-1,3,1,-3},{0,1,0,-1}}, posHull matrix{{1,0},{0,1}} => matrix{{1,0,-1,0},{0,1,0,-1}}, posHull matrix{{0,-1},{-1,3}} => matrix {{-1,-3,1,3},{0,-1,0,1}}})
assert(rank T == 4)
assert(dim T == 2)
///

--Test 15
-- Checking dsum for Klyachko
TEST ///
T1 = tangentBundle projectiveSpaceFan 3
T2 = weilToCartier({1,3,5,7},projectiveSpaceFan 3)
T = T1 ++ T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1,0,0,-1}},matrix{{0},{0},{1}} => matrix{{-1,0,0,-7}},matrix{{0},{1},{0}} => matrix{{-1,0,0,-5}}, matrix{{1},{0},{0}} => matrix{{-1,0,0,-3}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0,0},{-1,1,0,0},{-1,0,1,0},{0,0,0,1}},matrix{{0},{0},{1}} => matrix{{0_QQ,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0,0},{1,0,0,0},{0,0,1,0},{0,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(dim T == 3)
T1 = cotangentBundle hirzebruchFan 3
T2 = tangentBundle hirzebruchFan 3
T = T1 ++ T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{1,0,-1,0}},matrix{{0},{-1}} => matrix{{1,0,-1,0}},matrix{{0},{1}} => matrix{{1,0,-1,0}}, matrix{{1},{0}} => matrix{{1,0,-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{0,3,0,0},{1/3,1,0,0},{0,0,-1,1/3},{0,0,3,0}},matrix{{0},{-1}} => matrix{{0_QQ,1,0,0},{-1,0,0,0},{0,0,0,1},{0,0,-1,0}},matrix{{0},{1}} => matrix{{0,1_QQ,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,0}}, matrix{{1},{0}} => map(QQ^4,QQ^4,1)})
assert(rank T == 4)
assert(dim T == 2)
///

-- Test 16
-- Checking dual for Kaneyama
TEST ///
T = dual weilToCartier({1,2,3,4},projectiveSpaceFan 3,"Type" => "Kaneyama")
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ}},(0,2) => matrix{{1_QQ}}, (0,3) => matrix{{1_QQ}}, (1,2) => matrix{{1_QQ}},(1,3) => matrix{{1_QQ}},(2,3) => matrix{{1_QQ}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{2},{3},{-6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{-8},{3},{4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{2},{-7},{4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{2},{3},{4}}})
assert(rank T == 1)
assert(dim T == 3)
T1 = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = dual(T1 ++ T)
assert(T#"baseChangeTable" === hashTable{(0,2) => matrix{{-1_QQ,-1,-1,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},(0,1) => matrix{{1_QQ,0,0,0},{-1,-1,-1,0},{0,0,1,0},{0,0,0,1}}, (0,3) => matrix{{1_QQ,0,0,0},{-1,-1,-1,0},{0,1,0,0},{0,0,0,1}}, (1,2) => matrix{{0_QQ,1,0,0},{-1,-1,-1,0},{0,0,1,0},{0,0,0,1}},(1,3) => matrix{{1_QQ,0,0,0},{0,1,0,0},{-1,-1,-1,0},{0,0,0,1}},(2,3) => matrix{{-1_QQ,-1,-1,0},{1,0,0,0},{0,1,0,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,1,0,-2},{0,0,1,-3},{-1,-1,-1,6}},posHull matrix {{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{-1,-1,-1,8},{0,1,0,-3},{0,0,1,-4}},posHull matrix {{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,1,0,-2},{-1,-1,-1,7},{0,0,1,-4}},posHull map(ZZ^3,ZZ^3,1) => matrix{{1,0,0,-2},{0,1,0,-3},{0,0,1,-4}}})
assert(rank T == 4)
assert(dim T == 3)
///

-- Test 17
-- Checking dual for Klyachko
TEST ///
T = dual weilToCartier({1,2,3,4},projectiveSpaceFan 3)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1}},matrix{{0},{0},{1}} => matrix{{4}},matrix{{0},{1},{0}} => matrix{{3}}, matrix{{1},{0},{0}} => matrix{{2}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1_QQ}},matrix{{0},{0},{1}} => matrix{{1_QQ}},matrix{{0},{1},{0}} => matrix{{1_QQ}}, matrix{{1},{0},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(dim T == 3)
T1 = tangentBundle projectiveSpaceFan 3
T = dual(T1 ++ T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{1,0,0,-1}},matrix{{0},{0},{1}} => matrix{{1,0,0,-1}},matrix{{0},{1},{0}} => matrix{{1,0,0,-1}}, matrix{{1},{0},{0}} => matrix{{1,0,0,-1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,-1,-1,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},matrix{{0},{0},{1}} => matrix{{0_QQ,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0,0},{1,0,0,0},{0,0,1,0},{0,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(dim T == 3)
///

-- Test 18
-- Checking tproduct for Kaneyama
TEST ///
T1 = tangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
T2 = cotangentBundle(pp1ProductFan 2,"Type" => "Kaneyama")
T = T1 ** T2
assert(T#"baseChangeTable" === hashTable{(0,2) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}},(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}, (1,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{-1,0},{0,1}} => matrix{{0,-1,1,0},{0,-1,1,0}},posHull matrix {{-1,0},{0,-1}} => matrix{{0,-1,1,0},{0,1,-1,0}},posHull matrix {{1,0},{0,-1}} => matrix{{0,1,-1,0},{0,1,-1,0}},posHull map(ZZ^2,ZZ^2,1) => matrix{{0,1,-1,0},{0,-1,1,0}}})
assert(rank T == 4)
assert(dim T == 2)
T1 = tangentBundle(hirzebruchFan 2,"Type" => "Kaneyama")
T2 = weilToCartier({1,5,3,7},hirzebruchFan 2,"Type" => "Kaneyama")
T2 = T2 ++ T2
T = T1 ** T2
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}},(0,2) => matrix{{-1_QQ,0,0,0},{2,1,0,0},{0,0,-1,0},{0,0,2,1}}, (1,3) => matrix{{-1_QQ,0,0,0},{-2,1,0,0},{0,0,-1,0},{0,0,-2,1}}, (2,3) => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,-1}}})
assert(T#"degreeTable" === hashTable{posHull matrix {{1,0},{0,1}} => matrix{{-4,-3,-4,-3},{-1,-2,-1,-2}},posHull matrix {{1,0},{0,-1}} => matrix{{-4,-3,-4,-3},{5,6,5,6}},posHull matrix {{0,-1},{-1,2}} => matrix{{18,19,18,19},{5,6,5,6}},posHull matrix{{0,-1},{1,2}} => matrix{{6,3,6,3},{-1,-2,-1,-2}}})
assert(rank T == 4)
assert(dim T == 2)
///

-- Test 19
-- Checking tproduct for Klyachko
TEST ///
T1 = tangentBundle pp1ProductFan 2
T2 = cotangentBundle pp1ProductFan 2
T = T1 ** T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{0}} => matrix{{0,-1,1,0}},matrix{{0},{-1}} => matrix{{0,-1,1,0}},matrix{{0},{1}} => matrix{{0,-1,1,0}}, matrix{{1},{0}} => matrix{{0,-1,1,0}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{0}} => matrix{{1_QQ,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1}},matrix {{0},{-1}} => matrix{{0_QQ,0,0,1},{0,0,-1,0},{0,-1,0,0},{1,0,0,0}},matrix {{0},{1}} => matrix{{0_QQ,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}},matrix{{1},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(dim T == 2)
T1 = tangentBundle hirzebruchFan 2
T2 = weilToCartier({1,5,3,7},hirzebruchFan 2)
T2 = T2 ++ T2
T = T1 ** T2
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{2}} => matrix{{-8,-8,-7,-7}},matrix{{0},{-1}} => matrix{{-6,-6,-5,-5}},matrix{{0},{1}} => matrix{{-2,-2,-1,-1}}, matrix{{1},{0}} => matrix{{-4,-4,-3,-3}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{-1,0,1/2,0},{0,-1,0,1/2},{2,0,0,0},{0,2,0,0}},matrix {{0},{-1}} => matrix{{0_QQ,0,1,0},{0,0,0,1},{-1,0,0,0},{0,-1,0,0}},matrix {{0},{1}} => matrix{{0_QQ,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0}},matrix{{1},{0}} => matrix{{1_QQ,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}})
assert(rank T == 4)
assert(dim T == 2)
///

-- Test 20
-- Checking symmProd for Kaneyama
TEST ///
T = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = symmProd(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{1_QQ,-1,0,1,0,0},{0,-1,0,2,0,0},{0,-1,1,2,-1,0},{0,0,0,1,0,0},{0,0,0,2,-1,0},{0,0,0,1,-1,1}},(0,2) => matrix{{1_QQ,0,0,0,0,0},{2,-1,0,0,0,0},{2,0,-1,0,0,0},{1,-1,0,1,0,0},{2,-1,-1,0,1,0},{1,0,-1,0,0,1}}, (0,3) => matrix{{1_QQ,0,-1,0,0,1},{0,0,-1,0,0,2},{0,1,-1,0,-1,2},{0,0,0,0,0,1},{0,0,0,0,-1,2},{0,0,0,1,-1,1}}, (1,2) => matrix{{1_QQ,-1,0,1,0,0},{2,-1,0,0,0,0},{2,-1,-1,0,1,0},{1,0,0,0,0,0},{2,0,-1,0,0,0},{1,0,-1,0,0,1}}, (1,3) => matrix{{1_QQ,0,-1,0,0,1},{0,1,-1,0,-1,2},{0,0,-1,0,0,2},{0,0,0,1,-1,1},{0,0,0,0,-1,2},{0,0,0,0,0,1}},(2,3) => matrix{{0_QQ,0,0,0,0,1},{0,0,-1,0,0,2},{0,0,0,0,-1,2},{1,0,-1,0,0,1},{0,1,-1,0,-1,2},{0,0,0,1,-1,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{0,-1,0,-2,-1,0},{2,2,2,2,2,2},{0,0,-1,0,-1,-2}},posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-2,-1,-1,0,0,0},{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2}},posHull matrix{{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2},{2,2,2,2,2,2}},posHull matrix{{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{2,2,2,2,2,2},{0,-1,0,-2,-1,0},{0,0,-1,0,-1,-2}}})
assert(rank T == 6)
assert(dim T == 3)
///

-- Test 21
-- Checking symmProd for Klyachko
TEST ///
T = tangentBundle projectiveSpaceFan 3
T = symmProd(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{1},{0},{0}} => matrix{{-2,-1,-1,0,0,0}}, matrix{{0},{1},{0}} => matrix{{-2,-1,-1,0,0,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,0,0,1,0,0},{0,0,0,0,1,0},{0,1,0,0,0,0},{0,0,0,0,0,1},{0,0,1,0,0,0},{1,0,0,0,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{1_QQ,0,0,0,0,0},{2,-1,0,0,0,0},{2,0,-1,0,0,0},{1,-1,0,1,0,0},{2,-1,-1,0,1,0},{1,0,-1,0,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}}, matrix{{0},{1},{0}} => matrix{{0_QQ,0,0,1,0,0},{0,1,0,0,0,0},{0,0,0,0,1,0},{1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,0,1}}})
assert(rank T == 6)
assert(dim T == 3)
///

-- Test 22
-- Checking extPower for Kaneyama
TEST ///
T = cotangentBundle(hirzebruch 3,"Type" => "Kaneyama")
T = extPower(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{-1_QQ}}, (0,2) => matrix{{-1_QQ}}, (1,3) => matrix{{-1_QQ}}, (2,3) => matrix{{-1_QQ}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,0},{0,1}} => matrix{{1},{1}},posHull matrix{{1,0},{0,-1}} => matrix{{1},{-1}},posHull matrix{{0,-1},{1,3}} => matrix {{2},{1}},posHull matrix{{0,-1},{-1,3}} => matrix {{-4},{-1}}})
assert(rank T == 1)
assert(dim T == 2)
T = tangentBundle(projectiveSpaceFan 3,"Type" => "Kaneyama")
T = extPower(2,T)
assert(T#"baseChangeTable" === hashTable{(0,1) => matrix{{-1_QQ,0,0},{-1,1,-1},{0,0,-1}}, (0,2) => matrix{{-1_QQ,0,0},{0,-1,0},{1,-1,1}}, (0,3) => matrix{{0_QQ,-1,0},{1,-1,1},{0,0,1}}, (1,2) => matrix{{1_QQ,0,0},{1,-1,1},{0,-1,0}}, (1,3) => matrix{{1_QQ,-1,1},{0,-1,0},{0,0,-1}}, (2,3) => matrix{{0_QQ,1,0},{0,0,1},{1,-1,1}}})
assert(T#"degreeTable" === hashTable{posHull matrix{{1,-1,0},{0,-1,0},{0,-1,1}} => matrix{{-1,0,-1},{2,2,2},{0,-1,-1}},posHull matrix{{1,0,0},{0,1,0},{0,0,1}} => matrix{{-1,-1,0},{-1,0,-1},{0,-1,-1}},posHull matrix{{1,0,-1},{0,1,-1},{0,0,-1}} => matrix{{-1,0,-1},{0,-1,-1},{2,2,2}},posHull matrix{{0,-1,0},{1,-1,0},{0,-1,1}} => matrix{{2,2,2},{-1,0,-1},{0,-1,-1}}})
assert(rank T == 3)
assert(dim T == 3)
///

-- Test 23
-- Checking extPower for Klyachko
TEST ///
T = cotangentBundle hirzebruch 3
T = extPower(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{-1},{3}} => matrix{{1}}, matrix{{0},{1}} => matrix{{1}}, matrix{{0},{-1}} => matrix{{1}}, matrix{{1},{0}} => matrix{{1}}})
assert(T#"baseTable" === hashTable {matrix{{-1},{3}} => matrix{{-1_QQ}}, matrix{{0},{1}} => matrix{{-1_QQ}}, matrix{{0},{-1}} => matrix{{1_QQ}}, matrix{{1},{0}} => matrix{{1_QQ}}})
assert(rank T == 1)
assert(dim T == 2)
T = tangentBundle projectiveSpaceFan 3
T = extPower(2,T)
assert(T#"ring" === QQ)
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{-1,-1,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1,-1,0}}, matrix{{1},{0},{0}} => matrix{{-1,-1,0}}, matrix{{0},{1},{0}} => matrix{{-1,-1,0}}})
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,0,1},{-1,0,0},{0,-1,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0},{0,-1,0},{1,-1,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}}, matrix{{0},{1},{0}} => matrix{{-1_QQ,0,0},{0,0,1},{0,1,0}}})
assert(rank T == 3)
assert(dim T == 3)
///

-- Test 24
-- Checking eulerChi
TEST ///
T = tangentBundle hirzebruchFan 3
u = matrix {{0},{0}}
assert(eulerChi(T,u) == 2)
assert(eulerChi T == 6)
T = cotangentBundle projectiveSpaceFan 4
assert(eulerChi T == -1)
T = tangentBundle(hirzebruchFan 3,"Type" => "Kaneyama")
u = matrix {{0},{0}}
assert(eulerChi(T,u) == 2)
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
assert(T1#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{-1/2,1},{1,0}}, matrix{{0},{-1}} => matrix{{0,1_QQ},{1,0}}, matrix{{0},{1}} => matrix{{0,1_QQ},{1,0}}, matrix{{1},{0}} => matrix{{1_QQ,0},{0,1}}})
assert(rank T1 == 2)
assert(dim T1 == 2)
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
assert(T1#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{-3/2,1},{1,0}}, matrix{{0},{-1}} => matrix{{-2,2_QQ},{1,0}}, matrix{{0},{1}} => matrix{{-2,2_QQ},{1,0}}, matrix{{1},{0}} => matrix{{1_QQ,0},{0,1}}})
assert(rank T1 == 2)
assert(dim T1 == 2)
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
assert(T1#"baseTable" === hashTable {matrix{{-1},{2}} => matrix{{-1/2,1},{1,0}}, matrix{{0},{-1}} => matrix{{0,1_QQ},{1,0}}, matrix{{0},{1}} => matrix{{0,1_QQ},{1,0}}, matrix{{1},{0}} => matrix{{1_QQ,0},{0,1}}})
assert(rank T1 == 2)
assert(dim T1 == 2)
///

-- Test 28
-- Checking twist
TEST ///
T = tangentBundle projectiveSpaceFan 3
L = {1,-2,3,-4}
T = twist(T,L)
assert(T#"ring" === QQ)
assert(T#"baseTable" === hashTable {matrix{{0},{0},{1}} => matrix{{0_QQ,1,0},{0,0,1},{1,0,0}}, matrix{{-1},{-1},{-1}} => matrix{{-1_QQ,0,0},{-1,1,0},{-1,0,1}}, matrix{{1},{0},{0}} => matrix{{1_QQ,0,0},{0,1,0},{0,0,1}},matrix{{0},{1},{0}} => matrix{{0_QQ,1,0},{1,0,0},{0,0,1}}})
assert(T#"filtrationMatricesTable" === hashTable {matrix{{0},{0},{1}} => matrix{{3,4,4}}, matrix{{-1},{-1},{-1}} => matrix{{-2,-1,-1}}, matrix{{1},{0},{0}} => matrix{{1, 2, 2}}, matrix{{0},{1},{0}} => matrix{{-4,-3,-3}}})
assert(rank T == 3)
assert(dim T == 3)
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

end
     
     