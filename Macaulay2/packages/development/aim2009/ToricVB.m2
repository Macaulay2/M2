---------------------------------------------------------------------------
-- PURPOSE: Computations with vector bundles on toric varieties 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : November 2008
---------------------------------------------------------------------------
newPackage("ToricVB",
    Headline => "A package for computations with vector bundles on toric varieties",
    Version => "0.5",
    Date => "November 13, 2008",
    Authors => {
         {Name => "René Birkner", Email => "rbirkner@mi.fu-berlin.de"}},
    DebuggingMode => true,
    Configuration => {}
    )

export {ToricVectorBundle, ToricVectorBundleK,
        makeVB, makeVBK, addBase, addBaseChange, addDegrees, addFiltration, cocycleCheck, details, makeCompatible, regCheck, 
	charts, cohom, cotangentBundle, cotangentBundleK, deltaE, dsum, dualVB, extPower, symmProd, tangentBundle,  tangentBundleK, tproduct, 
	weilToCartier, hirzebruchfan, hypercubefan}

needsPackage "Polyhedra"

---------------------------------------------------------------------------
-- DEFINING NEW TYPES
---------------------------------------------------------------------------

-- Defining the new type ToricVectorBundle
ToricVectorBundle = new Type of HashTable
ToricVectorBundle.synonym = "vector bundle on a toric variety"
globalAssignment ToricVectorBundle



-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net ToricVectorBundle := tvb -> ( horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply({"dimension of the variety",
					       	    	      	    "rank of the vector bundle",
								    "number of affine charts"}, key -> (net key, " => ", net tvb#key))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


-- Defining the new type ToricVectorBundle
ToricVectorBundleK = new Type of HashTable
ToricVectorBundleK.synonym = "vector bundle on a toric variety (Klyachko)"
globalAssignment ToricVectorBundleK


-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net ToricVectorBundleK := tvb -> ( horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply({"dimension of the variety",
					       	    	      	    "rank of the vector bundle",
								    "number of affine charts",
								    "number of rays"}, key -> (net key, " => ", net tvb#key))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


---------------------------------------------------------------
-- FUNCTIONS TO CONSTRUCT VECTOR BUNDLES AND MODIFY THEM
---------------------------------------------------------------


-- PURPOSE : Building a Vector Bundle of rank 'k' on the Toric Variety given by the Fan 'F'
--           with 0 degrees and identity transition matrices
--   INPUT : '(k,F)',  a strictly psoitive integer 'k' and a pure and full dimensional
--                     Fan 'F' 
--  OUTPUT : The ToricVectorBundle 'VB'
makeVB = method(TypicalValue => ToricVectorBundle)
makeVB (ZZ,Fan) := (k,F) -> (
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
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "codim1Table" => Ltable,
	  "ToricVariety" => F,
	  "number of affine charts" => #topConeTable,
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => k,
	  "topConeTable" => topConeTable})


--   INPUT : '(k,F,degreeList,matrixList)',  a strictly positive integer 'k', a pure and full dimensional
--                     Fan 'F' of dimension n, a list 'degreeList' of k by n matrices over ZZ, one for each 
--     	    	       top dimensional Cone in 'F' where the columns give the degrees of the generators in the 
--     	    	       corresponding affine chart to this Cone, and a list 'matrixList' of  k by k matrices 
--     	    	       over QQ, one for each pair of top dimensional Cones intersecting in a common codim 1 face. 
--  OUTPUT : The ToricVectorBundle 'tvb' 
-- COMMENT : Note that the top dimensional cones are numbered starting with 0 and the codim 1 intersections are 
--           labelled by pairs (i,j) denoting the the two top dim cones involved, with i<j and they are ordered
--     	     in lexicographic order. So the matrices in 'matrixList' will be assigned to the pairs (i,j) in that 
--     	     order, where the matrix A assigned to (i,j) denotes the transition
--     	    	 (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)* A
--     	     The matrices in 'degreeList' will be assigned to the cones in the order in which they are numbered.
makeVB (ZZ,Fan,List,List) := (k,F,degreelist,matrixlist) -> (
     -- Generating the trivial vector bundle of rank k
     tvb := makeVB(k,F);
     -- Adding the given degrees and transition matrices
     tvb = addDegrees(tvb,degreelist);
     tvb = addBaseChange(tvb,matrixlist);
     tvb)


-- PURPOSE : Building a Vector Bundle in the Klyachko description of rank 'k' on the Toric Variety given by the Fan 'F'
--           with trivial Filtration for every ray
--   INPUT : '(k,F)',  a strictly psoitive integer 'k' and a pure and full dimensional Fan 'F' 
--  OUTPUT : The ToricVectorBundleK 'VB'
makeVBK = method(TypicalValue => ToricVectorBundleK)
makeVBK (ZZ,Fan) := (k,F) -> (
     -- Checking for input errors
     if k < 1 then error("The vector bundle must have a positive rank");
     if not isPure F then error("Fan must be of pure dimension");
     if dim F != ambDim F then error("Fan must be of full dimension");
     if not isPointed F then error("The Fan must be pointed");
     -- Writing the table of rays
     rayTable := toList F#"rays";
     rayTable = hashTable apply(#rayTable, i -> i => rayTable#i);
     -- Writing the table of identity matrices for the vector bundle bases
     baseTable := hashTable apply(#rayTable, i -> i => map(QQ^k,QQ^k,1));
     -- Writing the table of matrices for the filtration maps
     filtrationMatricesTable := hashTable apply(#rayTable, i -> i =>  matrix {toList(k:0)});
     -- Computing the list of changes in the filtrations
     filtrationTable := hashTable apply(pairs filtrationMatricesTable, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({(min L1-1) => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
     -- Generating the vector bundle
     new ToricVectorBundleK from {
	  "ring" => QQ,
	  "rayTable" => rayTable,
	  "baseTable" => baseTable,
	  "filtrationMatricesTable" => filtrationMatricesTable,
	  "filtrationTable" => filtrationTable,
	  "ToricVariety" => F,
	  "number of affine charts" => #(F#"generatingCones"),
	  "dimension of the variety" => dim F,
	  "rank of the vector bundle" => k,
	  "number of rays" => #rayTable})


--   INPUT : '(k,F,baseList,filtrationList)',  a strictly positive integer 'k', a pure and full dimensional
--                     Fan 'F' of dimension n, a list 'baseList' of k by k matrices over the same ring/field, one for each 
--     	    	       ray of 'F' where the columns give the basis of the vector bundle over the ray, and a list 
--     	    	       'filtrationList' of  1 by k matrices over ZZ, one for each ray such that the i-th column of 
--     	    	       the base matrix is at first in the part of the filtration indexed by the i-th entry in the filtration 
--     	    	       matrix.
--  OUTPUT : The ToricVectorBundleK 'tvb' 
-- COMMENT : Note that the the bases and filtration matrices will be assigned to the rays in the order, they appear in rays F
makeVBK (ZZ,Fan,List,List) := (k,F,Bm,Fm) -> (
     tvb := makeVBK(k,F);
     tvb = addBase(tvb,Bm);
     addFiltration(tvb,Fm))


-- PURPOSE : Changing the base matrices of a given ToricVectorBundleK to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundle 'tvb' and a list 'L'of k by k matrices over a common ring/field, one for each
--     	    	      	   ray of the underlying fan
--  OUTPUT : The ToricVectorBundleK 'tvb' 
-- COMMENT : Note that the  matrices in 'L' will be assigned to the rays in the order they appear in rays tvb
addBase = method(TypicalValue => ToricVectorBundleK)
addBase (ToricVectorBundleK,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     k := tvb#"rank of the vector bundle";
     n := tvb#"number of rays";
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
	       i => M));
     -- Writing the bases into the bundle
     new ToricVectorBundleK from {
	  "ring" => first P,
	  "rayTable" => tvb#"rayTable",
	  "baseTable" => baseTable,
	  "filtrationMatricesTable" => tvb#"filtrationMatricesTable",
	  "filtrationTable" => tvb#"filtrationTable",
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "number of rays" => tvb#"number of rays"})


-- PURPOSE : Changing the transition matrices of a given ToricVectorBundle to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundle 'tvb' and a list 'L'of k by k matrices over QQ, one for each 
--     	    	      	   	  pair of top dimensional Cones intersecting in a common codim 1 face. 
--  OUTPUT : The ToricVectorBundle 'tvb' 
-- COMMENT : Note that the ToricVectorBundle already has a list of pairs (i,j) denoting the codim 1 intersections 
--     	     of two top dim cones, with i<j and they are ordered in lexicographic order. So the matrices in 'L'
--     	     will be assigned to the pairs (i,j) in that order, where the matrix A assigned to (i,j) denotes the 
--     	     transition
--     	    	 (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)* A
addBaseChange = method(TypicalValue => ToricVectorBundle)
addBaseChange (ToricVectorBundle,List) := (tvb,L) -> (
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
	       M = if (R === ZZ) or (R === QQ) then promote(M,QQ) else error("expected base change over ZZ or QQ");
	       -- Inserting the matrix at the i-th position
	       pairlist#i => M));
     -- Writing the new transition matrices into the bundle
     new ToricVectorBundle from {
	  "degreeTable" => tvb#"degreeTable",
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => k,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


-- PURPOSE : Changing the degrees of the local generators of a given ToricVectorBundle to those given in the List 
--   INPUT : '(tvb,L)',  a ToricVectorBundle 'tvb' and a list 'L'of n by k matrices over ZZ, one for each 
--     	    	      	 top dimensional Cone. 
--  OUTPUT : The ToricVectorBundle 'tvb' 
-- COMMENT : Note that in the ToricVectorBundle the top dimensional Cones are already numbered and that the degree
--     	     matrices will be assigned to the Cones in that order. 
addDegrees = method(TypicalValue => ToricVectorBundle)
addDegrees (ToricVectorBundle,List) := (tvb,L) -> (
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
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => tvb#"baseChangeTable",
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => k,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


--   INPUT : '(tvb,L)',  a ToricVectorBundleK 'tvb' and a list 'L'of 1 by k matrices over ZZ, one for each 
--     	    	      	   	  ray of the fan
--  OUTPUT : The ToricVectorBundleK 'tvb' 
-- COMMENT : Note that the  matrices in 'L' will be assigned to the rays in the order they appear in rays tvb
addFiltration = method(TypicalValue => ToricVectorBundleK)
addFiltration (ToricVectorBundleK,List) := (tvb,L) -> (
     -- Extracting data out of tvb
     n := tvb#"number of rays";
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if n != #L then error("Number of matrices must match number of rays of the fan");
     if any(L, l -> not instance(l,Matrix)) then error("The filtrations must be given as matrices");
     if any(L, l -> ring l =!= ZZ) then error("The filtrations must be given as matrices over ZZ");
     if any(L, l -> numColumns l != k or numRows l != 1) then error("The filtrations must be given as 1 times n matrices");
     -- Writing the new filtration matrices  into the table
     filtrationMatricesTable := hashTable apply(n, i -> i => L#i);
     -- Computing the list of changes in the filtrations
     filtrationTable := hashTable apply(pairs filtrationMatricesTable, p -> (
	       L := flatten entries p#1;
	       L1 := sort unique L;
	       p#0 => hashTable ({(min L1-1) => {}} |  apply(L1, l -> l => positions(L,e -> e == l)))));
     -- Writing the new filtration maps and changes tables into the bundle
     new ToricVectorBundleK from {
	  "ring" => tvb#"ring",
	  "rayTable" => tvb#"rayTable",
	  "baseTable" => tvb#"baseTable",
	  "filtrationMatricesTable" => filtrationMatricesTable,
	  "filtrationTable" => filtrationTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "number of rays" => tvb#"number of rays"})


-- PURPOSE : Giving the number of affine charts of a ToricVectorBundle
--   INPUT : 'tvb', a ToricVectorBundle or ToricVectorBundleK
--  OUTPUT : 'ZZ',  the number of affine charts
charts = method(TypicalValue => ZZ)
charts ToricVectorBundle := tvb -> tvb#"number of affine charts"


charts ToricVectorBundleK := tvb -> tvb#"number of affine charts"
	       

-- PURPOSE : Checking if the ToricVectorBundle fulfills the cocycle condition
--   INPUT : 'tvb',  a ToricVectorBundle 
--  OUTPUT : 'true' or 'false' 
cocycleCheck = method(TypicalValue => Boolean)
cocycleCheck ToricVectorBundle := tvb -> (
     -- Extracting data out of tvb
     n := tvb#"dimension of the variety";
     k := tvb#"rank of the vector bundle";
     bCT := tvb#"baseChangeTable";
     topCones := sort keys tvb#"topConeTable";
     L := hashTable {};
     -- For each codim 2 Cone computing the list of topCones which have this Cone as a face
     -- and save the list of indices of these topCones as an element in L
     scan(#topCones, i -> L = merge(hashTable apply(faces(2,topCones#i), C -> C => {i}),L,(a,b) -> sort join(a,b)));
     -- Finding the cyclic order of every list of topCones in L and write this cyclic order as a 
     -- list of consecutive pairs
     L = flatten apply(values L, l -> (
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
	       if dim intersection(topCones#a,topCones#start) == n-1 then {pairings | {(a,start)}} else {}));
     -- Check for every cyclic order of topCones if the product of the corresponding transition
     -- matrices is the identity
     all(L, l -> product apply(reverse l, e -> if e#0 > e#1 then inverse(bCT#(e#1,e#0)) else bCT#e) == map(QQ^k,QQ^k,1)))


-- PURPOSE : Presenting some details of the given ToricVectorBundle
--   INPUT : 'tvb',  a ToricVectorBundle
--  OUTPUT : '(A,B,C)',	 where 'A' is a hashTable giving the enumeration of the maximal cones with their rays, 
--     	    	      	 'B' gives the corresponding degrees, and 'C' the transition matrices  
-- COMMENT : This function gives the posibillity to have a quick overview on the main properties of a ToricVectorBundle
details = method()
details ToricVectorBundle := tvb -> (hashTable apply(pairs(tvb#"topConeTable"), p -> ( p#1 => rays(p#0))),tvb#"degreeTable",tvb#"baseChangeTable")


--   INPUT : 'tvb',  a ToricVectorBundleK
--  OUTPUT : 'T',	 a HashTable that gives for the enumeration of the rays the corresponding ray, basis and filtration matrix
-- COMMENT : This function gives the posibillity to have a quick overview on the main properties of a ToricVectorBundleK
details ToricVectorBundleK := tvb -> (hashTable apply(pairs(tvb#"rayTable"), p -> ( p#0 => (p#1,(tvb#"baseTable")#(p#0),(tvb#"filtrationMatricesTable")#(p#0)))))


-- PURPOSE : Making two ToricVectorBundles on the same Fan compatible
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle s
--  OUTPUT : 'tvb2',	     the now changed ToricVectorBundle
-- COMMENT : This is for two VectorBundles on the same fan. Since the top dimensional Cones
--     	     can be ordered in different ways in the two Bundles this changes the order in
--     	     'tvb2' to that of 'tvb1' and also corrects the degree table and the transition 
--     	     matrices. Therefore the output is only the changed 'tvb2'
makeCompatible = method(TypicalValue => ToricVectorBundle)
	  
	  	  
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle s
--  OUTPUT : 'tvb2',	     the now changed ToricVectorBundle
-- COMMENT : This is for two VectorBundles on the same fan. Since the rays can be ordered in different ways
--     	     in the two Bundles, this changes the order in 'tvb2' to that of 'tvb1' and also corrects the base 
--     	     table and the filtration table. Therefore the output is only the changed 'tvb2'
makeCompatible (ToricVectorBundleK,ToricVectorBundleK) := (tvb1,tvb2) -> (
     	       -- Checking for input errors
	       if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("Vector bundles must be over the same toric variety");
	       if tvb1#"ring" =!= tvb2#"ring" then error("Vector bundles must be over the same field");
	       L := {};
	       rayTable1 := tvb1#"rayTable";
	       rayTable2 := tvb2#"rayTable";
	       -- Rearranging the rayTable of tvb2
	       scan(#rayTable1, i -> ( 
			 scan(#rayTable2, j -> (
				   if (rayTable1#i == rayTable2#j) then (
					L = append(L,j => i))))));
	       L = hashTable L;
	       -- Rearranging the tables
	       rayTable := hashTable apply(pairs rayTable2, p -> ( L#(p#0) => p#1));
	       baseTable := hashTable apply(pairs tvb2#"baseTable", p -> ( L#(p#0) => p#1));
	       filtrationTable := hashTable apply(pairs tvb2#"filtrationTable", p -> ( L#(p#0) => p#1));
	       filtrationMatricesTable := hashTable apply(pairs tvb2#"filtrationMatricesTable", p -> ( L#(p#0) => p#1));
	       -- Writing the rearranged tables into tvb2
	       new ToricVectorBundleK from {
		    "ring" => tvb2#"ring",
		    "rayTable" => rayTable,
		    "baseTable" => baseTable,
		    "filtrationMatricesTable" => filtrationMatricesTable,
		    "filtrationTable" => filtrationTable,
		    "ToricVariety" => tvb2#"ToricVariety",
		    "number of affine charts" => tvb2#"number of affine charts",
		    "dimension of the variety" => tvb2#"dimension of the variety",
		    "rank of the vector bundle" => tvb2#"rank of the vector bundle",
		    "number of rays" => tvb2#"number of rays"})
	  
	  
-- PURPOSE : Checking if a ToricVectorBundle satisfies the regularity conditions of the degrees
--   INPUT : 'tvb', a ToricVectorBundle
--  OUTPUT : 'true' or 'false'
-- COMMENT : This function is for checking ToricVectorBundles whose degrees and matrices 
--     	     are inserted by hand. Those generated for example by tangentBundle fulfill the 
--     	     conditions automatically.
regCheck = method(TypicalValue => Boolean)
regCheck ToricVectorBundle := tvb -> (
     -- Extracting the neccesary data
     tCT := sort keys tvb#"topConeTable";
     c1T := tvb#"codim1Table";
     bCT := tvb#"baseChangeTable";
     pairlist := keys bCT;
     dT := tvb#"degreeTable";
     k := tvb#"rank of the vector bundle";
     all(pairlist, p -> (
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


-- PURPOSE : Checking if a ToricVectorBundleK satisfies the compatability condition
--   INPUT : 'tvb', a ToricVectorBundleK
--  OUTPUT : 'true' or 'false'
-- COMMENT : This function is for checking ToricVectorBundles whose bases and filtration matrices 
--     	     are inserted by hand. Those generated for example by tangentBundle fulfill the 
--     	     conditions automatically.
regCheck ToricVectorBundleK := tvb -> (
  -- if the Fan is smooth, the condition is automatically satisfied
  if (isSmooth(tvb#"ToricVariety")) then (
       true)
  else ( 
     -- Extracting the neccesary data
     rayT := tvb#"rayTable";
     rayT = hashTable apply(pairs rayT, p -> (p#1 => p#0));
     baseT := tvb#"baseTable";
     filtrationT := tvb#"filtrationTable";
     -- Computing the actual bases of the filtrations for each ray
     filtrationT = hashTable apply(pairs filtrationT, p -> (
	       previous := 0;
	       Lfilt := {};
	       Bfilt := baseT#(p#0);
	       filt := sort keys p#1;
	       filt = hashTable apply(filt, q -> (
			 prev := previous;
			 previous = q;
			 Lfilt = Lfilt | (p#1)#q;
			 q => (Bfilt_Lfilt,prev)));
	       p#0 => filt));
     -- Extracting more data
     k := tvb#"rank of the vector bundle";
     n := tvb#"dimension of the variety";
     F := tvb#"ToricVariety";
     reg := true;
     L := genCones(F);
     -- Checking the condition from theorem 2.3 for every generating cone
     while (reg) and (L != {}) do (
	  -- Taking the rays of the active generating cone
	  R := rays(L#0);
	  -- Drop the active cone
	  L = drop(L,1);
	  -- Make a list of the rays
	  R = apply(numColumns R, i -> ( R_{i}));
	  -- Selecting the basis of the first ray is our "standard basis"
	  B := baseT#(rayT#(R#0));
	  i := 0;
	  -- Scanning the basis vectors of B
	  while reg and (i < k) do (
	       b := image(B_{i});
	       -- Generating the matrix of condition vectors for the Eigenspaces degrees
	       M := map(QQ^0,QQ^n,0);
	       v := map(ZZ^0,ZZ^1,0);
	       v1 := v;
	       -- for every ray find the index of the filtration where the actual b appears for the 
	       -- first time and add it to v and the ray to M
	       scan(R, r -> (
			 j := rayT#r;
			 s := (select(pairs filtrationT#j, p -> (isSubset(b,image(p#1#0)))))#0;
			 M = M || transpose(r);
			 v = v || matrix{{s#0}};
			 v1 = v1 || matrix{{(s#0)-1}}));
	       -- intersect these to get the polytope of possible degrees for the Eigenspace containing 'b'
	       P := intersection(M || (-M),v || (-v1));
	       P = latticePoints P;
	       v1 = substitute(v1,QQ);
	       -- Check which lattice points of 'P' are not in one of the previous Filtration steps
	       Plist = select(P, p -> (all(numRows M, j -> (((M^{j})*p) != (v1^{j})))));
	       -- If such a lattce point exists the Filtrations satisfy the condition
	       reg = (Plist != {});
	       i = i+1));
     reg))
       

----------------------------------------------------------------------------
-- OPERATIONS ON TORIC VECTOR BUNDLES
----------------------------------------------------------------------------


-- PURPOSE : Computing the cohomology of a given ToricVectorBundle
cohom = method()


--   INPUT : '(k,tvb,u)',  'k' for the 'k'th cohomology group, 'tvb' a ToricVectorBundle, and 'u' the degree
--  OUTPUT : 'ZZ',	     the dimension of the degree 'u' part of the 'k'th cohomology group of 'tvb'
cohom (ZZ,ToricVectorBundle,Matrix) := (k,tvb,u) -> (
     -- Checking for input errors
     if ((numgens target u) != (tvb#"dimension of the variety")) or ((numgens source u) != 1) then (
	  error ("The degree matrix must be a vector of variety dimension"));
     if ((ring u) =!= ZZ) then (
	  error ("The degree must be an integer vector"));
     if ((k < 0) or (tvb#"dimension of the variety" < k)) then (
	  error ("k must be between 0 and the variety dimension for the k-th cohomolgy"));
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
	       if ((member((i,j),pl)) or (member((j,i),pl))) then (
		    cl = append(cl,(i,j));
		    -- Check if the new found path is shorter than shortest so far
		    if (#cl < #minpath) or (minpath == {}) then (
			 minpath = cl))
	       -- otherwise find a path with the remaining steps in 'pl'
	       else (
		    L1 := {};
		    L2 := {};
		    -- Sort the remaining possible steps into those containing 'i'in 'L1' and those who not in 'L2'
		    scan(pl, e -> ( 
			      if (member(i,e)) then ( L1 = append(L1,e))
			      else ( L2 = append(L2,e))));
		    -- Call findrecursive for each step in 'L1', with new starting cone the other index in the pair and new 
		    -- remaining pairs list 'L2' and add the step to 'cl'
		    scan(L1, e -> ( 
			      if (e#0 == i) then (
				   minpath = findrecursive(e#1,j,L2,append(cl,e),minpath))
			      else ( 
				   minpath = findrecursive(e#0,j,L2,append(cl,(e#1,e#0)),minpath)))));
	       minpath);
	  -- Start with an empty sequence of steps, no minimal path yet and all possible stepsd
	  cl := {};
	  minpath := {};
	  findrecursive(i,j,pl,cl,minpath));
     local M1;
     local M2;
     -- For the 'k'th cohomology group one needs all intersections of 'k', of 'k+1', and of 'k+2' charts of the covering
     -- given by the maximal dimensional cones which are 'l' many, if 'k' is strictly positive. 
     if (k > 0) then (
	  -- Collecting all intersections of 'k' top cones together with the degrees 'd' of the first cone which satisfy
	  -- u-d in the dual cone of the intersection
	  M1 = subsets(0..l-1,k);
	  M1 = hashTable apply(M1, cl -> (
		    C := intersection apply(cl, i -> tCT#i);
		    degs := dT#(tCT#(cl#0));
		    L := select(toList(0..rk-1), i -> ( contains(dualCone(C),u-(degs_{i}))));
		    cl => (L,C)));
	  -- Collecting all intersections of 'k+1' top cones together with the degrees 'd' of the first cone which satisfy
	  -- u-d in the dual cone of the intersection by taking each intersection of M1 and adding one cone
	  M2 = {};
	  scan(pairs M1, p -> (
		    L := select(toList(0..rk-1), i -> (not member(i,p#1#0)));
		    scan((last(p#0)+1)..(l-1), i -> (
			      cl :=append(p#0,i);
			      C := intersection(p#1#1,tCT#i);
			      degs := dT#(tCT#(cl#0));
			      M2 = append(M2,cl => (sort(unique(join(p#1#0,select(L, i -> ( contains(dualCone(C),u-(degs_{i}))))))),C))))));
	  M2 = hashTable M2;
	  M1 = hashTable apply(pairs M1, p -> ( p#0 => p#1#0)))
     -- Otherwise take only the 'k+1' and 'k+2' intersections
     else (
	  -- The same as above
	  M2 = subsets(0..l-1,k+1);
	  M2 = hashTable apply(M2, cl -> (
		    C := intersection apply(cl, i -> tCT#i);
		    degs := dT#(tCT#(cl#0));
		    L := select(toList(0..rk-1), i -> ( contains(dualCone(C),u-(degs_{i}))));
		    cl => (L,C))));
     -- Collecting all intersections of 'k+2' top cones together with the degrees 'd' of the first cone which satisfy
     -- u-d in the dual cone of the intersection by taking each intersection of M2 and adding one cone
     M3 := {};
     scan(pairs M2, p -> (
		    L := select(toList(0..rk-1), i -> (not member(i,p#1#0)));
		    scan((last(p#0)+1)..(l-1), i -> (
			      cl :=append(p#0,i);
			      C := intersection(p#1#1,tCT#i);
			      degs := dT#(tCT#(cl#0));
			      M3 = append(M3,cl => sort(unique(join(p#1#0,select(toList(0..rk-1), i -> ( contains(dualCone(C),u-(degs_{i}))))))))))));
     M3 = hashTable M3;  
     M2 = hashTable apply(pairs M2, p -> ( p#0 => p#1#0));
     -- Constructing the zero map over QQ
     d1 := map(QQ^0,QQ^0,0);
     -- Constructing the matrix of the sequence for the cohomology
     if k > 0 then (
	  scan(pairs M1, (a,b) -> (
	       -- 'A' will be a column of the matrix d1 of the sequence
	       A := map(QQ^0,QQ^(#b),0);
	       -- One intersection in M1 is selected, by going through the intersections in M2 we get the first "column" of block matrices in A 
	       -- by looking at the images in all intersections in M2
	       scan(pairs(M2), (c,d) -> (
			 -- Only if the intersection is made by intersecting with one more cone, the resulting matrix has to be computed, 
			 -- because otherwise it is automatically zero
			 if (isSubset(a,c)) then (
			      -- get the signum by looking at the position the new cone is inserted
			      signum := (-1)^(#c-(position(c, e -> (not member(e,a))))-1);
			      i := a#0;
			      j := c#0;
			      -- if i == j then no base change between the two representations has to be made, so the submatrix of the 
			      -- identity inserting the positions of the degrees 'b' into the degrees 'd' is added in this column
			      if (i == j) then (
				   A = A || (signum*(((map(QQ^rk,QQ^rk,1))_b))))
			      -- Otherwise we have to find the transition matrix from cone 'i' to Cone 'j'
			      else (
				   -- find the transition matrix
				   mpath := findpath(i,j,keys(bCT));
				   -- If the path has one element then we take the 'b'-'d' part of that matrix, otherwise the multiplication 
				   -- of the matrices corresponding to the steps in the path and add the path as a new step with corresponding matrix
				   if (#mpath == 1) then (
					if (i < j) then (
					     A = A || (signum*(((bCT#(i,j))_b))))
					else (
					     A = A || (signum*(((inverse(bCT#(j,i)))_b)))))
				   else (
					A1 := map(QQ^rk,QQ^rk,1);
					scan(mpath, p -> (
						  if (p#0 < p#1) then (
						       A1 = bCT#p * A1)
						  else (
						       A1 = (inverse(bCT#(p#1,p#0)))*A1)));
					if (i < j) then (
					     bCT = hashTable join(apply(pairs bCT, ps -> (ps#0 => ps#1)), {(i,j) => A1}))
					else (
					     bCT = hashTable join(apply(pairs bCT, ps -> (ps#0 => ps#1)), {(j,i) => inverse(A1)}));
					A = A || (signum*((A1_b))))))
			 else (
			      A = A || map(QQ^(rk),QQ^(#b),0))));
	       -- Adding the new column to d1
	       if (d1 == map(QQ^0,QQ^0,0)) then (
		    d1 = A)
	       else (
		    d1 = d1 | A))));
     -- constructing d2 in the same way as d1
     d2 := map(QQ^0,QQ^0,0);
     scan(pairs(M2), (a,b) -> (
	       A := map(QQ^0,QQ^(#b),0);
	       scan(pairs(M3), (c,d) -> (
			 if (isSubset(a,c)) then (
			      signum := (-1)^(#c-(position(c, e -> (not member(e,a))))-1);
			      i := a#0;
			      j := c#0;
			      if (i == j) then (
				   A = A || (signum*(((map(QQ^rk,QQ^rk,1))_b))))
			      else (
				   mpath := findpath(i,j,keys(bCT));
				   if (#mpath == 1) then (
					if (i < j) then (
					     A = A || (signum*(((bCT#(i,j))_b))))
					else (
					     A = A || (signum*(((inverse(bCT#(j,i)))_b)))))
				   else (
					A1 := map(QQ^rk,QQ^rk,1);
					scan(mpath, p -> (
						  if (p#0 < p#1) then (
						       A1 = bCT#p * A1)
						  else (
						       A1 = (inverse(bCT#(p#1,p#0)))*A1)));
					if (i < j) then (
					     bCT = hashTable join(apply(pairs bCT, ps -> (ps#0 => ps#1)), {(i,j) => A1}))
					else (
					     bCT = hashTable join(apply(pairs bCT, ps -> (ps#0 => ps#1)), {(j,i) => inverse(A1)}));
					A = A || (signum*((A1_b))))))
			 else (
			      A = A || map(QQ^(rk),QQ^(#b),0))));
	       if (d2 == map(QQ^0,QQ^0,0)) then (
		    d2 = A)
	       else (
		    d2 = d2 | A)));
     if (k == 0) then ( (rank(ker(d2))))
     else ((rank(ker(d2))-rank(image(d1)))))


cohom (ZZ,ToricVectorBundleK,Matrix) := (k,tvb,u) -> (
     -- Checking for input errors
     if ((numgens target u) != (tvb#"dimension of the variety")) or ((numgens source u) != 1) then (
	  error ("The degree matrix must be a vector of variety dimension"));
     if ((ring u) =!= ZZ) then (
	  error ("The degree must be an integer vector"));
     if ((k < 0) or (tvb#"dimension of the variety" < k)) then (
	  error ("k must be between 0 and the variety dimension for the k-th cohomolgy"));
     -- Defining "intersectMatrices" to compute the intersection of the image of two matrices
     intersectMatrices := (M,N) -> (
	  m := numColumns M;
	  N = gens ker(M | N);
	  N = N^{0..m-1};
	  gens trim image(M*N));     
     -- Extracting neccessary data
     rT := tvb#"rayTable";
     rT = hashTable apply(pairs rT, p -> (p#1 => p#0));
     fMT := tvb#"filtrationMatricesTable";
     bT := tvb#"baseTable";
     n := tvb#"dimension of the variety";
     tvbR := tvb#"ring";
     tvbrank := tvb#"rank of the vector bundle";
     F1 := {};
     F2 := {};
     F1toF2 := {};
     F3 := {};
     F2toF3 := {};
     F1columns := {};
     F2columns := {};
     if (k > 0) then (
	  F1 = cones(n-k+1,tvb#"ToricVariety");
	  F1 = hashTable apply(#F1, Cnum -> (
		    C := F1#Cnum;
		    R := rays C;
		    R = apply(numColumns R, i -> (R_{i}));
		    R = sort apply(R, r -> (rT#r,r));
		    Esum := apply(R, r -> (r#0,((transpose u)*(r#1))_(0,0)));
		    R = apply(R, r -> (r#1));
		    Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#0)), j -> (j <= e#1))));
		    if any(Esum, e -> e#1 == {}) then Cnum => (R,map(tvbR^tvbrank,tvbR^0,0)) else (
			 E := map(tvbR^tvbrank,tvbR^tvbrank,1);
			 Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
			 Esum = apply(Esum, e -> (bT#(e#0))_(e#1));
			 scan(Esum, A -> E = intersectMatrices(E,A));
			 Cnum => (R,E))));		    
	  if (k == n) then (
	       F2 = hashTable { 0 => ({},map(tvbR^tvbrank,tvbR^tvbrank,1))};
	       F1toF2 = hashTable apply(pairs F1, (j,dat) -> (
			  F1columns = append(F1columns,j => numColumns(dat#1));
			 (j,0) => dat#1));
	       F3 = hashTable { 0 => ({},map(tvbR^tvbrank,tvbR^0,0))};
	       F2toF3 = hashTable {};
	       F2columns = append(F2columns,0 => tvbrank))
	  else (
	       F2 = {};
	       F1toF2 = {};
	       counter := 0;
	       scan(pairs(F1), (num,dat) -> (
			 R := dat#0;
			 Er := dat#1;
			 scan(#R, i -> (
				   Ri := drop(R,{i,i});
				   pos := position(F2, f -> f#1 === Ri);
				   if pos =!= null then F1toF2 = append(F1toF2,(num,pos,((-1)^i)*Er)) else (
					Esum := apply(Ri, r -> (rT#r,((transpose u)*r)_(0,0)));
					Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#0)), j -> (j <= e#1))));
					if any(Esum, e -> e#1 == {}) then F2 = append(F2,(counter,Ri,map(tvbR^tvbrank,tvbR^0,0))) else (
					     E := map(tvbR^tvbrank,tvbR^tvbrank,1);
					     Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
					     Esum = apply(Esum, e -> (bT#(e#0))_(e#1));
					     scan(Esum, A -> E = intersectMatrices(E,A));
					     F2 = append(F2,(counter,Ri,E)));
					F1toF2 = append(F1toF2,(num,counter,((-1)^i)*Er));
					counter = counter + 1)));
			 F1columns = append(F1columns,num => numColumns Er)));
	       F2 = hashTable apply(F2, f -> f#0 => (f#1,f#2));
	       F1toF2 = hashTable apply(F1toF2, f -> (f#0,f#1) => f#2)))
     else (
	  F1 = hashTable { 0 => ({},map(tvbR^tvbrank,tvbR^0,0))};
	  F1toF2 = hashTable {};
	  F1columns = append(F1columns,0 => 0);
	  F2 = cones(n-k,tvb#"ToricVariety");
	  F2 = hashTable apply(#F2, Cnum -> (
		    C := F2#Cnum;
		    R := rays C;
		    R = apply(numColumns R, i -> (R_{i}));
		    R = sort apply(R, r -> (rT#r,r));
		    Esum := apply(R, r -> (r#0,((transpose u)*(r#1))_(0,0)));
		    R = apply(R, r -> (r#1));
		    Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#0)), j -> (j <= e#1))));
		    if any(Esum, e -> e#1 == {}) then Cnum => (R,map(tvbR^tvbrank,tvbR^0,0)) else (
			 E := map(tvbR^tvbrank,tvbR^tvbrank,1);
			 Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
			 Esum = apply(Esum, e -> (bT#(e#0))_(e#1));
			 scan(Esum, A -> E = intersectMatrices(E,A));
			 Cnum => (R,E)))));	  		    
     if (k == n-1) then (
	  F3 = hashTable { 0 => ({},map(tvbR^tvbrank,tvbR^tvbrank,1))};
	  F2toF3 = hashTable apply(pairs F2, (j,dat) -> (
			 F2columns = append(F2columns,j => numColumns(dat#1));
			 (j,0) => dat#1)))
     else if (k < n-1) then (
	  F3 = {};
	  F2toF3 = {};
	  counter = 0;
	  scan(pairs(F2), (num,dat) -> (
			 R := dat#0;
			 Er := dat#1;
			 scan(#R, i -> (
				   Ri := drop(R,{i,i});
				   pos := position(F3, f -> f#1 === Ri);
				   if pos =!= null then F2toF3 = append(F2toF3,(num,pos,((-1)^i)*Er)) else (
					Esum := apply(Ri, r -> (rT#r,((transpose u)*r)_(0,0)));
					Esum = apply(Esum, e -> (e#0,positions(flatten entries(fMT#(e#0)), j -> (j <= e#1))));
					if any(Esum, e -> e#1 == {}) then F3 = append(F3,(counter,Ri,map(tvbR^tvbrank,tvbR^0,0))) else (
					     E := map(tvbR^tvbrank,tvbR^tvbrank,1);
					     Esum = select(Esum, e -> sort(e#1) != toList(0..tvbrank-1));
					     Esum = apply(Esum, e -> (bT#(e#0))_(e#1));
					     scan(Esum, A -> E = intersectMatrices(E,A));
					     F3 = append(F3,(counter,Ri,E)));
					F2toF3 = append(F2toF3,(num,counter,((-1)^i)*Er));
					counter = counter + 1)));
			 F2columns = append(F2columns,num => numColumns Er)));
	  F3 = hashTable apply(F3, f -> f#0 => (f#1,f#2));
	  F2toF3 = hashTable apply(F2toF3, f -> (f#0,f#1) => f#2));
     F1columns = hashTable F1columns;
     F2columns = hashTable F2columns;
     MapF1toF2 := matrix apply(#F2, j -> apply(#F1, i -> if F1toF2#?(i,j) then F1toF2#(i,j) else map(tvbR^tvbrank,tvbR^(F1columns#i),0)));
     MapF2toF3 := matrix apply(#F3, j -> apply(#F2, i -> if F2toF3#?(i,j) then F2toF3#(i,j) else map(tvbR^tvbrank,tvbR^(F2columns#i),0)));
     (rank ker MapF2toF3)-(rank image MapF1toF2))


cohomology(ZZ,ToricVectorBundle,Matrix) := opts -> (i,T,weight)->cohom(i,T,weight)

cohomology(ZZ,ToricVectorBundleK,Matrix) := opts -> (i,T,weight)->cohom(i,T,weight)

cohomology(ZZ,ToricVectorBundle,List) := opts -> (i,T,P)-> (
	print ("Number of degrees to calculate: "|(toString (#P)));
	L := apply(#P,j->(<< "." << flush;{P_j,cohomology(i,T,P_j)}));
	select(L,e-> (e_1 != 0)))

cohomology(ZZ,ToricVectorBundleK,List) := opts -> (i,T,P)-> (
	print ("Number of degrees to calculate: "|(toString (#P)));
	L := apply(#P,j->(<< "." << flush;{P_j,cohomology(i,T,P_j)}));
	select(L,e-> (e_1 != 0))) 

cohomology(ZZ,ToricVectorBundle) := opts -> (i,T)-> cohomology(i,T,latticePoints deltaE T)

cohomology(ZZ,ToricVectorBundleK) := opts -> (i,T)-> cohomology(i,T,latticePoints deltaE T)
     	       
	       
-- PURPOSE : Computing the cotangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle 
cotangentBundle = method(TypicalValue => ToricVectorBundle)
cotangentBundle Fan := F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety must be smooth");
     if (not isPure F) or (dim F != ambDim F) then error("The Toric Variety must be pure and full dimensional");
     -- Generating the trivial bundle of dimension n
     n := dim F;
     tvb := makeVB(n,F);
     tCT := sort keys tvb#"topConeTable";
     pairlist := keys tvb#"baseChangeTable";
     -- Computing the degrees and transition matrices of the cotangent bundle
     degreeTable := hashTable apply(tCT, p -> p => substitute(rays dualCone p,ZZ));
     baseChangeTable := hashTable apply(pairlist, p -> ( p => substitute(inverse(degreeTable#(tCT#(p#1)))*(degreeTable#(tCT#(p#0))),QQ)));
     -- Writing the data into the bundle
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => n,
	  "rank of the vector bundle" => n,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


-- PURPOSE : Computing the cotangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle K
cotangentBundleK = method(TypicalValue => ToricVectorBundleK)
cotangentBundleK Fan := F -> dualVB tangentBundleK F


-- PURPOSE : Computing the polytope deltaE in the degree space such that outside this polytope
--     	     every cohomology is 0 
--   INPUT : 'tvb',  a ToricVectorBundle
--  OUTPUT : a Polyhedron
deltaE = method()
deltaE ToricVectorBundle := tvb -> (
     -- Extracting neccesary data
     raylist := rays(tvb#"ToricVariety");
     l := #raylist;
     k := tvb#"rank of the vector bundle";
     n := tvb#"dimension of the variety";
     tCT := sort keys tvb#"topConeTable";
     dT := tvb#"degreeTable";
     -- Creating an index table, for each ray the first top cone containing it
     raytCTindex := hashTable apply(#raylist, r -> r => position(tCT, C -> contains(C,raylist#r)));
     raylist = transpose matrix {raylist};
     -- Get the subsets of 'n' elements in 'l'
     sset := subsets(toList(0..l-1),n);
     jList := {{}};
     -- Get all different combinations of choices of variety dimension many degree vectors
     scan(n, i -> jList = flatten apply(jList, l -> apply(k, j -> l|{j})));
     M := map(QQ^1,QQ^n,0);
     v := map(QQ^1,QQ^1,0);
     -- For every 'n' in 'l' subset and any combination in jList get the intersection of the dual cones
     -- of the corresponding rays. If this is a non-empty compact polytope then add the vertices to the
     -- list L
     L := unique flatten apply(sset, s -> (
	       unique flatten apply(jList, j -> (
			 N := matrix apply(n, i -> {raylist^{s#i},raylist^{s#i} * ((dT#(tCT#(raytCTindex#(s#i))))_{j#i})});
			 w := N_{n};
			 N = submatrix'(N,{n});
			 P := intersection(M,v,N,w);
			 if isCompact P and (not isEmpty P) then vertices P else {}))));
     -- Make a matrix of all the vertices in L
     M = matrix {L};
     convexHull M)


deltaE ToricVectorBundleK := tvb -> (
     -- Extracting neccesary data
     raylist := tvb#"rayTable";
     l := #raylist;
     n := tvb#"dimension of the variety";
     fMT := hashTable apply(pairs tvb#"filtrationMatricesTable", (i,j) -> (j=flatten entries j; i => promote(matrix {{-(min j),max j}},QQ)));
     sset := select(subsets(toList(0..l-1),n), s -> (M := matrix {apply(s, i -> raylist#i)}; rank M == n));
     convexHull matrix {apply(sset, s -> (
		    M := transpose matrix {apply(s, i -> (-(raylist#i) | raylist#i) || (fMT#i))};
		    vertices intersection(M_{0..n-1},M_{n})))})
     
     

-- PURPOSE : Computing the direct sum of two ToricVectorBundles over the same Fan
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the direct sum
dsum = method(TypicalValue => ToricVectorBundle)
dsum (ToricVectorBundle,ToricVectorBundle) := (tvb1,tvb2) -> (
	  -- Checking for input errors
	  if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("The bundles must be over the same toric variety!");
	  -- Extracting data out of tvb1 and tvb2
	  k1 := tvb1#"rank of the vector bundle";
	  k2 := tvb2#"rank of the vector bundle";
	  urmat := map(QQ^k1,QQ^k2,0);
	  llmat := map(QQ^k2,QQ^k1,0);
	  -- Generating the trivial bundle of dimension k1+k2
	  tvb := makeVB(k1 + k2,tvb1#"ToricVariety");
	  -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
	  new ToricVectorBundle from {
	       "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> a|b),
	       "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> (a|urmat)||(llmat|b)),
	       "ToricVariety" => tvb#"ToricVariety",
	       "number of affine charts" => tvb#"number of affine charts",
	       "dimension of the variety" => tvb#"dimension of the variety",
	       "rank of the vector bundle" => k1 + k2,
	       "codim1Table" => tvb#"codim1Table",
	       "topConeTable" => tvb#"topConeTable"})
     
     
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleK over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleK which is the direct sum
dsum (ToricVectorBundleK,ToricVectorBundleK) := (tvb1,tvb2) -> (
          -- Making the bundles compatible
	  tvb2 = makeCompatible(tvb1,tvb2);
	  -- Extracting data out of tvb1 and tvb2
	  k1 := (tvb1#"rank of the vector bundle");
	  k2 := (tvb2#"rank of the vector bundle");
	  k := k1 + k2;
	  F := tvb1#"ToricVariety";
	  R := tvb1#"ring";
	  tvb := makeVBK(k,F);
	  r := #(tvb1#"rayTable");
	  fT1 := tvb1#"filtrationMatricesTable";
	  fT2 := tvb2#"filtrationMatricesTable";
	  bT1 := tvb1#"baseTable";
	  bT2 := tvb2#"baseTable";
	  filtrationTable := apply(r, i -> ( fT1#i | fT2#i));
	  baseTable := apply(r, i -> ( (bT1#i | map(R^k1,R^k2,0)) || (map(R^k2,R^k1,0) | bT1#i)));
	  tvb = addFiltration(tvb,filtrationTable);
	  addBase(tvb,baseTable))

     
ToricVectorBundle ++ ToricVectorBundle := (tvb1,tvb2) -> dsum(tvb1,tvb2)
ToricVectorBundleK ++ ToricVectorBundleK := (tvb1,tvb2) -> dsum(tvb1,tvb2)


-- PURPOSE : Computing the dual bundle to a given ToricVectorBundle
--   INPUT : 'tvb',  a ToricVectorBundle 
--  OUTPUT : the dual ToricVectorBundle 
dualVB = method(TypicalValue => ToricVectorBundle)
dualVB(ToricVectorBundle) := tvb -> (
     -- Inverting the degrees and the transition matrices
     degreeTable := hashTable apply(pairs(tvb#"degreeTable"), p -> ( p#0 => -(p#1)));
     baseChangeTable := hashTable apply(pairs(tvb#"baseChangeTable"), p -> ( p#0 => transpose(inverse(p#1))));
     -- Writing the inverted tables into the bundle
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => tvb#"rank of the vector bundle",
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})

dualVB(ToricVectorBundleK) := tvb -> (
     -- Inverting the filtration. If the filtration has d steps then the new n-th boundary is -(d-n+1th boundary)-1 and the n-th step is the 
     -- d-n+2 th step
     fT := hashTable apply(pairs tvb#"filtrationTable", (num,e) -> num => (k:=sort keys e;e = apply(#k-1, i -> (k#i,e#(k#(i+1))))|{(last(k),{})}; hashTable apply(e, entry -> -(entry#0)-1 => entry#1)));
     fMT := hashTable apply(pairs fT, q -> q#0 => (q1new:= hashTable flatten apply(pairs q#1, p -> apply(p#1, i -> i => p#0)); matrix {apply(#q1new, j -> q1new#j)}));
     -- The orthogonal complement is given by the transpose of the inverse matrix
     bT := hashTable apply(pairs tvb#"baseTable", p -> p#0 => transpose inverse p#1);
     new ToricVectorBundleK from {
		    "ring" => tvb#"ring",
		    "rayTable" => tvb#"rayTable",
		    "baseTable" => bT,
		    "filtrationMatricesTable" => fMT,
		    "filtrationTable" => fT,
		    "ToricVariety" => tvb#"ToricVariety",
		    "number of affine charts" => tvb#"number of affine charts",
		    "dimension of the variety" => tvb#"dimension of the variety",
		    "rank of the vector bundle" => tvb#"rank of the vector bundle",
		    "number of rays" => tvb#"number of rays"})

-- PURPOSE : Computing the 'l'-th exterior power of a ToricVectorBundle
--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundleK
--  OUTPUT : 'tvb',  a ToricVectorBundleK which is the 'l'-th exterior power
extPower = method()
extPower (ZZ,ToricVectorBundleK) := (l,tvb) -> (
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     if l > k then error("The power must not be greater than the rank of the bundle");
     -- Extracting data
     baseTable := tvb#"baseTable";
     filtrationTable := tvb#"filtrationMatricesTable";
     r := tvb#"number of rays";
     R := tvb#"ring";
     F := tvb#"ToricVariety";
     -- Generating the list of 'l'-tuples of 0..k-1 and the corresponding index table
     ind := subsets(toList(0..k-1),l);
     indtable = hashTable apply(#ind, i -> ind#i => i);
     -- Computing the 'l'-th exterior powers of the base matrices
     baseTable = apply(r, i -> (
	       B := baseTable#i;
	       M := mutableMatrix(R,#ind,#ind);
	       scan(ind, j -> (
			 scan(ind, k -> (
				   M_(indtable#k,indtable#j) = det(B^k_j)))));
	       matrix M));
     -- Computing the 'l'-th exterior power of the filtration matrices
     filtrationTable = apply(r, i -> (
	       filt := filtrationTable#i;
	       matrix {apply(ind, j -> ( sum flatten entries filt_j))}));
     makeVBK(#ind,F,baseTable,filtrationTable))


--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundle
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the 'l'-th exterior power
extPower (ZZ,ToricVectorBundle) := (l,tvb) -> (
     k := tvb#"rank of the vector bundle";
     -- Checking for input errors
     if l < 1 then error("The power must be strictly positive");
     if l > k then error("The power must not be greater than the rank of the bundle");
     -- Generating the list of 'l'-tuples of 0..k-1 and the corresponding index table
     ind := subsets(toList(0..k-1),l);
     indtable := hashTable apply(#ind, i -> ind#i => i);
     -- Computing the 'l'-th exterior powers of the transition matrices
     baseChangeTable := hashTable apply(pairs tvb#"baseChangeTable", p -> p#0 =>  matrix apply(ind, j -> apply(ind, k -> det (p#1)^j_k)));
     -- Computing the 'l'-th exterior power of the degrees
     degreeTable := hashTable apply(pairs tvb#"degreeTable", p -> p#0 => matrix {apply(ind, j -> (p#1)_j * matrix toList(l:{1}))});
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => #ind,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


-- PURPOSE : Giving the rays of the underlying Fan of a toric vector bundle
--   INPUT : 'tvb',  a TorcVectorBundleK
--  OUTPUT : 'L',  a List containing the rays of the Fan underlying the bundle
rays ToricVectorBundleK := tvb -> rays tvb#"ToricVariety"

--   INPUT : 'tvb',  a TorcVectorBundleK
--  OUTPUT : 'L',  a List containing the rays of the Fan underlying the bundle
rays ToricVectorBundle := tvb -> rays tvb#"ToricVariety"


-- PURPOSE : Computing the 'l'-th symmetric product of a ToricVectorBundle
--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundleK
--  OUTPUT : 'tvb',  a ToricVectorBundleK which is the 'l'-th exterior power
symmProd = method()
symmProd(ZZ,ToricVectorBundleK) := (l,tvb) -> (
     -- Checking for input errors
     if (l < 1) then (
	  error ("The power must be strictly positive"));
     -- Extracting data
     k := tvb#"rank of the vector bundle";
     baseTable := tvb#"baseTable";
     filtrationTable := tvb#"filtrationMatricesTable";
     r := tvb#"number of rays";
     R := tvb#"ring";
     F := tvb#"ToricVariety";
     -- Generating the list of 'l'-tuples of 0..k-1 with duplicates and the corresponding index table
     ind := toList(0..k-1);
     allind := sort unique subsets(flatten toList(l:ind),l);
     ind = sort unique subsets(sort flatten toList(l:ind),l);
     indtable = hashTable apply(#ind, i -> ind#i => i);
     -- Computing the 'l'-th symmetric product of the base matrices
     baseTable = apply(r, i -> (
	       B := baseTable#i;
	       M := mutableMatrix(R,#ind,#ind);
	       scan(ind, i1 -> (
			 Bi := B_(i1);
			 scan(allind, j -> (
				   M_(indtable#(sort(j)),indtable#i1) = (M_(indtable#(sort(j)),indtable#i1) + product(apply(#j, j1 -> (Bi_(j#j1,j1)))))))));
	       matrix M));
     -- Computing the 'l'-th symmetric products of the filtration matrices
     filtrationTable = apply(r, i -> (
	       filt := filtrationTable#i;
	       matrix {apply(ind, j -> sum flatten entries filt_j)}));
     makeVBK(#ind,F,baseTable,filtrationTable))


--   INPUT : '(l,tvb)',  where 'l' is a strictly positive integer and 'tvb'is a TorcVectorBundle
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the 'l'-th exterior power
symmProd(ZZ,ToricVectorBundle) := (l,tvb) -> (
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
	       scan(ind, i1 -> (
			 Bi := B_(i1);
			 scan(allind, j -> (
				   M_(indtable#(sort(j)),indtable#i1) = (M_(indtable#(sort(j)),indtable#i1) + product(apply(#j, j1 -> (Bi_(j#j1,j1)))))))));
	       M = matrix M;
	       p#0 => M));
     -- Computing the 'l'-th symmetric products of the degrees
     degreeTable := hashTable apply(pairs tvb#"degreeTable", p -> (
	       dM := p#1;
	       dM = apply(ind, j -> (
			 flatten(entries((dM_j)*(matrix toList((#j):{1}))))));
	       dM = transpose matrix dM;			      
	       p#0 => dM));
     new ToricVectorBundle from {
	  "degreeTable" => degreeTable,
	  "baseChangeTable" => baseChangeTable,
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => #ind,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


-- PURPOSE : Computing the tangent bundle on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle 
tangentBundle = method(TypicalValue => ToricVectorBundle)
tangentBundle Fan := F -> dualVB cotangentBundle F


-- PURPOSE : Computing the tangent bundle (Klyachko) on a smooth, pure, and full dimensional Toric Variety 
--   INPUT : 'F',  a smooth, pure, and full dimensional Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleK 
tangentBundleK = method()
tangentBundleK Fan := F -> (
     -- Checking for input errors
     if not isSmooth F then error("The Toric Variety must be smooth");
     if not isPure F or dim F != ambDim F then error("The Toric Variety must be pure and full dimensional");
     -- Generating the trivial bundle of dimension n
     n := dim F;
     tvb := makeVBK(n,F);
     -- Extracting the rayTable
     rayTable := tvb#"rayTable";
     -- Adding the filtration matrix |-1,0,0,...,0| for each ray
     filtrationTable := apply(#rayTable, p -> matrix{flatten({-1,toList(n-1:0)})});
     -- Adding the base which has as first vector the ray itself to each ray
     baseTable := apply(#rayTable, p -> rayTable#p | complement rayTable#p);
     -- Adding bases filtration matrices to the bundle
     tvb = addFiltration(tvb,filtrationTable);
     addBase(tvb,baseTable))


-- PURPOSE : Checking if the two ToricVectorBundle are equal
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle
--  OUTPUT : 'true' or 'false' 
ToricVectorBundle == ToricVectorBundle := (tvb1,tvb2) -> tvb1 === tvb2


-- PURPOSE : Checking if the two ToricVectorBundlK are equal
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleK
--  OUTPUT : 'true' or 'false' 
ToricVectorBundleK == ToricVectorBundleK := (tvb1,tvb2) -> (
     -- Check if they are over the same Fan
     equality := tvb1#"ToricVariety" == tvb2#"ToricVariety";
     if equality then (
	  -- Check if they have the same rank
	  equality = ((tvb1#"rank of the vector bundle") == (tvb2#"rank of the vector bundle"));
	  if equality then (
	       equality = tvb1#"ring" === tvb2#"ring";
	       if equality then (
		    -- Make them compatible to compare the bases and filtrations
		    tvb2 = makeCompatible(tvb1,tvb2);
		    i := 0;
		    n := tvb1#"number of rays";
		    k := tvb1#"rank of the vector bundle";
		    -- Check for each ray:	    
		    while i < n and equality do (
			 fL1 := (tvb1#"filtrationTable")#i;
			 fL2 := (tvb2#"filtrationTable")#i;
			 -- if the filtrations have the same keys
			 equality = keys fL1 == keys fL2;
			 -- If they have then check if the filtrations are the same
			 if equality then (
			      fL1 = drop(values fL1,1);
			      fL2 = drop(values fL2,1);
			      bM1 := (tvb1#"baseTable")#i;
			      bM2 := (tvb2#"baseTable")#i;
			      B1 := {};
			      B2 := {};
			      while fL1 != {} and equality do (
				   B1 = B1 | fL1#0;
				   B2 = B2 | fL2#0;
				   fL1 = drop(fL1,1);
				   fL2 = drop(fL2,1);
				   equality = image(bM1_B1) == image(bM2_B2)));
			 i = i + 1))));
     equality)

      
-- PURPOSE : Computing the tensor product of two ToricVectorBundles over the same Fan
--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundle over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundle which is the tensor product
tproduct = method(TypicalValue => ToricVectorBundle)
tproduct (ToricVectorBundle,ToricVectorBundle) := (tvb1,tvb2) -> (
     -- Checking for input errors
     if tvb1#"ToricVariety" != tvb2#"ToricVariety" then error("The bundles must be over the same toric variety!");
     -- Extracting data out of tvb1 and tvb2
     k1 := tvb1#"rank of the vector bundle";
     k2 := tvb2#"rank of the vector bundle";
     -- Generating the trivial bundle of dimension k1+k2
     tvb := makeVB(k1 * k2,tvb1#"ToricVariety");
     -- Computing the new degree table and transition matrices and writing the degrees and transition matrices into the bundle
     new ToricVectorBundle from {
	  "degreeTable" => merge(tvb1#"degreeTable",tvb2#"degreeTable", (a,b) -> matrix {flatten apply(k2, j -> apply(k1, i -> a_{i}+b_{j}))}),
	  "baseChangeTable" => merge(tvb1#"baseChangeTable",tvb2#"baseChangeTable", (a,b) -> (
		    matrix flatten apply(k2, j -> apply(k1, i -> flatten apply(k2, j' -> apply(k1, i' -> a_(i,i') * b_(j,j'))))))),
	  "ToricVariety" => tvb#"ToricVariety",
	  "number of affine charts" => tvb#"number of affine charts",
	  "dimension of the variety" => tvb#"dimension of the variety",
	  "rank of the vector bundle" => k1 + k2,
	  "codim1Table" => tvb#"codim1Table",
	  "topConeTable" => tvb#"topConeTable"})


--   INPUT : '(tvb1,tvb2)',  two ToricVectorBundleK over the same Fan
--  OUTPUT : 'tvb',  a ToricVectorBundleK which is the tensor product
tproduct (ToricVectorBundleK,ToricVectorBundleK) := (tvb1,tvb2) -> (
     -- Making the two bundles compatible
     tvb2 = makeCompatible(tvb1,tvb2);
     -- Extracting data out of tvb1 and tvb2
     k1 := tvb1#"rank of the vector bundle";
     k2 := tvb2#"rank of the vector bundle";
     n := tvb1#"dimension of the variety";
     F := tvb1#"ToricVariety";
     bT1 := tvb1#"baseTable";
     bT2 := tvb2#"baseTable";
     fmT1 := tvb1#"filtrationMatricesTable";
     fmT2 := tvb2#"filtrationMatricesTable";
     r := #(tvb1#"rayTable");
     -- Computing the bases and filtration matrices
     k := k1 * k2;
     baseTable := apply(r, i -> (bT1#i) ** (bT2#i));
     filtrationTable := apply(r, i -> (
	       f1 := fmT1#i;
	       f2 := fmT2#i;
	       matrix {flatten apply(flatten entries f1, e1 -> apply(flatten entries f2, e2 -> e1 + e2))}));
     -- Writing the new Tables into the bundle
     tvb := makeVBK(k,F);
     tvb = addBase(tvb,baseTable);
     addFiltration(tvb,filtrationTable))

     
ToricVectorBundle ** ToricVectorBundle := tproduct
ToricVectorBundleK ** ToricVectorBundleK := tproduct


-- PURPOSE : Generating the Vector Bundle given by a divisor
--   INPUT : '(L,F)',  a list 'L' of weight vectors, one for each ray of the Fan 'F'
--  OUTPUT : 'tvb',  a ToricVectorBundle
weilToCartier = method(TypicalValue => ToricVectorBundle)
weilToCartier (List,Fan) := (L,F) -> (
     rl := rays F;
     -- Checking for input errors
     if not isPure F or ambDim F != dim F then error("The Fan must be pure of maximal dimension.");
     if #L != #rl then error("The number of weights must equal the number of rays.");
     n := ambDim F;
     -- Creating 0 matrices to compute interssection of hyperplanes to  compute the degrees
     Mfull := matrix {toList(n:0)};
     vfull := matrix {{0}};
     -- Checking for further errors and assigning the weights to the rays
     L = hashTable apply(#rl, i -> (
	       if class L#i =!= ZZ then error("The weights must be in ZZ.");
	       rl#i => L#i));
     -- Keeping track of the lowest common multiple of denominators of the degrees,
     -- to check wether the divisor itself is Cartier or which multiple
     denom := 1;
     lcm := (a,b) -> (substitute((a*b)/gcd(a,b),ZZ));
     -- Computing the degree vector for every top dimensional cone
     gC := genCones F;
     gC = apply(gC, C -> (
	       rC := rays C;
	       -- Taking the first n x n submatrix
	       rC1 := rC_{0..n-1};
	       -- Setting up the solution vector by composing the corresponding weights
	       v := matrix apply(n, i -> (c := rC1_{i}; {-(L#c)}));
	       -- Computing the degree vector
	       w := vertices intersection(Mfull,vfull,transpose rC1,v);
	       -- Checking if w also fulfils the equations given by the remaining rays
	       if numColumns rC != n then (
		    v = v || matrix apply(n..(numColumns rC)-1, i -> {-(L#(rC_{i}))});
		    if (transpose rC)*w != v then error("The weights do not define a Weil divisor."));
	       -- Check if w is Cartier
	       scan(flatten entries w, e -> denom = lcm(denominator e ,denom));
	       w));
     -- If the divisor is only QQ Cartier, the its replaced by its first Cartier multiple
     if denom != 1 then print(toString(denom)|" times the divisor is Cartier, which is the output.");
     gC = apply(gC, e -> substitute(denom*e,ZZ));
     -- Construct the actual line bundle
     tvb := makeVB(1,F);
     addDegrees(tvb,gC))

hypercubefan = method()
hypercubefan ZZ := n -> normalFan hypercube n

hirzebruchfan = method()
hirzebruchfan ZZ := n -> hirzebruch n

---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     	Key => ToricVB,
	Headline => "for cohomology computations of equivariant vector bundles on toric varieties",
	
	"Using the descriptions of Kaneyama and Klyachko this package implements the construction of
	equivariant vector bundles on toric varieties.",
	
	PARA{}, TT "ToricVB", " uses the ", TO Polyhedra, " package by ", 
	HREF("http://page.mi.fu-berlin.de/rbirkner/", "René Birkner"), ". Each vector bundle is saved 
	either in the description of Kaneyama or the one of Klyachko. The first description gives the 
	multidegrees (in the dual lattice of the fan) of the generators of the bundle over each 
	full dimensional cone, and for each codim 1 cone a transition matrix. The description by 
	Klyachko has for each ray of the fan a filtration of the vector bundle.",
	
	PARA{}, "For the mathematical background see ", EM "Computing Cech Cohomology of Vector Bundles 
	on Toric Varieties", ", R. Birkner, N. O. Ilten, and L. Petersen, in preparation"
	
	}
   
document {     
     Key => ToricVectorBundle,
     Headline => "the class of all toric vector bundles in Kaneyama's description",
     
     "A toric vector bundle of rank ",TT "k"," in Kaneyama's description is given in the following way: 
     First, the maximal dimensional cones in the ",TT "n","-dimensional fan are enumerated. There is 
     a ",TT "n"," times ",TT "k"," matrix over ",TO ZZ," assignd to each of these cones, which gives 
     ",TT "k"," degree vectors in the lattice of the fan, on for each generator of the bundle. Additionally, 
     for every pair of maximal cones that intersects in a common codimension 1 face, there is a matrix in 
     GL(",TT "k",",",TO QQ,"), representing the base change between these two affine charts. The output of 
     a ToricVectorBundle gives an overview on the characteristics of the bundle:",
     
     EXAMPLE {
	  " tvb = cotangentBundle hirzebruchfan 3"
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,".",
     
     EXAMPLE {
	  " details tvb"
	  }
     
     }

document {     
     Key => ToricVectorBundleK,
     Headline => "the class of all toric vector bundles in Klyachko's description",
     
     "A toric vector bundle of rank ",TT "k"," in Klyachko's description is given in the following way: 
     First, the rays in the ",TT "n","-dimensional fan are enumerated. There is  a matrix in 
     GL(",TT "k",",",TO QQ,"), assignd to each of the rays, which gives the basis of the bundle over this ray. 
     Additionally, for every ray there is a ",TT "1"," times ",TT "k"," matrix over ",TO ZZ,", the filtration matrix, 
     that determines the filtration on the basis given before. I.e. if the j-th entry of the filtration matrix is i 
     then the j-th basis vector appears in the filtration at the i-th step. The output of a ToricVectorBundle gives 
     an overview on the characteristics of the bundle:",
     
     EXAMPLE {
	  " tvb = tangentBundleK hirzebruchfan 3"
	  },
     
     PARA{}, "To see all relevant details of a bundle use ",TO details,".",
     
     EXAMPLE {
	  " details tvb"
	  }
     
     }

document {
     Key => {addBaseChange, (addBaseChange,ToricVectorBundle,List)},
     Headline => "changing the transition matrices of a toric vector bundle",
     Usage => " tvb1 = addBaseChange(tvb,L)",
     Inputs => {
	  "tvb" => ToricVectorBundle,
	  "L" => List  => {"with matrices over ",TO ZZ," or ",TO QQ}
	  },
     Outputs => {
	  "tvb1" => ToricVectorBundle
	  },
     
     PARA{}, TT "addBaseChange"," replaces the transition matrices in ",TT "tvb"," by the matrices in 
     the ",TO List," ",TT "L",". The matrices in ",TT "L"," must be in GL(k,",TO ZZ,") or GL(k,",TO QQ,"), 
     where k is the rank of the vector bundle ",TT "tvb",". The list has to contain one matrix for each maximal 
     dimensional cone of the Fan ",TT "F"," over which ",TT "tvb"," is defined. The vector bundle already has a 
     list of pairs (i,j) denoting the codim 1 intersections of two maximal cones with i<j and they are ordered in 
     lexicographic order. The matrices will be assigned to the pairs (i,j) in that order. The matrix A assigned to 
     (i,j) denotes the transition (e_i^1,...,e_i^k) = (e_j^1,...,e_j^k)*A. The matrices must not satisfy the regularity 
     or the cocycle condition. These can be checked with ",TO regCheck," and ",TO cocycleCheck,".",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " details tvb",
	  " tvb1 = addBaseChange(tvb,{matrix{{1,2},{0,1}},matrix{{1,0},{3,1}},matrix{{1,-2},{0,1}},matrix{{1,0},{-3,1}}})",
	  " details tvb1",
	  " cocycleCheck tvb1"
	  }
     
     }

document {
     Key => {addDegrees, (addDegrees,ToricVectorBundle,List)},
     Headline => "changing the degrees of a toric vector bundle",
     Usage => " tvb1 = addDegrees(tvb,L)",
     Inputs => {
	  "tvb" => ToricVectorBundle,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "tvb1" => ToricVectorBundle
	  },
     
     PARA{}, TT "addDegrees"," replaces the degree matrices in ",TT "tvb"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be ",TT "n"," by ",TT "k"," matrices over ",TO ZZ,", where ",TT "k"," is the rank of the vector 
     bundle ",TT "tvb"," and ",TT "n"," is the dimension of the underlying toric variety. The list has to contain one matrix for 
     each maximal dimensional cone of the Fan ",TT "F"," over which ",TT "tvb"," is defined. Note that in ",TT "tvb"," the top 
     dimensional Cones are already numbered and that the degree matrices in ",TT "L"," will be assigned to the Cones in that 
     order. The matrices must not satisfy the regularity condition. This can be checked with ",TO regCheck,".",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " details tvb",
	  " tvb1 = addDegrees(tvb,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details tvb1",
	  " regCheck tvb1"
	  }
     
     }

document {
     Key => {addBase, (addBase,ToricVectorBundleK,List)},
     Headline => "changing the basis matrices of a toric vector bundle in Klyachko's description",
     Usage => "tvb1 = addBase(tvb,L)",
     Inputs => {
	  "tvb" => ToricVectorBundleK,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "tvb1" => ToricVectorBundleK
	  },
     
     PARA{}, TT "addBase"," replaces the basis matrices in ",TT "tvb"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be in GL(",TT "k",",R), where ",TT "k"," is the rank of the vector bundle ",TT "tvb",", for the same 
     field R (",TO ZZ," works as well). The list has to contain one matrix for each ray of the Fan ",TT "F"," over which ",TT "tvb"," 
     is defined. Note that in ",TT "tvb"," the rays are already numbered and that the basis matrices in ",TT "L"," will be assigned to the 
     rays in that order. To see the order, call ",TT "rays(tvb)",".",
     
     PARA{}, "The matrices must not satisfy the compatability condition. This can be checked with ",TO regCheck,".",
     
     EXAMPLE {
	  " tvb = makeVBK(2,hypercubefan 2)",
	  " details tvb",
	  " tvb1 = addBase(tvb,{matrix{{1,2},{3,1}},matrix{{-1,0},{3,1}},matrix{{1,2},{-3,-1}},matrix{{-1,0},{-3,-1}}})",
	  " details tvb1",
	  " regCheck tvb1"
	  }
     
     }

document {
     Key => {addFiltration, (addFiltration,ToricVectorBundleK,List)},
     Headline => "changing the filtration matrices of a toric vector bundle in Klyachko's description",
     Usage => "tvb1 = addFiltration(tvb,L)",
     Inputs => {
	  "tvb" => ToricVectorBundleK,
	  "L" => List => {"with matrices over ",TO ZZ}
	  },
     Outputs => {
	  "tvb1" => ToricVectorBundleK
	  },
     
     PARA{}, TT "addFiltration"," replaces the filtration matrices in ",TT "tvb"," by the matrices in the ",TO List," ",TT "L",". The 
     matrices in ",TT "L"," must be ",TT "1"," by ",TT "k"," matrices over ",TO ZZ,", where ",TT "k"," is the rank of the vector 
     bundle ",TT "tvb",". The list has to contain one matrix for each ray of the Fan ",TT "F"," over which ",TT "tvb"," is defined. 
     Note that in ",TT "tvb"," the rays are already numbered and that the filtration matrices in ",TT "L"," will be assigned to the 
     rays in that order. To see the order, call ",TT "rays(tvb)",".",
     
     PARA{}, "The filtration on the vector bundle over a ray is given by the filtration matrix for this ray in the following way. The 
     first index j such that the i-th basis vector in the basis over this ray appears in the j-th filtration is the i-th entry of 
     the filtration matrix. The matrices must not satisfy the compatability condition. This can be checked with ",TO regCheck,".",
     
     EXAMPLE {
	  " tvb = makeVBK(2,hypercubefan 2)",
	  " details tvb",
	  " tvb1 = addFiltration(tvb,{matrix{{1,3}},matrix{{-1,3}},matrix{{2,-3}},matrix{{0,-1}}})",
	  " details tvb1",
	  " regCheck tvb1"
	  }
     
     }

document {
     Key => {cocycleCheck, (cocycleCheck,ToricVectorBundle)},
     Headline => " checking if the toric vector bundle fulfills the cocycle condition",
     Usage => " b = cocycleCheck(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle 
	  },
     Outputs => {
	  "b" => Boolean
	  },
     
     PARA{}, "The transition matrices in ",TT "tvb"," define an equivariant toric vector bundle if they satify the cocycle condition. 
     I.e. for every codimension 2 cone of the fan the cycle of transition matrices of codimension 1 cones containing the codimension 2 
     cone gives the identity when multiplied.",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " details tvb",
	  " A = matrix{{1,2},{0,1}};",
	  " B = matrix{{1,0},{3,1}};",
	  " C = matrix{{1,-2},{0,1}};",
	  " tvb1 = addBaseChange(tvb,{A,B,C,matrix{{1,0},{0,1}}})",
	  " cocycleCheck tvb1",
	  " D = inverse(B)*A*C",
	  " tvb1 = addBaseChange(tvb,{A,B,C,D})",
	  " cocycleCheck tvb1"
	  }
     
     }

document {
     Key => {details, (details,ToricVectorBundle), (details,ToricVectorBundleK)},
     Headline => " returns the details of a toric vector bundle (for both descriptions",
     Usage => " ht = details(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle => {"or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "ht" => Sequence => {" or ",TO HashTable," if the bundle is in Klyachko's description"}
	  },
     
     PARA{}, "For a toric vector bundle in Kaneyama's description, the sequence ",TT "ht"," contains the table of full dimensional 
     cones, the table assigning the degrees to these cones and the table giving a transition matrix for every pair of maximal cones 
     that intersect in a codimension 1 face.",
     
     EXAMPLE {
	  " tvb = tangentBundle hypercubefan 2",
	  "details tvb"
	  },
     
     PARA{}, "For a toric vector bundle in Klyachko's description, the hash table ",TT "ht"," contains an enumeration of the rays of the 
     fan and for each number the ray, the basis of the bundle over this ray and the filtration matrix.",
     
     EXAMPLE {
	  " tvb = tangentBundleK hypercubefan 2",
	  "details tvb"
	  }
     
     }

document {
     Key => {makeVB},
     Headline => " generates the trivial bundle of rank 'k' for a given Fan in Kaneyama's description",
     Usage => " tvb = makeVB(k,F) \ntvb = makeVB(k,F,dL,tL)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"},
	  "dL" => List,
	  "tL" => List
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed Fan ",TT "F"," the function ",TT "makeVB"," generates the trivial toric vector bundle of rank ",TT "k",". The degree 
     vectors of this bundle are all zero vectors and the transition matrices are all the identity. If the function is applied to a Fan ",TT "F",", 
     a list ",TT "dL"," of degree matrices and a list ",TT "tL"," of transition matrices, then the resulting vector bundle will have these degrees 
     and tranisition matrices. The number of matrices in ",TT "dL"," must match the number of maximal cones of the Fan and  they must be ",TT "n"," 
     times ",TT "k"," matrices over ",TO ZZ,". They will be assigned to the cones in the order they appear in ",TT "genCones(F)",". The number of 
     matrices in ",TT "tL"," must match the number of pairs of maximal cones that intersect in a common codimension 1 face and must all be in 
     GL(",TT "k",",",TO QQ,"). They will be assigned to the pairs (i,j) in lexicographic order. note that the degrees and transition matrices that are 
     given to the function must not satisfy the regularity or the cocycle condition. These can by checked by using ",TO regCheck," 
     and ",TO cocycleCheck,".",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " details tvb"
	  }
     
     } 

document {
     Key => {makeVBK},
     Headline => " generates the trivial bundle of rank 'k' for a given fan in Klyachko's description",
     Usage => " tvb = makeVBK(k,F) \ntvb = makeVBK(k,F,bL,fL)",
     Inputs => {
	  "k" => ZZ => {" strictly positive"},
	  "F" => {"an object of class Fan"},
	  "bL" => List,
	  "fL" => List
	  },
     Outputs => {
	  "tvb" => ToricVectorBundleK
	  },
     
     PARA{}, "For a given pure, full dimensional and pointed Fan ",TT "F"," the function ",TT "makeVB"," generates the trivial toric vector bundle 
     of rank ",TT "k",". The basis assigned to every ray is the standard basis of ",TO QQ,"^k and the filtration is given by ",TT "0"," for all 
     ",TT "i<0"," and ",TO QQ,"^k for ",TT "i>=0",". If the function is applied to a Fan ",TT "F",", a list ",TT "bL"," of basis matrices and a 
     list ",TT "fL"," of filtration matrices, then the resulting vector bundle will have these basis and filtration matrices. The number of matrices 
     in ",TT "bL"," must match the number of rays of the Fan and  they must be in GL(",TT "k",",",TO QQ,"). They will be assigned to the rays in the 
     order they appear in ",TT "rays(F)",". The number of  matrices in ",TT "fL"," must match the number of rays also, and must be ",TT "1"," times 
     ",TT "k"," matrices over ",TO ZZ,". The assignment order is the same as for the basis matrices. Note that the basis and filtration matrices that are 
     given to the function must not satisfy the compatability condition. These can by checked by using ",TO regCheck,".",
     
     EXAMPLE {
	  " tvb = makeVBK(2,hypercubefan 2)",
	  " details tvb"
	  }
     
     } 

document {
     Key => {regCheck, (regCheck,ToricVectorBundle), (regCheck,ToricVectorBundleK)},
     Headline => " checking the regularity condition for a toric vector bundle",
     Usage => " b = regCheck(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle
	  },
     Outputs => {
	  "b" => Boolean
	  },
     
     PARA{}, "For a toric vector bundle in Kaneyama's description, the regularity condition means that for every pair of maximal cones 
     intersecting in a common codimension 1 face, the two sets of degrees and the transition matrix fulfill the regularity condition. I.e. 
     for every i and j we have that the (i,j) entry of the matrix is 0 or the difference of the i-th degree vector of one cone and the j-th 
     degree vector of the other con is in the dual cone of the intersection. Note that this is only neccessary for toric vector bundles 
     generated 'by hand' using ",TO addBaseChange," and ",TO addDegrees," since bundles generated for example by ",TO tangentBundle,", satisfy 
     the condition autmatically.",
     
     EXAMPLE {
	  " tvb = tangentBundle hypercubefan 2",
	  " regCheck tvb"
	  },
     
     PARA{}, "For a toric vector bundle in Klyachko's description, the regularity condition means that for every maximal cone there is 
     a decomposition into torus eigenspaces such that the filtration for each ray is induced by this decomposition (See: " , EM "Computing 
     Cech Cohomology of Vector Bundles on Toric Varieties", ", R. Birkner, N. O. Ilten, and L. Petersen, in preparation). Note that this 
     is only neccessary for toric vector bundles generated 'by hand' using ",TO addBase," and ",TO addFiltration,", since bundles generated 
     for example by ",TO tangentBundleK," satisfy the condition autmatically.",
     
     EXAMPLE {
	  " tvb = tangentBundleK hypercubefan 2",
	  " regCheck tvb"
	  }
     
     }

document {
     Key => {charts, (charts,ToricVectorBundle), (charts,ToricVectorBundleK)},
     Headline => " returning the number of maximal affine charts",
     Usage => " n = charts(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "n" => ZZ
	  },
     
     PARA{}, TT "charts"," returns the number of maximal cones in the fan, i.e. the number of affine charts."
     
     }

document {
     Key => {cohom, (cohom,ZZ,ToricVectorBundle,Matrix)},
     Headline => " computing the degree u part of the i-th cohomology of a toric vector bundle",
     Usage => "n = cohom(i,tvb,u)",
     Inputs => {
	  "i" => ZZ,
	  "tvb" => ToricVectorBundle,
	  "u" => Matrix => {"over ",TO ZZ,", giving a point in the lattice of the fan"}
	  },
     Outputs => {
	  "n" => ZZ
	  },
     
     PARA{}, "Computes the degree ",TT "u"," part of the ",TT "i","-th cohomology of the toric vector bundle ",TT "tvb",". The 
     degree ",TT "u"," must be a one column matrix giving a point in the lattice of the Fan over which ",TT "tvb"," is defined. 
     This means the number of rows of ",TT "u"," must equal the ambient dimension of the Fan.",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " cohom(0,tvb,matrix{{0},{0}})"
	  }
     
     }

document {
     Key => {cotangentBundle},
     Headline => " generates the cotangent bundle in Kaneyama's description",
     Usage => " tvb = cotangentBundle F",
     Inputs => {
	  "F" => {"an object of class Fan"}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle
	  },
     
     PARA{}, "If the Fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the cotangent bundle of the 
     toric variety given by ",TT "F",".",
     
     EXAMPLE {
	  " F = hypercubefan 2",
	  " tvb = cotangentBundle F",
	  " details tvb"
	  }
     
     }

document {
     Key => {deltaE, (deltaE,ToricVectorBundle)},
     Headline => " computes the polytope of possible degrees that give non zero cohomology",
     Usage => " P = deltaE(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle
	  },
     Outputs => {
	  "P" => {"an object of class Polyhedron"}
	  },
     
     PARA{}, "For a toric vector bundle there is a finite set of degrees 'u' such that the degree 'u' part of the cohomology of the 
     vector bundle is not zero. This function computes the polytope ",TT "deltaE"," such that these degrees are contained in the 
     polytope. See ", EM "Computing Cech Cohomology of Vector Bundles on Toric Varieties", ", R. Birkner, N. O. Ilten, and L. 
     Petersen, in preparation.",
     
     EXAMPLE {
	  " tvb = makeVB(2,hypercubefan 2)",
	  " P = deltaE tvb",
	  " P#\"vertices\""
	  }
     
     }

document {
     Key => {dsum, (dsum,ToricVectorBundle,ToricVectorBundle), (dsum,ToricVectorBundleK,ToricVectorBundleK)},
     Headline => " computes the direct sum of two vector bundles",
     Usage => " tvb = dsum(tvb1,tvb2)",
     Inputs => {
	  "tvb1" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  "tvb2" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     
     PARA{}, TT "dsum"," computes the direct sum of two toric vector bundles if they are defined over the same fan. Note that the two 
     vector bundles must either both be in Kaneyama's description or both in Klyachko's description. As soon 
     as the conversion from one description into the other has been implemented, this restriction will be removed.",
     
     PARA{}, "You can also use ",TO (symbol ++,ToricVectorBundle,ToricVectorBundle),".",
     
     EXAMPLE {
	  " tvb1 = makeVB(2,hirzebruchfan 3)",
	  " tvb2 = tangentBundle hirzebruchfan 3",
	  " tvb = dsum(tvb1,tvb2)",
	  " details tvb"
	  }
     
     }

document {
     Key => {dualVB, (dualVB,ToricVectorBundle)},
     Headline => " computes the dual bundle of a toric vector bundle",
     Usage => " tvbd = dualVB(tvb)",
     Inputs => {
	  "tvb" => ToricVectorBundle
	  },
     Outputs => {
	  "tvbd" => ToricVectorBundle
	  },
     
     PARA{}, TT "dualVB"," computes the dual vector bundle of a toric vector bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundle hypercubefan 2",
	  " tvbd = dualVB tvb",
	  " tvbd == cotangentBundle hypercubefan 2"
	  }
     
     }

document {
     Key => {extPower, (extPower,ZZ,ToricVectorBundle), (extPower,ZZ,ToricVectorBundleK)},
     Headline => " computes the l-th exterior power of atoric vector bundle",
     Usage => " tvbe = extPower(l,tvb)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvbe" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     
     PARA{}, TT "extPower"," computes the ",TT "l","-th exterior power of a toric vector bundle in each description. The resulting 
     bundle will be given in the same description as the original bundle. ",TT "l"," must be strictly positive and at most the rank 
     of the bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundle hirzebruchfan 3",
	  " tvbe = extPower(2,tvb)",
	  " details tvbe"
	  }
     
     }

document {
     Key => {symmProd, (symmProd,ZZ,ToricVectorBundle), (symmProd,ZZ,ToricVectorBundleK)},
     Headline => " computes the l-th symmetric product of a toric vector bundle",
     Usage => " tvbs = symmProd(l,tvb)",
     Inputs => {
	  "l" => ZZ => {" strictly positive"},
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvbs" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     
     PARA{}, TT "symmProd"," computes the ",TT "l","-th symmetric product of a toric vector bundle in each description. The resulting 
     bundle will be given in the same description as the original bundle. ",TT "l"," must be strictly positive.",
     
     EXAMPLE {
	  " tvb = tangentBundleK hirzebruchfan 3",
	  " tvbs = symmProd(2,tvb)",
	  " details tvbs"
	  }
     
     }

document {
     Key => {tangentBundle},
     Headline => " generates the tangent bundle in Kaneyama's description",
     Usage => " tvb = tangentBundle F",
     Inputs => {
	  "F" => {"an object of class Fan"}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle
	  },
     
     PARA{}, "If the Fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the tangent bundle of the 
     toric variety given by ",TT "F",".",
     
     EXAMPLE {
	  " F = hypercubefan 2",
	  " tvb = tangentBundle F",
	  " details tvb"
	  }
     
     }

document {
     Key => {tangentBundleK},
     Headline => " generates the tangent bundle in Klyachko's description",
     Usage => " tvb = tangentBundleK F",
     Inputs => {
	  "F" => {"an object of class Fan"}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundleK
	  },
     
     PARA{}, "If the Fan ",TT "F"," is pure, of full dimension and smooth, then the function generates the tangent bundle of the 
     toric variety given by ",TT "F",".",
     
     EXAMPLE {
	  " F = hypercubefan 2",
	  " tvb = tangentBundleK F",
	  " details tvb"
	  }
     
     }

document {
     Key => {tproduct, (tproduct,ToricVectorBundle,ToricVectorBundle), (tproduct,ToricVectorBundleK,ToricVectorBundleK)},
     Headline => " computes the tensor product of two toric vector bundles",
     Usage => " tvb = tproduct(tvb1,tvb2)",
     Inputs => {
	  "tvb1" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  "tvb2" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}, 
	  },
     
     PARA{}, "If ",TT "tvb1"," and ",TT "tvb2"," are defined over the same fan, then ",TT "tproduct"," computes the tensor product of the 
     two vector bundles.",
     
     EXAMPLE {
	  " tvb1 = makeVB(2,hirzebruchfan 3)",
	  " tvb2 = tangentBundle hirzebruchfan 3",
	  " tvb = tproduct(tvb1,tvb2)",
	  " details tvb"
	  }	  
     
     }

document {
     Key => {weilToCartier},
     Headline => " generates the bundle given by a Cartier divisor",
     Usage => " tvb = weilToCartier(L,F)",
     Inputs => {
	  "L" => List,
	  "F" => {"an objct of class Fan"}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle
	  },
     
     PARA{}, TT "F"," must be a pure and full dimensional Fan and ",TT "L"," must a list of weights, exactly one for each ray of the 
     fan. If the Weil divisor defined by these weights defines in fact a Cartier divisor, then ",TT "weilToCartier"," computes the 
     toric vector bundle associated to the Cartier divisor.",
     
     EXAMPLE {
	  " F = hirzebruchfan 3",
	  " tvb =weilToCartier({1,-3,4,-2},F)",
	  " details tvb"
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle)},
     Headline => " computes the i-th cohomology of a toric vector bundle",
     Usage => " c = HH^i tvb ",
     Inputs => {
	  "i" => ZZ,
	  "tvb" => ToricVectorBundle
	  },
     Outputs => {
	  "c" => List
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology of the toric vector bundle ",TT "tvb",". The output is a list, where each entry is a pair 
     consisting of a degree vector and the cohomology of that degree. ",TT "i"," must be between 0 and the rank of the vector bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundle hirzebruchfan 3",
	  " HH^0 tvb",
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle,Matrix)},
     Headline => " computes the i-th cohomology of a toric vector bundle in a given degree",
     Usage => " c = HH_i^tvb u ",
     Inputs => {
	  "i" => ZZ,
	  "tvb" => ToricVectorBundle,
	  "u" => Matrix => {"over ",TO ZZ," with just one column, giving a weight in the lattice"}
	  },
     Outputs => {
	  "c" => List
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology of the toric vector bundle ",TT "tvb"," of degree ",TT "u"," where ",TT "u"," must be a 
     one column matrix giving a point in the lattice of the Fan over which ",TT "tvb"," is defined and ",TT "i"," must be between 0 and 
     the rank of the vector bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundle hirzebruchfan 3",
	  " HH_0^tvb matrix{{1},{0}}"
	  }
     
     }

document {
     Key => {(cohomology,ZZ,ToricVectorBundle,List)},
     Headline => " computes the i-th cohomology of a toric vector bundle for a given list of degrees",
     Usage => " c = HH_i^tvb L",
     Inputs => {
	  "i" => ZZ,
	  "tvb" => ToricVectorBundle,
	  "L" => List => {" containing weights of the form one column matrix over ",TO ZZ}
	  },
     Outputs => {
	  "c" => List
	  },
     
     PARA{}, "Computes the ",TT "i","-th cohomology of the toric vector bundle ",TT "tvb"," for a given list of degrees. ",TT "i"," must 
     be between 0 and the rank of the vector bundle. The entries of the list ",TT "L"," must be one column matrices each defining a point 
     in the lattice of the Fan over which ",TT "tvb"," is defined",
     
     EXAMPLE {
	  " tvb = tangentBundle hirzebruchfan 3",
	  " HH_0^tvb {matrix{{1},{0}},matrix{{-1},{0}}}"
	  }
     
     }

document {
     Key => {(symbol **,ToricVectorBundle,ToricVectorBundle), (symbol **,ToricVectorBundleK,ToricVectorBundleK)},
     Headline => " computes the tensor product of two toric vector bundles",
     Usage => " tvb = tvb1 ** tvb2",
     Inputs => {
	  "tvb1" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  "tvb2" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     
     PARA{}, "If ",TT "tvb1"," and ",TT "tvb2"," are defined over the same fan, then ",TT "tproduct"," computes the tensor product of the 
     two vector bundles. The bundles must both be given in the same description.",
     
     EXAMPLE {
	  " tvb1 = makeVB(2,hirzebruchfan 3)",
	  " tvb2 = tangentBundle hirzebruchfan 3",
	  " tvb = tvb1 ** tvb2",
	  " details tvb"
	  },
     
     PARA{}, " See also ",TO tproduct,"."
     
     }

document {
     Key => {(symbol ++,ToricVectorBundle,ToricVectorBundle), (symbol ++,ToricVectorBundleK,ToricVectorBundleK)},
     Headline => " computes the direct sum of two toric vector bundles",
     Usage => " tvb = tvb1 ++ tvb2",
     Inputs => {
	  "tvb1" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  "tvb2" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvb" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     
     PARA{}, "If ",TT "tvb1"," and ",TT "tvb2"," are defined over the same fan, then ",TT "dsum"," computes the direct sum of the 
     two vector bundles. The bundles must both be given in the same description.",
     
     EXAMPLE {
	  " tvb1 = makeVB(2,hirzebruchfan 3)",
	  " tvb2 = tangentBundle hirzebruchfan 3",
	  " tvb = tvb1 ++ tvb2",
	  " details tvb"
	  },
     
     PARA{}, " See also ",TO tproduct,"."
     
     }

document {
     Key => {(symbol ==,ToricVectorBundle,ToricVectorBundle), (symbol ==,ToricVectorBundleK,ToricVectorBundleK)},
     Headline => " checks for equality",
     Usage => " b = tvb1 == tvb2",
     Inputs => {
	  "tvb1" => ToricVectorBundle => {" or ",TO ToricVectorBundleK},
	  "tvb2" => ToricVectorBundle => {" or ",TO ToricVectorBundleK}
	  },
     Outputs => {
	  "tvb" => Boolean
	  },
     
     PARA{}, "Checks if two toric vector bundles are the same. This only works if they are given in the same 
     description.",
     
     EXAMPLE {
	  " tvb1 = makeVB(2,hirzebruchfan 3)",
	  " tvb2 = tangentBundle hirzebruchfan 3",
	  " tvb1 == tvb2"
	  }
     
     }

document {
     Key => (net,ToricVectorBundle),
     Headline => "displays characteristics of a toric vector bundle",
     Usage => " net tvb",
     Inputs => {
	  "tvb" => ToricVectorBundle
	  },
     
     PARA{}, "Displays an overview of the properties of the toric vector bundle, 
     the dimension of the variety, the number of affine charts, and the rank of the 
     vector bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundle hirzebruchfan 3;",
	  " net tvb"
	  }
     
     }

document {
     Key => (net,ToricVectorBundleK),
     Headline => "displays characteristics of a toric vector bundle in Klyachko's description",
     Usage => " net tvb",
     Inputs => {
	  "tvb" => ToricVectorBundleK
	  },
     
     PARA{}, "Displays an overview of the properties of the toric vector bundle, 
     the dimension of the variety, the number of affine charts, the number of rays of the fan, 
     and the rank of the vector bundle.",
     
     EXAMPLE {
	  " tvb = tangentBundleK hirzebruchfan 3;",
	  " net tvb"
	  }
     
     }

document {
     Key => {hirzebruchfan,(hirzebruchfan,ZZ)},
     Headline => "Internal function, not for public use",
     
     }

document {
     Key => {hypercubefan,(hypercubefan,ZZ)},
     Headline => "Internal function, not for public use",
     
     }



TEST ///


///

end
     
     