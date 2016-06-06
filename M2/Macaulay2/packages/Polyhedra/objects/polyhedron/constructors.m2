-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron

compute#Polyhedron = new MutableHashTable

-- PURPOSE : Building the polyhedron 'P'
--   INPUT : '(hyperA,verticesA)',  a pair of two matrices each describing the homogenization of P
--                                 directly ('verticesA') and in the dual description ('hyperA')
--  OUTPUT : The polyhedron 'P'
polyhedronBuilder = (hyperA,verticesA) -> (
        -- Checking if the polyhedron is empty
	test := matrix join({{1}},toList((numgens target verticesA#0)-1:{0_QQ}));
	if  (((transpose(verticesA#0))*test == 0) and  ((transpose(verticesA#1))*test == 0)) then (
	     zeromap := map(target verticesA#0,ZZ^0,0);
	     verticesA = (zeromap,zeromap);
	     hyperA = fourierMotzkin verticesA);
	-- Sorting into vertices and rays
	VR := verticesA#0;
        C := map(target VR,ZZ^0,0);
	B := promote(C,QQ);
	VRpart := partition(n -> VR_n_0 != 0,toList(0..(numColumns VR)-1));
	if VRpart#?true then (
	     B = promote(VR_(VRpart#true),QQ);
	     B = matrix transpose apply(numColumns B, j -> flatten entries((1/B_j_0)*B_{j})));
	if VRpart#?false then C = VR_(VRpart#false);
	--B = B_{1..(numgens source B)-1};
	--C = C_{1..(numgens source C)-1};
	-- Elimination of the trivial half-space
	test = matrix join({{-1}},toList((numgens target (hyperA#0))-1:{0}));
	H := transpose (hyperA#0)_(toList select(0..(numColumns hyperA#0)-1, i -> test =!= (hyperA#0)_{i}));
	-- Determine the lineality space
	LS := verticesA#1;
	LS = LS^{1..(numgens target LS)-1};
	-- Determine the defining hyperplanes
	hyperplanesTmp := transpose(hyperA#1);
	hyperplanesTmp = (hyperplanesTmp_{1..(numgens source hyperplanesTmp)-1},-hyperplanesTmp_{0});
	-- Defining the Polyhedron
	result := new Polyhedron from {
	     "ambient dimension" => (numgens target B)-1,
	     "dimension" =>  ((numgens target B)-1)-(rank(hyperA#1)),
	     "dimension of lineality space" => numgens source LS,
	     "linealitySpace" => LS,
	     "number of vertices" => numgens source B,
	     "number of rays" => numgens source C,
	     "vertices" => B^{1..(numgens target B)-1},
--	     "rays" => C^{1..(numgens target C)-1},
	     "number of facets" => numgens target H,
	     "halfspaces" => (H_{1..(numgens source H)-1},-H_{0}),
	     "hyperplanes" => hyperplanesTmp,
	     "homogenizedVertices" => verticesA,
	     "homogenizedHalfspaces" => hyperA,
	     symbol cache => new CacheTable};
   result.cache.computedRays = C^{1..(numgens target C)-1};
   result
)
     
--   INPUT : 'P'  a Polyhedron
posHull Polyhedron := P -> (
     Mrays := makePrimitiveMatrix vertices(P) | rays(P);
     Mlinspace := linSpace(P);
     posHull(Mrays,Mlinspace))

-- PURPOSE : Computing the Convex Hull of a given set of points and rays
convexHull = method(TypicalValue => Polyhedron)

--   INPUT : 'Mvert'  a Matrix containing the generating points as column vectors
--		 'Mrays'  a Matrix containing the generating rays as column vectors
--  OUTPUT : 'P'  a Polyhedron
-- COMMENT : The description by vertices and rays is stored in P as well as the 
--           description by defining half-spaces and hyperplanes.
convexHull(Matrix, Matrix, Matrix) := (Mvert, Mrays, Mlineality) -> (
   if numgens target Mvert =!= numgens target Mrays then error ("points and rays must lie in the same space");
   result := new Polyhedron from {
      ambientDimension => numRows Mvert,
      cache => new CacheTable
   };
   setProperty(result, points, Mvert);
   setProperty(result, inputRays, Mrays);
   setProperty(result, inputLinealityGenerators, Mlineality);
   result
)

convexHull(Matrix,Matrix) := (Mvert,Mrays) -> (
   r := ring Mvert;
	Mlineality := map(target Mvert,r^0,0);
   convexHull(Mvert, Mrays, Mlineality)
	-- checking for input errors
-- 	Mvert = chkZZQQ(Mvert,"points");
-- 	Mrays = chkZZQQ(Mrays,"rays");
-- 	if numRows Mvert == 0 then Mvert = matrix{{0}};
-- 	if numColumns Mvert == 0 then Mvert = map(target Mvert,QQ^1,0);
-- 	if numRows Mrays == 0 then Mrays = matrix{{0}};
-- 	if numColumns Mrays == 0 then Mrays = map(target Mrays,QQ^1,0);
-- 	-- homogenization of M
-- 	Mvert = map(QQ^1,source Mvert,(i,j)->1) || Mvert;
-- 	Mrays = map(QQ^1,source Mrays,0) || Mrays;
-- 	M := Mvert | Mrays;
-- 	-- Computing generators of the cone M and its dual cone
-- 	hyperA := fourierMotzkin M;
-- --	verticesA := fourierMotzkin hyperA;
--      	local verticesA;
-- 	(verticesA,hyperA) = fMReplacement(M,hyperA#0,hyperA#1);
-- 	polyhedronBuilder(hyperA,verticesA)
)


--   INPUT : 'M'  a Matrix containing the generating points as column vectors
convexHull Matrix := Mvert -> (
   r := ring Mvert;
	-- Generating the zero ray R
	Mrays := map(target Mvert,r^0,0);
	convexHull(Mvert, Mrays)
)


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
	-- Combining the vertices/rays and the lineality spaces in one matrix each
	M := (P1#"homogenizedVertices")#0 | (P2#"homogenizedVertices")#0;
	LS := (P1#"homogenizedVertices")#1 | (P2#"homogenizedVertices")#1;
	hyperA := fourierMotzkin(M,LS);
--	verticesA := fourierMotzkin hyperA;
        local verticesA;
	(verticesA,hyperA) = fMReplacement(M,hyperA#0,hyperA#1);
	polyhedronBuilder(hyperA,verticesA))
   
--   INPUT : 'L',   a list of Cones, Polyhedra, vertices given by M, 
--     	    	    and (vertices,rays) given by '(V,R)'
convexHull List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that give valid vertices and rays
     isValidPair := S -> #S == 2 and if S#1 == 0 then instance(S#0,Matrix) else instance(S#1,Matrix) and numRows S#0 == numRows S#1;
     -- Checking for input errors  
     if L == {} then error("List of convex objects must not be empty");   
     P := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local V;
     local R;
     if (not instance(P,Cone)) and (not instance(P,Polyhedron)) and (not instance(P,Sequence)) and (not instance(P,Matrix)) then 
	  error ("The input must be cones, polyhedra, vertices, or (vertices,rays).");
     -- Adding the vertices and rays to 'V,R', depending on the type of 'P'
     if instance(P,Cone) then (
	  n = ambDim(P);
	  V = map(QQ^n,QQ^1,0);
	  R = rays P | linSpace P | -(linSpace P))
     else if instance(P,Polyhedron) then (
	  n = ambDim(P);
	  V = vertices P;
	  R = rays P | linSpace P | -(linSpace P))
     else if instance(P,Sequence) then (
	  -- Checking for input errors
	  if not isValidPair(P) then error ("Vertices and rays must be given as a sequence of two matrices with the same number of rows");
	  V = chkZZQQ(P#0,"vertices");
	  n = numRows V;
	  if P#1 == 0 then R = map(ZZ^n,ZZ^1,0)
	  else R = chkQQZZ(P#1,"rays"))
     else (
	  V = chkZZQQ(P,"vertices");
	  n = numRows P;
	  R = map(ZZ^n,ZZ^1,0));
     --  Adding the vertices and rays to 'V,R', for each remaining element in 'L', depending on the type of 'P'
     L = apply(drop(L,1), C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone)) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,Matrix)) then error("The input must be cones, polyhedra, vertices, or (vertices,rays).");
	       if instance(C1,Cone) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    ({},rays C1 | linSpace C1 | -(linSpace C1)))
	       else if instance(C1,Polyhedron) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (vertices C1,rays C1 | linSpace C1 | -(linSpace C1)))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if not isValidPair(C1) then error("(Vertices,rays) must be given as a sequence of two matrices with the same number of rows");
		    if numRows C1#0 != n then error("(Vertices,rays) must be of the correct dimension.");
		    if C1#1 != 0 then (chkZZQQ(C1#0,"vertices"),chkQQZZ(C1#1,"rays"))
		    else (chkZZQQ(C1#0,"vertices"),{}))
	       else (
		    -- Checking for input errors
		    if numRows C1 != n then error("Vertices must be of the correct dimension.");
		    (chkZZQQ(C1,"vertices"),{}))));
     LV := flatten apply(L, l -> l#0);
     if LV != {} then V = V | matrix {LV};
     L = flatten apply(L, l -> l#1);
     if L != {} then R = R | matrix {L};
     if R == 0 then convexHull V else convexHull(V,R))
