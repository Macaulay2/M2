--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : April 2008, December 2008, March 2009, Juli 2009
---------------------------------------------------------------------------
newPackage("Polyhedra",
    Headline => "A package for computations with convex polyhedra",
    Version => "1.0.1",
    Date => "July 14, 2009",
    Authors => {
         {Name => "René Birkner",
	  HomePage => "http://page.mi.fu-berlin.de/rbirkner/index.htm",
	  Email => "rbirkner@mi.fu-berlin.de"}},
    DebuggingMode => true,
    AuxiliaryFiles=>true,
    Configuration => {}
    )

export {Polyhedron, Cone, Fan, polyhedronBuilder, coneBuilder, fanBuilder, convexHull, posHull, intersection, makeFan, addCone,
        equalLists, symmDiff, union, 
        ambDim, cones, genCones, halfspaces, hyperplanes, linSpace, rays, vertices,
        areCompatible, commonFace, contains, equals, isCompact, isComplete, isEmpty, isFace, isPointed, isProjective, 
	isPure, isSmooth,
	faces, fVector, hilbertBasis, incompCones, inInterior, interiorPoint, interiorVector, latticePoints, 
	minkSummandCone, skeleton, smallestFace, smoothSubfan, tailCone, vertexEdgeMatrix, vertexFacetMatrix,
	affineHull, affineImage, affinePreimage, bipyramid, ccRefinement, coneToPolyhedron, directProduct,  dualCone, imageFan,
	minkowskiSum, normalFan, polar, pyramid,
	crossPolytope, cyclicPolytope, emptyPolyhedron, hirzebruch, hypercube, newtonPolytope, posOrthant, statePolytope, stdSimplex,
	saveSession}

needsPackage "FourierMotzkin"


-- Defining the new type Polyhedron
Polyhedron = new Type of MutableHashTable
Polyhedron.synonym = "convex polyhedron"
Polyhedron.GlobalAssignHook = globalAssignFunction
Polyhedron.GlobalReleaseHook = globalReleaseFunction

-- Defining the new type Cone
Cone = new Type of MutableHashTable
Cone.synonym = "convex rational cone"
Cone.GlobalAssignHook = globalAssignFunction
Cone.GlobalReleaseHook = globalReleaseFunction

-- Defining the new type Fan
Fan = new Type of MutableHashTable
Fan.GlobalAssignHook = globalAssignFunction
Fan.GlobalReleaseHook = globalReleaseFunction


-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net Polyhedron := P -> ( info:= hashTable{"ambient dimension" => P#"ambientDimension",
							"dimension of polyhedron" => P#"polyhedronDimension",
							"dimension of lineality space" => P#"linealitySpaceDimension",
							"number of facets" => P#"numFacets", 
							"number of vertices" => P#"numVertices",
							"number of rays" => P#"numRays"};
				horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply(pairs info,(k,v) -> (net k, " => ", net v))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( info:= hashTable{"ambient dimension" => C#"ambientDimension",
							"dimension of the cone" => C#"coneDimension",
							"dimension of lineality space" => C#"linealitySpaceDimension",
							"number of facets" => C#"numFacets", 
							"number of rays" => C#"numRays"};
				horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply(pairs info,(k,v) -> (net k, " => ", net v))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


-- Modifying the standard output for a Cone to give an overview of its characteristica
net Fan := F -> ( info:= hashTable{"ambient dimension" => F#"ambientDimension",
							"top dimension of the cones" => F#"topDimension",
							"number of generating cones" => F#"numGeneratingCones",
							"number of rays" => F#"numRays"};
				horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply(pairs info,(k,v) -> (net k, " => ", net v))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))



-- PURPOSE : Building the polyhedron 'P'
--   INPUT : '(hyperA,verticesA)',  a pair of two matrices each describing the homogenization of P
--                                 directly ('verticesA') and in the dual description ('hyperA')
--  OUTPUT : The polyhedron 'P'
polyhedronBuilder = method()
polyhedronBuilder (Sequence,Sequence) := (hyperA,verticesA) -> (
        -- Checking if the polyhedron is empty
	test := matrix join({{1}},toList((numgens target verticesA#0)-1:{0_QQ}));
	if  (((transpose(verticesA#0))*test == 0) and  ((transpose(verticesA#1))*test == 0)) then (
	     zeromap := map(QQ^(numgens target verticesA#0),QQ^0,0);
	     verticesA = (zeromap,zeromap);
	     hyperA = fourierMotzkin verticesA);
	-- Sorting into vertices and rays
	VR := verticesA#0;
	B := matrix toList(numgens target VR:{0_QQ});
	C := B;
	scan(numgens source VR, n -> (if (VR_n_0=!=0_QQ) then B=B|((1/VR_n_0)*VR_{n}) else C=C|(VR_{n})));
	B = B_{1..(numgens source B)-1};
	C = C_{1..(numgens source C)-1};
	-- Elimination of the trivial halfspace
	H := map(QQ^0, QQ^(numgens target (hyperA#0)),0);
	test = matrix join({{-1}},toList((numgens target (hyperA#0))-1:{0_QQ}));
	scan(numgens source (hyperA#0), i -> ( if (test=!=(hyperA#0)_{i}) then H=H || transpose((hyperA#0)_{i})));
	-- Determine the lineality space
	LS := verticesA#1;
	LS = LS^{1..(numgens target LS)-1};
	-- Determine the defining hyperplanes
	HP := transpose(hyperA#1);
	HP = (HP_{1..(numgens source HP)-1},(-1)*HP_{0});
	-- Defining P
	P := new Polyhedron;
	P#"ambientDimension" = (numgens target B)-1;
	P#"polyhedronDimension" =  ((numgens target B)-1)-(rank(hyperA#1));
	P#"linealitySpaceDimension" = numgens source LS;
	P#"linealitySpace" = LS;
	P#"numVertices" = numgens source B;
	P#"numRays" = numgens source C;
	P#"vertices" = B^{1..(numgens target B)-1};
	P#"rays" = C^{1..(numgens target C)-1};
	P#"numFacets" = numgens target H;
	P#"halfspaces" = (H_{1..(numgens source H)-1},(-1)*H_{0});
	P#"hyperplanes" = HP;
	P#"homogenizedVertices" = verticesA;
	P#"homogenizedHalfspaces" = hyperA;
	P)
   
   
-- PURPOSE : Building the Cone 'C'
--   INPUT : '(genrays,dualgens)',  a pair of two matrices each describing the cone C
--                                	directly  as generating rays ('genrays') and in the 
--						dual description as intersection of halfspaces through 
--						the origin ('dualgens')
--  OUTPUT : The Cone 'C'
coneBuilder = method()
coneBuilder (Sequence,Sequence) := (genrays,dualgens) -> (
      -- Sorting into rays, lineality space generators, supporting halfspaces, and hyperplanes
	RM := genrays#0;
	LS := genrays#1;
	HS := transpose((-1)*(dualgens#0));
	HP := transpose(dualgens#1);
	-- Defining C
	C := new Cone;
	C#"ambientDimension" = numgens target RM;
	C#"coneDimension" = (numgens target RM)-(rank HP);
	C#"linealitySpaceDimension" = numgens source LS;
	C#"linealitySpace" = LS;
	C#"numRays" = numgens source RM;
	C#"rays" = RM;
	C#"numFacets" = numgens target HS;
	C#"halfspaces" = HS;
	C#"hyperplanes" = HP;
	C#"genrays" = genrays;
	C#"dualgens" = dualgens;
	C)
   
   
   
fanBuilder = method()
fanBuilder (List,List,List,List) := (GC,rayList,Ffaces,compError) -> (
     F := new Fan;
     F#"generatingCones" = GC;
     F#"ambientDimension" = ambDim(GC#0);
     F#"topDimension" = dim(GC#0);
     F#"numGeneratingCones" = #GC;
     F#"rays" = rayList;
     F#"numRays" = #rayList;
     F#"isPure" = (dim(GC#0) == dim(last(GC)));
     F#"faces" = Ffaces;
     F#"isComplete" = (Ffaces == {});
     if (compError != {}) then (
	  F#"comperror" = compError);
     F);




-- PURPOSE : Computing the Convex Hull of a given set of points and rays
convexHull = method(TypicalValue => Polyhedron)

--   INPUT : 'Mvert'  a Matrix containing the generating points as column vectors
--		 'Mrays'  a Matrix containing the generating rays as column vectors
--  OUTPUT : 'P'  a Polyhedron
-- COMMENT : The description by vertices and rays is stored in P as well as the 
--           description by defining halfspaces and hyperplanes.
convexHull(Matrix,Matrix) := (Mvert,Mrays) -> (
	-- checking for input errors
     	if ((numgens target Mvert) =!= (numgens target Mrays)) then (
	     error ("points and rays must lie in the same space"));
	local Mv;
	local Mr;
	R := ring source Mvert;
	if (R === ZZ) then (
		Mv = substitute(Mvert, QQ))
	else if (R === QQ) then (
		Mv = Mvert)
	else error ("expected points over ZZ or QQ");
	R = ring source Mrays;
	if (R === ZZ) then (
		Mr = substitute(Mrays, QQ))
	else if (R === QQ) then (
		Mr = Mrays)
	else error ("expected rays over ZZ or QQ");
	if (numRows(Mv) == 0) then (
	     Mv = matrix{{0}});
	if (numColumns(Mv) == 0) then (
	     Mv = map(QQ^(numRows(Mv)),QQ^1,0));
	if (numRows(Mr) == 0) then (
	     Mr = matrix{{0}});
	if (numColumns(Mr) == 0) then (
	     Mr = map(QQ^(numRows(Mr)),QQ^1,0));
	-- homogenization of M
	Mv = (matrix {toList(numgens source Mv:1_QQ)})||Mv;
	Mr = (matrix {toList(numgens source Mr:0_QQ)})||Mr;
	M := Mv|Mr;
	-- Computing generators of the cone M and its dual cone
	hyperA := fourierMotzkin M;
	verticesA := fourierMotzkin hyperA;
	polyhedronBuilder(hyperA,verticesA))


--   INPUT : 'M'  a Matrix containing the generating points as column vectors
convexHull(Matrix) := M -> (
	-- Checking for input errors
	if (numRows(M) == 0) then (
	     M = matrix{{0}});
	if (numColumns(M) == 0) then (
	     M = map(QQ^(numRows(M)),QQ^1,0));
	-- Generating the zero ray R
	R := matrix toList(numgens target M:{0_QQ});
	convexHull(M,R))


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if ((P1#"ambientDimension") =!= (P2#"ambientDimension")) then (
	     error ("Polyhedra must lie in the same ambientspace"));
	-- Combining the vertices/rays and the lineality spaces in one matrix each
	M := (P1#"homogenizedVertices")#0 | (P2#"homogenizedVertices")#0;
	LS := (P1#"homogenizedVertices")#1 | (P2#"homogenizedVertices")#1;
	hyperA := fourierMotzkin(M,LS);
	verticesA := fourierMotzkin hyperA;
	polyhedronBuilder(hyperA,verticesA))
   
--   INPUT : 'L',   a list of Cones, Polyhedra, vertices given by M, 
--     	    	    and (vertices,rays) given by '(V,R)'
convexHull List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that give valid vertices and rays
     isValidPair := S -> (
	  test := (#S == 2);
	  if test then (
	       if (S#1 == 0) then ( test = (instance(S#0,Matrix)))
	       else ( test = (instance(S#1,Matrix));
		    if test then ( test = (numRows(S#0) == numRows(S#1)))));
	  test);
     -- Checking for input errors  
     if (L=={}) then (error ("List of convex objects must not be empty"));   
     P := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local V,R;
     if ((not instance(P,Cone)) and (not instance(P,Polyhedron)) and (not instance(P,Sequence)) and (not instance(P,Matrix))) then (
	  error ("The input must be cones, polyhedra, vertices, or (vertices,rays)."));
     -- Adding the vertices and rays to 'V,R', depending on the type of 'P'
     if instance(P,Cone) then (
	  n = P#"ambientDimension";
	  V = map(QQ^n,QQ^1,0);
	  R = (rays P)|(linSpace P)|(-(linSpace P)))
     else if instance(P,Polyhedron) then (
	  n = P#"ambientDimension";
	  V = vertices P;
	  R = (rays P)|(linSpace P)|(-(linSpace P)))
     else if instance(P,Sequence) then (
	  -- Checking for input errors
	  if (not isValidPair(P)) then (
	       error ("Vertices and rays must be given as a sequence of two matrices with the same number of rows"));
	  n = numRows(P#0);
	  if ((ring(P#0) =!= ZZ) and (ring(P#0) =!= QQ)) then (
	       error ("expected halfspaces over ZZ or QQ"));
	  V = substitute(P#0,QQ);
	  if (P#1 == 0) then (
	       R = map(QQ^n,QQ^1,0))
	  else (  
	       if ((ring(P#1) =!= ZZ) and (ring(P#1) =!= QQ)) then (
		    error ("expected halfspaces over ZZ or QQ"));
	       R = substitute(P#1,QQ)))
     else (
	  if ((ring(P) =!= ZZ) and (ring(P) =!= QQ)) then (
	       error ("expected vertices over ZZ or QQ"));
	  n = numRows P;
	  V = substitute(P,QQ);
	  R = map(QQ^n,QQ^1,0));
     L = drop(L,1);
     --  Adding the vertices and rays to 'V,R', for each remaining element in 'L', depending on the type of 'P'
     scan(L, C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,Matrix))) then (
		    error ("The input must be cones, polyhedra, vertices, or (vertices,rays)."));
	       if instance(C1,Cone) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    R = R | rays C1 | (linSpace C1) | (-(linSpace C1)))
	       else if instance(C1,Polyhedron) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    V = V | vertices C1;
		    R = R | rays C1 | (linSpace C1) | (-(linSpace C1)))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if (not isValidPair(C1)) then (
			 error ("(Vertices,rays) must be given as a sequence of two matrices with the same number of rows"));
		    if (numRows(C1#0) != n) then (
			 error ("(Vertices,rays) must be of the correct dimension."));
		    if ((ring(C1#0) =!= ZZ) and (ring(C1#0) =!= QQ)) then (
			 error ("expected (vertices,rays) over ZZ or QQ"));
		    V = V | substitute(C1#0,QQ);
		    if (C1#1 != 0) then (
			 if ((ring(C1#1) =!= ZZ) and (ring(C1#1) =!= QQ)) then (
			      error ("expected halfspaces over ZZ or QQ"));
			 R = R | substitute(C1#1,QQ)))
	       else (
		    -- Checking for input errors
		    if (numRows(C1) != n) then (
			 error ("Vertices must be of the correct dimension."));
		    if ((ring(C1) =!= ZZ) and (ring(C1) =!= QQ)) then (
			 error ("expected vertices over ZZ or QQ"));
		    V = V | substitute(C1,QQ))));
     if (R == 0) then (
	  P = convexHull V)
     else (
	  P = convexHull(V,R));
     P)



-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
posHull = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'LS'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining halfspaces and hyperplanes.
posHull(Matrix,Matrix) := (Mrays,LS) -> (
	-- checking for input errors
     	if ((numgens target Mrays) =!= (numgens target LS)) then (
	     error ("rays and linSpace generators must lie in the same space"));
	local Mr;
	local Mls;
	R := ring source Mrays;
	if (R === ZZ) then (
		Mr = substitute(Mrays, QQ))
	else if (R === QQ) then (
		Mr = Mrays)
	else error ("expected rays over ZZ or QQ");
	R = ring source LS;
	if (R === ZZ) then (
		Mls = substitute(LS, QQ))
	else if (R === QQ) then (
		Mls = LS)
	else error ("expected rays over ZZ or QQ");
	-- Computing generators of the cone and its dual cone
	dualgens := fourierMotzkin(Mr,Mls);
	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))


--   INPUT : 'M'  a Matrix containing the generating rays as column vectors
posHull(Matrix) := M -> (
	-- Generating the zero ray R
	R := matrix toList(numgens target M:{0_QQ});
	posHull(M,R))


--   INPUT : '(C1,C2)'  two cones
posHull(Cone,Cone) := (C1,C2) -> (
	-- Checking for input errors
	if ((C1#"ambientDimension") =!= (C2#"ambientDimension")) then (
	     error ("Cones must lie in the same ambientspace"));
	-- Combining the rays and the lineality spaces into one matrix each
	M := (C1#"rays") | (C2#"rays");
	LS := (C1#"linealitySpace") | (C2#"linealitySpace");
	dualgens := fourierMotzkin(M,LS);
	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))


--   INPUT : 'P'  a Polyhedron
posHull(Polyhedron) := (P) -> (
     Mrays := (P#"vertices") | (P#"rays");
     Mlinspace := P#"linealitySpace";
     posHull(Mrays,Mlinspace))


--   INPUT : 'L',   a list of Cones, Polyhedra, rays given by R, 
--     	    	    and (rays,linSpace) given by '(R,LS)'
posHull List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that give valid rays and linSpace
     isValidPair := S -> (
	  test := (#S == 2);
	  if test then (
	       if (S#1 == 0) then ( test = (instance(S#0,Matrix)))
	       else ( test = (instance(S#1,Matrix));
		    if test then ( test = (numRows(S#0) == numRows(S#1)))));
	  test);
     -- Checking for input errors  
     if (L == {}) then (error ("List of convex objects must not be empty"));   
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local R,LS;
     if ((not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,Matrix))) then (
	  error ("The input must be cones, polyhedra, rays, or (rays,linSpace)."));
     -- Adding the vertices and rays to 'R,LS', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = C#"ambientDimension";
	  R = rays C;
	  LS = linSpace C)
     else if instance(C,Polyhedron) then (
	  n = C#"ambientDimension";
	  R = (vertices C) | (rays C);
	  LS = linSpace C)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if (not isValidPair(C)) then (
	       error ("Rays and lineality space must be given as a sequence of two matrices with the same number of rows"));
	  n = numRows(C#0);
	  if ((ring(C#0) =!= ZZ) and (ring(C#0) =!= QQ)) then (
	       error ("expected rays over ZZ or QQ"));
	  R = substitute(C#0,QQ);
	  if (C#1 == 0) then (
	       LS = map(QQ^n,QQ^1,0))
	  else (  
	       if ((ring(C#1) =!= ZZ) and (ring(C#1) =!= QQ)) then (
		    error ("expected lineality space over ZZ or QQ"));
	       LS = substitute(C#1,QQ)))
     else (
	  if ((ring(C) =!= ZZ) and (ring(C) =!= QQ)) then (
	       error ("expected rays over ZZ or QQ"));
	  n = numRows C;
	  R = substitute(C,QQ);
	  LS = map(QQ^n,QQ^1,0));
     L = drop(L,1);
     --  Adding the rays and lineality spaces to 'R,LS' for each remaining element in 'L', depending on the type of 'C'
     scan(L, C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,Matrix))) then (
		    error ("The input must be cones, polyhedra, rays, or (rays,lineality space)"));
	       if instance(C1,Cone) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    R = R | (rays C1);
		    LS = LS | (linSpace C1))
	       else if instance(C1,Polyhedron) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    R = R | (vertices C1) | (rays C1);
		    LS = LS | (linSpace C1))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if (not isValidPair(C1)) then (
			 error ("(Rays,lineality space) must be given as a sequence of two matrices with the same number of rows"));
		    if (numRows(C1#0) != n) then (
			 error ("(Rays,lineality space) must be of the correct dimension."));
		    if ((ring(C1#0) =!= ZZ) and (ring(C1#0) =!= QQ)) then (
			 error ("expected rays over ZZ or QQ"));
		    R = R | substitute(C1#0,QQ);
		    if (C1#1 != 0) then (
			 if ((ring(C1#1) =!= ZZ) and (ring(C1#1) =!= QQ)) then (
			      error ("expected lineality space over ZZ or QQ"));
			 LS = LS | substitute(C1#1,QQ)))
	       else (
		    -- Checking for input errors
		    if (numRows(C1) != n) then (
			 error ("Rays must be of the correct dimension."));
		    if ((ring(C1) =!= ZZ) and (ring(C1) =!= QQ)) then (
			 error ("expected rays over ZZ or QQ"));
		    R = R | substitute(C1,QQ))));
     if (LS == 0) then (
	  C = posHull R)
     else (
	  C = posHull(R,LS));
     C)


-- PURPOSE : Computing a polyhedron as the intersection of affine halfspaces and hyperplanes
intersection = method()

--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix,Matrix,Matrix) := (M,v,N,w) -> (
	-- checking for input errors
	if ((numgens source M) =!= (numgens source N)) then (
		error ("equations of halfspaces and hyperplanes must have the same dimension"));
	if (((numgens target M) =!= (numgens target v)) or ((numgens source v) =!= 1)) then (
		error ("invalid condition vector for halfspaces"));
	if (((numgens target N) =!= (numgens target w)) or ((numgens source w) =!= 1)) then (
		error ("invalid condition vector for hyperplanes"));
	local HS;
	local HP;
	R := ring source M;
	if (R === ZZ) then (
		HS = substitute(M, QQ))
	else if (R === QQ) then (
		HS = M)
	else error ("expected halfspaces over ZZ or QQ");
	R = ring source v;
	if (R === ZZ) then (
		HS = ((-1)*(substitute(v, QQ)))|HS)
	else if (R === QQ) then (
		HS = ((-1)*v)|HS)
	else error ("expected condition vector for halfspaces over ZZ or QQ");
	R = ring source N;
	if (R === ZZ) then (
		HP = substitute(N, QQ))
	else if (R === QQ) then (
		HP = N)
	else error ("expected hyperplanes over ZZ or QQ");
	R = ring source w;
	if (R === ZZ) then (
		HP = ((-1)*(substitute(w, QQ)))|HP)
	else if (R === QQ) then (
		HP = ((-1)*w)|HP)
	else error ("expected condition vector for halfspaces over ZZ or QQ");
	-- Computing generators of the cone and its dual cone
	HS = (transpose HS)|(matrix join({{-1}},toList((numgens source HS)-1:{0_QQ})));
	HP = transpose HP;
	verticesA := fourierMotzkin(HS,HP);
	hyperA := fourierMotzkin verticesA;
	polyhedronBuilder(hyperA,verticesA))


--   INPUT : '(M,N)',  two matrices where either 'P' is the Cone {x | Mx<=0, Nx=0} if 'M' and 'N' have the same source space 
--     	    	       or, if 'N' is only a Column vector the Polyhedron {x | Mx<=v} 
--  OUTPUT : 'P', the Cone or Polyhedron
intersection(Matrix,Matrix) := (M,N) -> (
	-- Checking for input errors
	if (((((numgens source M) =!= (numgens source N)) and ((numgens source N) =!= 1)) or 
		(((numgens source N) == 1) and ((numgens target M) =!= (numgens target N)))) and 
		(N != (0*N))) then (
		error ("invalid condition vector for halfspaces"));
	local Ml;
	local Nl;
	local P;
	local genrays;
	local dualgens;
	R := ring source M;
	if (R === ZZ) then (
		Ml = substitute(M, QQ))
	else if (R === QQ) then (
		Ml = M)
	else error ("expected halfspaces over ZZ or QQ");
	R = ring source N;
	if (R === ZZ) then (
		Nl = substitute(N, QQ))
	else if (R === QQ) then (
		Nl = N)
	else error ("expected condition vector for halfspaces over ZZ or QQ");
	-- Decide whether 'M,N' gives the Cone C={p | M*p >= 0, N*p = 0}
	if ((numgens source Ml) == (numgens source Nl)) and ((numgens source Nl) != 1) then (
		Ml = (-1)*(transpose Ml);
		Nl = transpose Nl;
		genrays = fourierMotzkin(Ml,Nl);
		dualgens = fourierMotzkin genrays;
		P = coneBuilder(genrays, dualgens))
	-- or the Cone C={p | M*p >= N=0}
	else if ( Nl == (0*Nl)) then (
		Ml = (-1)*(transpose Ml);
		genrays = fourierMotzkin Ml;
		dualgens = fourierMotzkin genrays;
		P = coneBuilder(genrays,dualgens))
	-- or the Polyhedron P={p | M*p >= N != 0}
	else (	-- Computing generators of the Polyhedron and its dual cone
		Ml = ((-1)*Nl) | Ml;
		Ml = (transpose Ml)|(matrix join({{-1}},toList((numgens source Ml)-1:{0_QQ})));
		verticesA := fourierMotzkin Ml;
		hyperA := fourierMotzkin verticesA;
		P = polyhedronBuilder(hyperA,verticesA));
	P)
   



--   INPUT : '(P1,P2)',  two polyhedra 
--  OUTPUT : 'P', the polyhedron which is the intersection of both
intersection(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking if P1 and P2 lie in the same space
	if ((P1#"ambientDimension") =!= (P2#"ambientDimension")) then (
		error ("Polyhedra must lie in the same ambientspace"));
	-- Combining the Halfspaces and the Hyperplanes
	M := ((halfspaces P1)#0)||((halfspaces P2)#0);
	v := ((halfspaces P1)#1)||((halfspaces P2)#1);
	N := ((hyperplanes P1)#0)||((hyperplanes P2)#0);
	w := ((hyperplanes P1)#1)||((hyperplanes P2)#1);
	intersection(M,v,N,w))


--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
intersection(Matrix) := M -> (
	-- Checking for input errors
	local HS;
	R := ring source M;
	if (R === ZZ) then (
		HS = substitute(M, QQ))
	else if (R === QQ) then (
		HS = M)
	else error ("expected halfspaces over ZZ or QQ");
	-- Computing generators of the cone and its dual cone
	HS = (-1)*(transpose HS);
	genrays := fourierMotzkin HS;
	dualgens := fourierMotzkin genrays;
	coneBuilder(genrays,dualgens))



--   INPUT : '(C1,C2)',  two Cones
--  OUTPUT : 'C', the Cone which is the intersection of both
intersection(Cone,Cone) := (C1,C2) -> (
	-- Checking if C1 and C2 lie in the same space
	if ((C1#"ambientDimension") =!= (C2#"ambientDimension")) then (
		error ("Cones must lie in the same ambientspace"));
	M := (halfspaces C1)||(halfspaces C2);
	N := (hyperplanes C1)||(hyperplanes C2);
	intersection(M,N))



--   INPUT : 'L',   a list of Cones, Polyhedra, inequalities given by (M,v), 
--     	    	    and hyperplanes given by '{N,w}'
intersection List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that give valid in/equalities
     isValidPair := S -> (
	  test := (#S == 2);
	  if test then (
	       if (S#1 == 0) then ( test = (instance(S#0,Matrix)))
	       else ( test = (instance(S#1,Matrix));
		    if test then ( test = ((numRows(S#0) == numRows(S#1)) and (numColumns(S#1) == 1)))));
	  test);
     -- Checking for input errors  
     if (L=={}) then (error ("List of cones must not be empty"));   
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local Ml,vl,Nl,wl;
     if ((not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,List))) then (
	  error ("The input must be cones, polyhedra, inequalities, equalities."));
     -- Adding the inequalities and equalities to 'M,v,N,w', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = C#"ambientDimension";
	  Ml = halfspaces C;
	  vl = map(QQ^(numgens target halfspaces C),QQ^1,0);
	  Nl = hyperplanes C;
	  wl = map(QQ^(numgens target hyperplanes C),QQ^1,0))
     else if instance(C,Polyhedron) then (
	  n = C#"ambientDimension";
	  Ml = (halfspaces C)#0;
	  vl = (halfspaces C)#1;
	  Nl = (hyperplanes C)#0;
	  wl = (hyperplanes C)#1)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if (not isValidPair(C)) then (
	       error ("Inequalities must be given as a sequence of a matrix and a column vector"));
	  n = numColumns(C#0);
	  if ((ring(C#0) =!= ZZ) and (ring(C#0) =!= QQ)) then (
	       error ("expected halfspaces over ZZ or QQ"));
	  Ml = substitute(C#0,QQ);
	  if (C#1 ==0) then (
	       vl = map(QQ^(numgens target(C#0)),QQ^1,0))
	  else (  
	       if ((ring(C#1) =!= ZZ) and (ring(C#1) =!= QQ)) then (
		    error ("expected halfspaces over ZZ or QQ"));
	       vl = substitute(C#1,QQ));
	  Nl = map(QQ^1,QQ^(numgens source(C#0)),0);
	  wl = map(QQ^1,QQ^1,0))
     else (
	  -- Checking for input errors
	  if (not isValidPair(C)) then (
	       error ("Equalities must be given as a list of a matrix and a column vector"));
	  n = numColumns(C#0);
	  Ml = map(QQ^1,QQ^(numgens source(C#0)),0);
	  vl = map(QQ^1,QQ^1,0);
	  if ((ring(C#0) =!= ZZ) and (ring(C#0) =!= QQ)) then (
	       error ("expected halfspaces over ZZ or QQ"));
	  Nl = substitute(C#0,QQ);
	  if (C#1 == 0) then (
	       wl = map(QQ^(numgens target(C#0)),QQ^1,0))
	  else (  
	       if ((ring(C#1) =!= ZZ) and (ring(C#1) =!= QQ)) then (
		    error ("expected halfspaces over ZZ or QQ"));
	       wl = substitute(C#1,QQ)));
     L = drop(L,1);
     --  Adding the inequalities and equalities to 'M,v,N,w', for each remaining element in 'L', depending on the type of 'C'
     scan(L, C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,List))) then (
		    error ("The input must be cones, polyhedra, inequalities, equalities."));
	       if instance(C1,Cone) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    Ml = Ml || halfspaces C1;
		    vl = vl || map(QQ^(numgens target halfspaces C1),QQ^1,0);
		    Nl = Nl || hyperplanes C1;
		    wl = wl || map(QQ^(numgens target hyperplanes C1),QQ^1,0))
	       else if instance(C1,Polyhedron) then (
		    if ((ambDim C1) != n) then (
			 error ("All Cones and Polyhedra must be in the same ambient space"));
		    Ml = Ml || (halfspaces C1)#0;
		    vl = vl || (halfspaces C1)#1;
		    Nl = Nl || (hyperplanes C1)#0;
		    wl = wl || (hyperplanes C1)#1)
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if (not isValidPair(C1)) then (
			 error ("Inequalities must be given as a sequence of a matrix and a column vector"));
		    if (numColumns(C1#0) != n) then (
			 error ("Inequalities must be for the same ambient space."));
		    if ((ring(C1#0) =!= ZZ) and (ring(C1#0) =!= QQ)) then (
			 error ("expected halfspaces over ZZ or QQ"));
		    Ml = Ml || substitute(C1#0,QQ);
		    if (C1#1 == 0) then (
			 vl = vl || map(QQ^(numgens target(C1#0)),QQ^1,0))
		    else ( 
			 if ((ring(C1#1) =!= ZZ) and (ring(C1#1) =!= QQ)) then (
			      error ("expected halfspaces over ZZ or QQ"));
			 vl = vl || substitute(C1#1,QQ));
		    Nl = Nl || map(QQ^1,QQ^(numgens source(C1#0)),0);
		    wl = wl || map(QQ^1,QQ^1,0))
	       else (
		    -- Checking for input errors
		    if (not isValidPair(C1)) then (
			 error ("Equalities must be given as a list of a matrix and a column vector"));
		    if (numColumns(C1#0) != n) then (
			 error ("Inequalities must be for the same ambient space."));
		    Ml = Ml || map(QQ^1,QQ^(numgens source(C1#0)),0);
		    vl = vl || map(QQ^1,QQ^1,0);
		    if ((ring(C1#0) =!= ZZ) and (ring(C1#0) =!= QQ)) then (
			 error ("expected halfspaces over ZZ or QQ"));
		    Nl = Nl || substitute(C1#0,QQ);
		    if (C1#1 ==0) then (
			 wl = wl || map(QQ^(numgens target(C1#0)),QQ^1,0))
		    else ( 
			 if ((ring(C1#1) =!= ZZ) and (ring(C1#1) =!= QQ)) then (
			      error ("expected halfspaces over ZZ or QQ"));
			 wl = wl || substitute(C1#1,QQ)))));
     if ((vl == 0*vl) and (wl == 0*wl)) then (
	  C = intersection(Ml,Nl))
     else (
	  C = intersection(Ml,vl,Nl,wl));
     C)
     


-- PURPOSE : Building the Fan 'F'
--   INPUT : 'L',  a list of cones and fans in the same ambient space
--  OUTPUT : The fan of all Cones in 'L' and all Cones in of the fans in 'L' and all their faces
makeFan = method(TypicalValue => Fan)
makeFan List := L -> (
     -- Checking for input errors
     if (L == {}) then (error ("List of cones and fans must not be empty"));
     if ((not instance(L#0,Cone)) and (not instance(L#0,Fan))) 
        then (error ("Input must be a list of cones and fans"));
     -- Starting with the first Cone in the list and extracting its information
     C := L#0;
     L = drop(L,1);
     ad := C#"ambientDimension";
     local F;
     if (instance(C,Fan)) then (
	  F = C)
     else (
	  rm := rays C;
	  -- Collecting the rays
	  rayList := {};
	  scan(numgens source rm, i -> (rayList = append(rayList,rm_{i})));
	  -- Generating the new fan
	  F = new Fan;
	  F#"generatingCones" = {C};
	  F#"ambientDimension" = ad;
	  F#"topDimension" = C#"coneDimension";
	  F#"numGeneratingCones" = 1;
	  F#"rays" = rayList;
	  F#"numRays" = #rayList;
	  F#"isPure" = true;
	  F#"faces" = {};
	  F#"isComplete" = false;
	  -- Keeping track of the completeness of the fan (Checking if there are no outer codim 1 faces)
	  if (ad == C#"coneDimension") then (
	       F#"faces" = toList symmDiff(F#"faces",faces(1,C));
	       if (F#"faces" == {}) then (F#"isComplete" = true)));
     -- Adding the remaining cones of the list with 'addCone' 
     while (L != {}) do (
	  C = L#0;
	  L = drop(L,1);
	  if ((not instance(C,Cone)) and (not instance(C,Fan)))
	     then (error ("Input must be a list of cones and fans"));
	  if (instance(C,Fan)) then (
	       L2 := C#"generatingCones";
	       C = L2#0;
	       L2 = drop(L2,1);
	       L = L2 | L);
	  F = addCone(C,F);
	  if (F#?"comperror") then (
	       L = {} ));
     F)


--   INPUT : 'C',  a Cone
--  OUTPUT : The Fan given by 'C' and all of its faces
makeFan Cone := C -> (
     makeFan({C}));



-- PURPOSE : Adding a Cone to an existing fan 
--   INPUT : '(C,F)',  where 'C' is a Cone in the same ambient space as 'F'
--  OUTPUT : The original fan 'F' together with 'C' if it is compatible with the already existing cones, 
--     	     if not the cone win 'F' with which 'C' is not compatible is saved under F#"comperror"
addCone = method(TypicalValue => Fan)
addCone (Cone,Fan) := (C,F) -> (
     -- Checking for input errors
     if ((C#"ambientDimension") != (F#"ambientDimension")) then (error ("Cones must lie in the same ambient space"));
     -- Removing compatability errors if existent
     if (F#?"comperror") then (
	  remove(F,"comperror"));
     --Extracting data
     GC := F#"generatingCones";
     d := C#"coneDimension";
     inserted := false;
     stopper := false;
     newGC := {};
     i := 0;
     -- Cones in the list 'GC' are ordered by decreasing dimension so we start compatibility checks with 
     -- the cones of highest dimension
     while ((not stopper) and (i < #GC)) do (
	  Cf := GC#i;
	  dimCf := Cf#"coneDimension";
	  --local a,b;
	  -- Check if 'C' is still of smaller dimension
	  if (dimCf >= d) then (
	       (a,b) := areCompatible(Cf,C);
	       -- if 'Cf' and 'C' are not compatible then 'C' is not added to 'F' and 
	       -- comperror is given and no further checks are neccessary
	       if (not a) then (
		    newGC = GC;
		    F#"comperror" = {Cf,C};
		    stopper = true)
	       -- if they are compatible and 'C' is a face of 'Cf' then 'C' does not 
	       -- need to be added to 'F' and no further checks are neccessary
	       else if (equals(b,C)) then (
		    newGC = GC;
		    stopper = true)
	       -- otherwise 'Cf' is still a generating Cone and has to be kept and the remaining cones 
	       -- have to be checked
	       else (newGC = append(newGC,Cf)))
	  -- Otherwise the 'C' has higher dimension
	  else (
	       -- If we arrive at the first Cone of smaller dimension than 'C', 'C' has to be inserted here
	       if (not inserted) then (
		    newGC = append(newGC,C);
		    inserted = true);
	       -- Now we have to check for the remaining cones if they are compatible
	       (a1,b1) = areCompatible(Cf,C);
	       if (not a1) then (
		    newGC = GC;
		    F#"comperror" = {Cf,C};
		    stopper = true)
	       -- if one of the remaining cones is a face of 'C' this Cone can be dropped
	       else if (not equals(b1,Cf)) then (
		    newGC = append(newGC,Cf)));
	  i = i+1);
     -- If 'C' was compatible with every Cone, but not the face of any, and is of smaller dimension, 'C' is appended to GC
     if ((not stopper) and (not inserted)) then (
	  newGC = append(newGC,C);
	  inserted = true);
     -- If 'C' was added to the Fan as a generating cone then the codim 1 faces on the boundary have to changed to check for 
     -- completeness
     if inserted then (
	  if (d == C#"ambientDimension") then (
	       F#"faces" = toList symmDiff(F#"faces",faces(1,C));
	       if (F#"faces" == {}) then (F#"isComplete" = true));
	  -- The rays of 'C' have to be added
	  rayList := F#"rays";
	  rm := rays C;
	  scan(numgens source rm, i -> (rayList = append(rayList,rm_{i})));
	  F#"rays" = unique rayList;
	  F#"numRays" = #(F#"rays"));
     -- Saving the fan
     F#"isPure" = (dim(first(newGC)) == dim(last(newGC)));
     F#"generatingCones" = newGC;
     F#"topDimension" = dim(newGC#0);
     F#"numGeneratingCones" = #newGC;
     if (F#?"comperror") then (
	  print "One cone could not be added because it was not compatible.");
     F)


--   INPUT : '(L,F)',  where 'L' is a list of Cones in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones in the list 'L'
addCone (List,Fan) := (L,F) -> (     
    -- Checking for input errors
    if (L == {}) then (
	 error ("The list must not be empty"));
    if ((not instance(L#0,Cone)) and (not instance(L#0,Fan))) then (
	 error ("The list may only contain cones and fans"));
    if (#L == 1) then (F = addCone(L#0,F))
    else if (#L > 1) then ( 
	 F = addCone(L#0,F);
	 if (not F#?"comperror") then (
	      F = addCone(drop(L,1),F)));
    F)


--   INPUT : '(F1,F)',  where 'F1' is a fan in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones of the fan 'F1'
addCone (Fan,Fan) := (F1,F) -> (
     -- Checking for input errors
     if (ambDim(F) != ambDim(F1)) then (
	  error ("The fans must be in the same ambient space"));
     L := F1#"generatingCones";
     addCone(L,F))


equalLists = method(TypicalValue => Boolean)
equalLists (List,List) := (L1,L2) -> (
     L1 = uniquePO L1;
     L2 = uniquePO L2;
     equality := (#L1 == #L2);
     if equality then (
	  while ((L1 != {}) and equality) do (
	       e := L1#0;
	       L1 = drop(L1,1);
	       p = position(L2, l -> (l == e));
	       if (p === null) then (
		    equality = false)
	       else (
		    L2 = drop(L2,{p,p}))));
     equality)
	       
     



-- PURPOSE : Computing the symmetric difference of two lists
--   INPUT : '(S1,S2)',  two lists
--  OUTPUT : The symmetric difference of the two lists
symmDiff = method(TypicalValue => List)
symmDiff(List,List) := (S1,S2) -> (
     L := {};
     L2 := set {};
     scan(S1, s -> (
	       -- Check if 's' is contained in S2, if so remember the indices of its appearance in S2 
	       insert := true;
	       scan(#S2, i -> ( if (s == S2#i) then (
			      insert = false;
			      L2 = L2 + set{i})));
	       -- If not then append it to 'L2'
	       if insert then ( L = append(L,s))));
     -- No add the remaining elements of 'S2' whose indeces have not been saved
     scan(#S2, i -> (if (not member(i,L2)) then ( L = append(L,S2#i))));
     L)

-- PURPOSE : Computing the union of two lists
--   INPUT : '(L1,L2)',  two lists
--  OUTPUT : The union of the two lists
union = method(TypicalValue => List)
union(List,List) := (L1,L2) -> (
     L := {};
     scan(L1, C -> (
	       -- Scanning the elements of 'L1' if they are already in 'L2', those who are not,
	       -- are saved in 'L' and in the end 'L' is appended to 'L2'
	       insert := true;
	       i := 0;
	       while (insert and (i < #L2)) do (if (C == L2#i) then (insert = false); i=i+1);
	       if insert then L = append(L,C)));
     L2|L)


uniquePO = method(TypicalValue => List)
uniquePO List := L -> (
     Lout := {};
     while (L != {}) do (
	  e := L#0;
	  L = select(L, l -> (l != e));
	  Lout = Lout | {e});
     Lout)


-- PURPOSE : Giving the defining affine hyperplanes
ambDim = method(TypicalValue => ZZ)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambDim(Polyhedron) := P -> (
	P#"ambientDimension")

--   INPUT : 'C'  a Cone 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambDim(Cone) := C -> (
	C#"ambientDimension")

--   INPUT : 'F'  a Fan 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambDim(Fan) := F -> (
	F#"ambientDimension")



-- PURPOSE : Giving the k dimensionial Cones of the Fan
--   INPUT : (k,F)  where 'k' is a positive integer and F is a Fan 
--  OUTPUT : a List of Cones
cones = method(TypicalValue => List)
cones(ZZ,Fan) := (k,F) -> (
	-- Checking for input errors
	if ((k < 0) or (dim(F) < k)) then (
	     error ("k must be between 0 and the dimension of the fan."));
	L := {};
	i := 0;
	stopper := false;
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	while (i < F#"numGeneratingCones") and (not stopper) do (
	     C := (F#"generatingCones")#i;
	     if (dim(C) >= k) then (L = union(L,faces(dim(C)-k,C)))
	     else stopper = true;
	     i=i+1);
	L)


	     
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, which is the dimension of the polyhedron
dim(Polyhedron) := P -> (
	P#"polyhedronDimension")


--   INPUT : 'C'  a Cone 
--  OUTPUT : an integer, which is the dimension of the Cone
dim(Cone) := C -> (
	C#"coneDimension")


--   INPUT : 'F'  a Fan 
--  OUTPUT : an integer, which is the highest dimension of Cones in 'F'
dim(Fan) := F -> (
	F#"topDimension")


-- PURPOSE : Giving the generating Cones of the Fan
--   INPUT : 'F'  a Fan
--  OUTPUT : a List of Cones
genCones = method(TypicalValue => List)
genCones(Fan) := F -> (
	F#"generatingCones")



-- PURPOSE : Giving the defining affine halfspaces
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
halfspaces = method()
halfspaces(Polyhedron) := P -> (
	P#"halfspaces")


--   INPUT : 'C'  a Cone
--  OUTPUT : 'M', where M is a matrix and C={x in H | Mx>=0}, where 
--		 H is the intersection of the defining hyperplanes
halfspaces(Cone) := C -> (
	C#"halfspaces")



-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine halfspaces
hyperplanes = method()
hyperplanes(Polyhedron) := P -> (
	P#"hyperplanes")


--   INPUT : 'C'  a Cone
hyperplanes(Cone) := C -> (
	C#"hyperplanes")



-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace(Polyhedron) := P -> (
	P#"linealitySpace")


--   INPUT : 'C'  a Cone
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace(Cone) := C -> (
	C#"linealitySpace")


--   INPUT : 'F'  a Fan
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace(Fan) := F -> (
	C:= (F#"generatingCones")#0;
	C#"linealitySpace")


-- PURPOSE : Giving the rays
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the rays of P as column vectors
rays = method(TypicalValue => Matrix)
rays(Polyhedron) := P -> (
	P#"rays")


--   INPUT : 'C'  a Cone
rays(Cone) := C -> (
	C#"rays")


--   INPUT : 'F'  a Fan
rays(Fan) := F -> (
	F#"rays")


   
-- PURPOSE : Giving the vertices
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the vertices of P as column vectors
vertices = method(TypicalValue => Matrix)
vertices(Polyhedron) := P -> (
	P#"vertices")



-- PURPOSE : Tests whether the intersection of two Polyhedra/Cones is a face of both
areCompatible = method()

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : '(Boolean,Cone)'   (true,the intersection),if their intersection is a face of each and false otherwise
areCompatible(Cone,Cone) := (C1,C2) -> (
     test := false;
	if ((C1#"ambientDimension") == (C2#"ambientDimension")) then (
	     I := intersection(C1,C2);
	     test = isFace(I,C1) and isFace(I,C2);
	     test = (test,I));
	test)


-- PURPOSE : Tests whether the intersection of two Polyhedra/Cones is a face of both
commonFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
commonFace(Polyhedron,Polyhedron) := (P,Q) -> (
	test := false;
	if ((P#"ambientDimension") == (Q#"ambientDimension")) then (
	     I := intersection(P,Q);
	     test = isFace(I,P) and isFace(I,Q));
	test)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
commonFace(Cone,Cone) := (C1,C2) -> (
	test := false;
	if ((C1#"ambientDimension") == (C2#"ambientDimension")) then (
	     I := intersection(C1,C2);
	     test = isFace(I,C1) and isFace(I,C2));
	test)



-- PURPOSE : Check if 'P' contains 'Q'
--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
contains = method(TypicalValue => Boolean)
contains(Polyhedron,Polyhedron) := (P,Q) -> (
      -- checking for input errors
      if (P#"ambientDimension" =!= Q#"ambientDimension") then (
	   error ("Polyhedra must lie in the same ambient space"));
      -- Saving the equations of P and vertices/rays of Q
      (A,B) := P#"homogenizedHalfspaces";
      (C,D) := Q#"homogenizedVertices";
      A = transpose A;
      B = transpose B;
      E := A*C;
      test := true;
      -- Checking if vertices/rays of Q satisfy the equations of P
      scan(flatten entries E, e -> (test = test and (e<=0)));
      test = test and (A*D == 0*A*D) and (B*C == 0*B*C) and (B*D == 0*B*D);
      test)


-- PURPOSE : Check if 'C1' contains 'C2'
--   INPUT : '(C1,C2)'  two Cones
contains(Cone,Cone) := (C1,C2) -> (
      -- checking for input errors
      if (C1#"ambientDimension" =!= C2#"ambientDimension") then (
	   error ("Cones must lie in the same ambient space"));
      -- Saving the equations of C1 and rays of C2
      (A,B) := C1#"dualgens";
      (C,D) := C2#"genrays";
      A = transpose A;
      B = transpose B;
      E := A*C;
      test := true;
      -- Checking if the rays of C2 satisfy the equations of C1
      scan(flatten entries E, e -> (test = test and (e<=0)));
      test = test and (A*D == 0*A*D) and (B*C == 0*B*C) and (B*D == 0*B*D);
      test)
 

 
-- PURPOSE : Check if 'C' contains 'P'
--   INPUT : '(C,P)'  a Cone and a Polyhedron
contains(Cone,Polyhedron) := (C,P) -> (
      -- checking for input errors
      if (C#"ambientDimension" =!= P#"ambientDimension") then (
	   error ("Cone and Polyhedron must lie in the same ambient space"));
      -- Saving the equations of C and vertices/rays of P
      M := (P#"vertices") | (P#"rays");
      C1:= posHull M;
      contains(C,C1))



-- PURPOSE : Check if 'P' contains 'C'
--   INPUT : '(P,C)'  a Polyhedron and a Cone
contains(Polyhedron,Cone) := (P,C) -> (
      -- checking for input errors
      if (C#"ambientDimension" =!= P#"ambientDimension") then (
	   error ("Polyhedron and Cone must lie in the same ambient space"));
      -- Saving the cone 'C' as a polyhedron and using the function on two polyhedra
      Q := coneToPolyhedron(C);
      contains(P,Q))



-- PURPOSE : Check if 'P' contains 'p'
--   INPUT : '(P,p)'  a Polyhedron 'P' and a point 'p' given as a matrix
contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if (P#"ambientDimension" =!= (numgens target p)) then (
	   error ("Polyhedron and point must lie in the same ambient space"));
       if ((numgens source p) =!= 1) then (
	   error ("The point must be given as a one row matrix"));
      contains(P,convexHull(p)))



-- PURPOSE : Check if 'C' contains 'p'
--   INPUT : '(C,p)'  a Cone 'C' and a point 'p' given as a matrix
contains(Cone,Matrix) := (C,p) -> (
      -- checking for input errors
      if (C#"ambientDimension" =!= (numgens target p)) then (
	   error ("Polyhedron and point must lie in the same ambient space"));
       if ((numgens source p) =!= 1) then (
	   error ("The point must be given as a one row matrix"));
      contains(C,convexHull(p)))



-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Cone) := (L,C) -> (
      i := 0;
      stopper := false;
      while (not stopper) and (i < #L) do (
	   C1 := L#i;
	   -- checking for input errors
	   if (instance(C1,Cone)) then (
		if (ambDim(C) == ambDim(C1)) then (stopper = equals(C,C1)));
	   i = i+1);
      stopper)
 
 
-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Polyhedron) := (L,P) -> (
      i := 0;
      stopper := false;
      while (not stopper) and (i < #L) do (
	   P1 := L#i;
	   -- checking for input errors
	   if (instance(P1,Polyhedron)) then (
		if (ambDim(P) == ambDim(P1)) then (stopper = equals(P,P1)));
	   i = i+1);
      stopper)
 
 
-- PURPOSE : Check if 'F' contains 'C'
--   INPUT : '(F,C)'  a Fan 'F' and a Cone 'C'
contains(Fan,Cone) := (F,C) -> (
      -- Checking for input errors
      if (ambDim(F) != ambDim(C)) then (
	   error ("Fan and Cone must lie in the same ambient space"));
      -- Making the list of cones of same dimension as 'C'
      L := cones(dim(C),F);
      contains(L,C))
      



-- PURPOSE : Check if 'P' equals 'Q'
--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
equals = method(TypicalValue => Boolean)
equals(Polyhedron,Polyhedron) := (P,Q) -> (
     contains(P,Q) and contains(Q,P))      



-- PURPOSE : Check if 'C1' equals 'C2'
--   INPUT : '(C1,C2)'  two Cones
equals(Cone,Cone) := (C1,C2) -> (
     contains(C1,C2) and contains(C2,C1))      


-- PURPOSE : Check if 'F1' equals 'F2'
--   INPUT : '(F1,F2)'  two Fans
equals(Fan,Fan) := (F1,F2) -> (
     (symmDiff(F1#"generatingCones",F2#"generatingCones") == {}))      

Polyhedron == Polyhedron := (P,Q) -> (equals(P,Q))

Polyhedron == Thing := (P,T) -> (false)

Cone == Cone := (C1,C2) -> (equals(C1,C2))

Cone == Thing := (C,T) -> (false)

Fan == Fan := (F1,F2) -> (equals(F1,F2))

Fan == Thing := (F,T) -> (false)

Thing == Polyhedron := (T,P) -> (false)

Thing == Cone := (T,C) -> (false)

Thing == Fan := (T,F) -> (false)




-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact(Polyhedron) := P -> (
     ((P#"linealitySpace" == 0) and (P#"rays" == 0)))


-- PURPOSE : Tests if a Fan is complete
--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isComplete = method(TypicalValue => Boolean)
isComplete(Fan) := F -> (
     F#"isComplete")


isEmpty = method(TypicalValue => Boolean)
isEmpty Polyhedron := P -> (
     P#"polyhedronDimension" == -1)



     
-- PURPOSE : Tests if the first Polyhedron/Cone is a face of the second Polyhedron/Cone
isFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
isFace(Polyhedron,Polyhedron) := (P,Q) -> (
     test := false;
     -- Checking if the two polyhedra lie in the same space and computing the dimension difference
     if ((P#"ambientDimension") == (Q#"ambientDimension")) then (
	  c := (Q#"polyhedronDimension")-(P#"polyhedronDimension");
	  if (c >= 0) then (
	       -- Checking if one of the codim 'c' faces of Q is P
	       L := faces(c,Q);
	       i := 0;
	       while ((not test) and (i < #L)) do (
		    test = equals(P,L#i);
		    i = i+1)));
     test)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
isFace(Cone,Cone) := (C1,C2) -> (
     test := false;
     -- Checking if the two cones lie in the same space and computing the dimension difference
     if ((C1#"ambientDimension") == (C2#"ambientDimension")) then (
	  c := (C2#"coneDimension")-(C1#"coneDimension");
	  if (c >= 0) then (
	       -- Checking if one of the codim 'c' faces of C2 is C1
	       L := faces(c,C2);
	       i := 0;
	       while ((not test) and (i < #L)) do (
		    test = equals(C1,L#i);
		    i = i+1)));
     test)
     

-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed = method(TypicalValue => Boolean)
isPointed Cone := C -> (
     (rank(C#"linealitySpace") == 0))


--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     isPointed((F#"generatingCones")#0))



-- PURPOSE : Tests if a Fan is projective
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isProjective = method(TypicalValue => Boolean)
isProjective(Fan) := F -> (
     test := false;
     -- First of all the fan must be complete
     if (isComplete(F)) then (
	  -- Extracting the generating cones, the ambient dimension, the codim 1 
	  -- cones (corresponding to the edges of the polytope if it exists)
	  i := 0;
	  L := hashTable apply(F#"generatingCones", l -> (i=i+1; i=>l));
	  n := F#"ambientDimension";
	  edges := cones(n-1,F);
	  -- Making a table that indicates in which generating cones each 'edge' is contained
	  edgeTCTable := hashTable apply(edges, e -> ( select(1..(#L), j -> (contains(L#j,e))) => e));
	  i = 0;
	  -- Making a table of all the edges where each entry consists of the pair of top cones corr. to
	  -- this edge, the codim 1 cone, an index number i, and the edge direction from the first to the
	  -- second top Cone
	  edgeTable := apply(pairs edgeTCTable, e -> (i=i+1; 
		    v := transpose hyperplanes (e#1);
		    if (not (contains(dualCone(L#((e#0)#0)),v))) then (v = (-1)*v);
		    (e#0, e#1, i, v)));
	  edgeTCNoTable := hashTable apply(edgeTable, e -> (e#0 => (e#2,e#3)));
	  edgeTable = hashTable apply(edgeTable, e -> (e#1 => (e#2,e#3)));
	  corrList := {};
	  -- Computing the list of correspondencies, i.e. for each codim 2 cone ( corresponding to 2dim-faces of the polytope) save 
	  -- the indeces of the top cones containing it
	  scan(keys L, j -> (
		    M := faces(2,L#j);
		    scan(M, C -> (
			      stopper := false;
			      k := 0;
			      while ((k < #corrList) and (not stopper)) do (
				   if (C == corrList#k#0) then (
					corrList = take(corrList,{0,k-1})|{{corrList#k#0,append(corrList#k#1,j)}}|take(corrList,{k+1,(#corrList)-1});
					stopper = true); 
				   k=k+1);
			      if (not stopper) then corrList = append(corrList, {C,{j}})))));
	  --  Generating the 0 matrix for collecting the conditions on the edges
	  m = #(keys edgeTable);
	  HP = map(QQ^0,QQ^m,0);
	  NM = map(QQ^n,QQ^m,0);
	  -- for each entry of corrlist another matrix is added to HP
	  scan(#corrList, j -> (
		    v := corrList#j#1;
		    HPnew := NM;
		    -- Scanning trough the top cones containing the active codim2 cone and order them in a circle by their 
		    -- connecting edges
		    v = apply(v, e -> (L#e));
		    C := v#0;
		    v = drop(v,1);
		    C1 := C;
		    while (v != {}) do (
			 C2 := v#0;
			 v = drop(v,1);
			 C' := intersection(C1,C2);
			 if (dim(C') == n-1) then (
			      scan(keys edgeTable, k -> ( if (k == C') then (a,b)=edgeTable#k));
			      if (not contains(dualCone(C2),b)) then ( b = (-1)*b);
			      -- 'b' is the edge direction inserted in column 'a' which is the index of this edge
			      HPnew = (HPnew_{0..a-2})|(b)|(HPnew_{a..m-1});
			      C1 = C2)
			 else (v = append(v,C2)));
		    C'' := intersection(C,C1);
		    scan(keys edgeTable, k -> ( if (k == C'') then (a,b)=edgeTable#k));
		    if (not contains(dualCone(C),b)) then ( b = (-1)*b);
		    -- 'b' is the edge direction inserted in column 'a' which is the index of this edge
		    HPnew = (HPnew_{0..a-2})|(b)|(HPnew_{a..m-1});
 		    -- the new restriction is that the edges around that codim2 Cone must add up to 0
		    HP = HP || HPnew));
	  -- Find an interior vector in the cone of all positive vectors satisfying the restrictions
	  v := flatten entries interiorVector intersection(id_(QQ^m),HP);
	  M := {};
	  -- If the vector is strictly positive then there is a polytope with 'F' as normalFan
	  if (all(v, e -> (e > 0))) then (
	       -- Construct the polytope
	       i = 1;
	       -- Start with the origin
	       p := map(QQ^n,QQ^1,0);
	       M = {p};
	       Lyes := {};
	       Lno := {};
	       vlist := keys edgeTCTable;
	       -- Walk alung all edges recursively
	       edgerecursion := (i,p,vertexlist,Mvertices) -> (
		    vLyes := {};
		    vLno := {};
		    -- Sorting those edges into 'vLyes' who emerge from vertex 'i' and the rest in 'vLno'
		    scan(vertexlist, w -> (
			      if (member(i,w)) then (vLyes = append(vLyes,w))
			      else (vLno = append(vLno,w))));
		    -- Going along the edges in 'vLyes' with the length given in 'v' and calling edgerecursion again with the new index of the new 
		    -- top Cone, the new computed vertex, the remaining edges in 'vLno' and the extended matrix of vertices
		    scan(vLyes, w -> (
			      j := edgeTCNoTable#w;
			      if (w#0 == i) then (
				   (vLno,Mvertices) = edgerecursion(w#1,p+(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p+(j#1)*(v#((j#0)-1)))))
			      else (
				   (vLno,Mvertices) = edgerecursion(w#0,p-(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p-(j#1)*(v#((j#0)-1)))))));
		    (vLno,Mvertices));
	       -- Start the recursion with vertex '1', the origin, all edges and the vertexmatrix containing already the origin
	       M = unique ((edgerecursion(i,p,vlist,M))#1);
	       verts := M#0;
	       scan(1..(#M-1), j -> (verts = verts | M#j));
	       -- Computing the convex hull
	       test = convexHull verts));
     test)



-- PURPOSE : Checks if the Fan is of pure dimension
--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isPure = method(TypicalValue => Boolean)
isPure Fan := F -> (
      F#"isPure")


-- PURPOSE : Checks if the input is smooth
isSmooth = method(TypicalValue => Boolean)

--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isSmooth Cone := C -> (
      -- generating the non-linealityspace cone of C
      smooth := false;
      R := rays C;
      C = posHull R;
      -- if the cone is full dimensional then it is smooth iff its rays form a basis over ZZ
      if (dim(C) == ambDim(C)) then (
	   if ((numgens source R) == (numgens target R)) then ( smooth = (abs(det(R)) == 1)))
      -- otherwise the rays must be part of a basis over ZZ
      else (
	   if ((numgens source R) == dim(C)) then (
		A := (smithNormalForm(R))#0;
		smooth = (det(A^{0..(numgens source A)-1}) == 1)));
      smooth);
	   

--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isSmooth Fan := F -> (
     -- Checking isSmooth for every generating cone of the fan
     smooth := true;
     i := 0;
     while ((i < F#"numGeneratingCones") and (smooth)) do (
	  C := (F#"generatingCones")#i;
	  smooth = isSmooth(C);
	  i = i+1);
     smooth)



-- PURPOSE : Computing the faces of codimension 'k' of 'P'
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'P'  a polyhedron
--  OUTPUT : a List, containing the faces as polyhedra
faces = method(TypicalValue => List)
faces(ZZ,Polyhedron) := (k,P) -> (
     --Checking for input errors
     if (k < 0) or (k > dim(P)) then (
	  error ("the codimension must be between 0 and the dimension of the polyhedron"));
     d := dim(P) - k;
     dl := P#"linealitySpaceDimension";
     LS := P#"linealitySpace";
     -- for d=dim(P) it is the polyhedron itself
     if (d == dim(P)) then ({P})
     -- for k=dim(P) the faces are the vertices
     else if (d == dl) then (V := vertices P;
	  -- Generating the list of vertices with each vertex as a polyhedron
	  Lf:={};
	  scan(numgens source V, i -> (Lf = join(Lf,{convexHull(V_{i},LS)})));
	  Lf)
     else if (d < dl) then ({})
     else (
	  -- Saving the halfspaces and hyperplanes
	  (HS,v) := halfspaces P;
	  (HP,w) := hyperplanes P;
	  -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
	  F := apply(numRows HS, i -> (intersection(HS,v,(HP||HS^{i}),(w||v^{i}))));
	  F = apply(F, f -> (
		    V := vertices(f);
		    R := rays(f);
		    (set apply(numColumns(V), i -> V_{i}),set apply(numColumns(R), i -> R_{i}))));
	  -- Saving the lineality space which is the also the lineality space of each face
	  -- Duplicating the list of facets
	  L := F;
	  i := 1;
	  -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	  while (i<k) do (
	       L = intersectionwithFacets(L,F);
	       i = i+1);
	  -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
	  L = apply(L, l -> (
		    l = (toList(l#0),toList(l#1));
		    V := l#0#0;
		    local R;
		    scan(drop(l#0,1), m -> (V=V|m));
		    if (l#1 != {}) then (
			 R = l#1#0;
			 scan(drop(l#1,1), m -> (R=R|m)))
		    else (R = matrix toList(numgens target V:{0_QQ}));
		    if (LS != 0) then (
			 R = R | LS | ((-1)*LS));
		    convexHull(V,R)));
	  L))


--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a polyhedron
--  OUTPUT : a List, containing the faces as cones
--faces(ZZ,Cone) := (k,C) -> (
--     -- checking for input errors
--     if ((k < 0) or (k > (C#"coneDimension"))) then (
--	  error ("invalid dimension"));
--     -- defining recursive face builder, that memorizes which halfspaces have already been choosen ('p')
--     -- how many halfspaces have already been choosen ('c')
--     -- chooses in each step one of the remaining halfspaces (each of them for a new recursion) and
--     -- and adds the equation to the hyperplane equations
--     -- if the counter 'c' reaches 0 then it checks if the resulting cone has the correct dimension ('d')
--     -- and adds it to L
--     facerecursion := (p,c,HS,HP,L,d) -> (
--	  if (c == 0) then (
--	       test := false;
--	       Q := intersection(HS,HP);
--	       if ((Q#"coneDimension") == d) then (
--		    scan(L, F -> ( test = test or (equals(F,Q))));
--		    if (not test) then L = join(L,{Q})))
--	  else ( c = c-1;
--	       scan(p..(numgens target HS)-1, i -> (
--			 HPnew := HP||(HS^{i});
--			 L = facerecursion(i+1,c,HS,HPnew,L,d))));
--	  L);
--     -- Saving the dimension of the faces
--     d :=  (C#"coneDimension")-k;
--     counter := k;
--     pos := 0;
--     HS := halfspaces C;
--     HP := hyperplanes C;
--     L := {};
--     if (d == 0) then ( M := map(QQ^(C#"ambientDimension"),QQ^1,0);
--	  L = {posHull(M)})
--     else if (d == 1) then (
--	  R := rays C;
--	  L = apply(numColumns R, i -> ( posHull(R_{i}))))
--     else ( L = facerecursion(pos,counter,HS,HP,L,d));
--     L)



--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a polyhedron
--  OUTPUT : a List, containing the faces as cones
faces(ZZ,Cone) := (k,C) -> (
     d := dim(C) - k;
     dl := C#"linealitySpaceDimension";
     LS := linSpace C;
     --Checking for input errors
     if (d < 0) or (d > dim(C)) then (
	  error ("the codimension must be between 0 and the dimension of the cone"))
     -- for d=dim(C) it is the cone itself
     else if (d == dim(C)) then ({C})
     -- for d=dl it is the lineality space
     else if (d == dl) then (
	  {posHull(LS | (-LS))})
     -- for d=dl+1 it is the lineality space plus one of the rays
     else if (d == dl+1) then (
	  -- Generating the list of cones given by one ray and the lineality space
	  R := rays C;
	  Lf:={};
	  scan(numgens source R, i -> (Lf = join(Lf,{posHull(R_{i},LS)})));
	  Lf)
     else if (0 <= d) and (d < dl) then (
	  {})
     else (
	  -- Saving the halfspaces and hyperplanes
	  HS := halfspaces C;
	  HP := hyperplanes C;
	  -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
	  F := apply(numRows HS, i -> (intersection(HS,(HP||HS^{i}))));
	  F = apply(F, f -> (
		    R := rays(f);
		    (set apply(numColumns(R), i -> R_{i}))));
	  -- Duplicating the list of facets
	  L := F;
	  i := 1;
	  -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	  while (i<k) do (
	       L = intersectionwithFacetsCone(L,F);
	       i = i+1);
	  -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
	  L = apply(L, l -> (
		    l = toList(l);
		    Rs := l#0;
		    scan(drop(l,1), m -> (Rs = Rs | m));
		    posHull(Rs,LS)));
	  L))
	  
     
     

     
--faces = method(TypicalValue => List)
--faces(ZZ,Polyhedron) := (k,P) -> (
--     -- checking for input errors
--     if ((k < 0) or (k > (P#"polyhedronDimension"))) then (
--	  error ("invalid dimension"));
--     -- defining recursive face builder, that memorizes which halfspaces have already been choosen ('p')
--     -- how many halfspaces have already been choosen ('c')
--     -- chooses in each step one of the remaining halfspaces (each of them for a new recursion) and
--     -- and adds the equation to the hyperplane equations
--     -- if the counter 'c' reaches 0 then it checks if the polyhedron has the correct dimension ('d')
--     -- and adds it to L
--     facerecursion := (p,c,HS,v,HP,w,L,d) -> (
--	  if (c == 0) then (
--	       test := false;
--	       Q := intersection(HS,v,HP,w);
--	       if ((Q#"polyhedronDimension") == d) then (
--		    scan(L, F -> ( test = test or (equals(F,Q))));
--		    if (not test) then L=join(L,{Q})))
--	  else ( c = c-1;
--	       scan(p..(numgens target HS)-1, i -> (
--			 HPnew := HP||(HS^{i});
--			 wnew := w||(v^{i});
--			 L = facerecursion(i+1,c,HS,v,HPnew,wnew,L,d))));
--	  L);
--     -- Saving the dimension of the faces
--     d :=  (P#"polyhedronDimension")-k;
--     counter := k;
--     pos := 0;
--     (HS,v) := halfspaces P;
--     (HP,w) := hyperplanes P;
--     L := {};
--     if (d == 0) then ( V := vertices P;
--	  scan(numgens source V, i -> (L = join(L,{convexHull(V_{i})}))))
--     else ( L = facerecursion(pos,counter,HS,v,HP,w,L,d));
--     L)




-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)
fVector(Polyhedron) := P -> (
     -- applying faces to P for every positive int up to the dimension
     L := {};
     scan((P#"polyhedronDimension")+1, d -> (L = join({#faces(d,P)},L)));
     L)



--   INPUT : 'C'  a Cone
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector(Cone) := C -> (
     -- applying faces to P for every positive int up to the dimension
     L := {};
     scan((C#"coneDimension")+1, d -> (L = join({#faces(d,C)},L)));
     L)

-- AUXILIARY FUCTION for computing the Hilbert Basis of a Cone
constructHilbertBasis := (A) -> (
    -- Defining the function to determine if u is lower v
    lowvec := (u,v) -> (
	 test := true;
	 i := 0;
	 n := (numgens target u)-1;
	 while (test and (i < n)) do (
	      test = (flatten entries u)#i <= (flatten entries v)#i;
	      i = i+1);
	 test = test and (abs((flatten entries u)#n) <= abs((flatten entries v)#n)) and (((flatten entries u)#n)*((flatten entries v)#n) >= 0);
	 test);
    -- Collecting data
    A = substitute(A,ZZ);
    H := {A^{0}_{0}};
    s := numgens target A;
    n := numgens source A;
    --doing the project and lift algorithm step by step with increasing dimensions
    scan(n-1, i -> (
	      -- the set 'F' will contain the lifted basis vectors, 'B' are the first i+2 columns of 'A' as a rowmatrix,
	      -- the set 'H' contains the vectors from the last loop which are one dimension smaller
	      F := {};
	      B := transpose A_{0..(i+1)};
	      -- Decide between lifting the existing vectors (i>s-1) or also adding the next column of 'B'
	      if (i < (s-1)) then (
		   -- Lifting the existing vectors from 'H'
		   scan(H, h -> (
			     j := 0;
			     while ((numgens target h) == (i+1)) do (
				  if (isSubset(image(h || matrix{{j}}), image B)) then (h = (h || matrix{{j}}));
				  j=j+1);
			     F = append(F,h)));
		   -- Adding +- the next column of 'B'
		   F = join(F,{B_{i+1}^{0..(i+1)},(-1)*B_{i+1}^{0..(i+1)}}))
	      else (
		   -- Lifting the existing vectors from 'H'
		   nullmap := map(ZZ^1,ZZ^s,0);
		   nullvec := map(ZZ^1,ZZ^1,0);
		   scan(H, h -> (
			     h = B*substitute(vertices intersection(nullmap,nullvec,B^{0..i},h),ZZ);
			     F = append(F,h))));
	      C := {};
	      -- Computing the S-pairs from the elements of 'F' and saving them in 'C'
	      scan(#F-1, k -> (
			scan(#F-k-1, l -> (
				  f := F#k;
				  g := F#(k+l+1);
				  if ((((entries f)#(i+1)#0)*((entries g)#(i+1)#0) < 0) and ((f+g) != 0*(f+g))) then (C=append(C,f+g))))));
	      Cnew := {};
	      -- The elements of 'F' are saved in 'G'
	      G := F;
	      j := 0;
	      -- Adding those elements of 'C' to 'G' that satisfy the "normalform" condition by increasing last entry
	      while (C != {}) do (
		   f := C#0;
		   C = drop(C,1);
		   if ( (sum drop(flatten entries f,-1)) != j) then (
			Cnew = append(Cnew,f))
		   else (
			test := true;
			k := 0;
			while (test and (k <= (#G-1))) do (
			     test = not lowvec(G#k,f);
			     k = k+1);
			if test then (
			     scan(G, g -> (
				       if ((((entries f)#(i+1)#0)*((entries g)#(i+1)#0) < 0) and ((f+g) != 0*(f+g))) then (C=append(C,f+g))));
			     G = append(G,f)));
		   if (C == {}) then (
			j = j+1;
			C = Cnew;
			Cnew = {}));
	      -- saving those elements of 'G' with positive last entry into 'H'
	      H = {};
	      scan(G, g -> (
			if ((entries g)#(i+1)#0 >= 0) then ( H=append(H,g))))));
    H)


-- PURPOSE : Computing the Hilbert basis of a Cone 
--   INPUT : 'C',  a Cone
--  OUTPUT : 'L',  a list containing the Hilbert basis as one column matrices 
hilbertBasis = method(TypicalValue => List)
hilbertBasis Cone := (C) -> (
     -- Computing the row echolon form of the matrix M
     ref := (M) -> (
	  n := numgens source M;
	  s := numgens target M;
	  BC := map(ZZ^n,ZZ^n,1);
	  m := min(n,s);
	  -- Scan through the first square part of 'M'
	  i := 0;
	  stopper := 0;
	  while ((i<m) and (stopper<n)) do (
		    j := i;
		    stop := false;
		    -- Selecting the first non-zero entry after the i-th row in the i-th column
		    while ((not stop) and (j < s)) do (
			 if (M_i_j != 0) then (stop = true)
			 else (j = j+1));
		    -- if there is a non-zero entry, scan the remaining entries and compute the reduced form for this column
		    if stop then (
			 scan((j+1)..(s-1), k -> (
				   if (M_i_k != 0) then (
					a := M_i_j;
					b := M_i_k;
					L := gcdCoefficients(a,b);
					a = substitute(a/(L#0),ZZ);
					b = substitute(b/(L#0),ZZ);
					M = (M^{0..j-1}) || ((L#1)*M^{j})+((L#2)*M^{k}) || (M^{j+1..k-1}) || ((-b)*M^{j})+(a*M^{k}) || (M^{k+1..s-1}))));
			 if (i != j) then (
			      M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1});
			 if (M_i_i < 0) then (M = M^{0..i-1} || (-1)*M^{i} || M^{i+1..s-1}))
		    else (
			 M = M_{0..i-1} | M_{i+1..n-1} | M_{i};
			 BC = BC_{0..i-1} | BC_{i+1..n-1} | BC_{i};
			 i = i-1);
		    i = i+1;
		    stopper = stopper + 1);
	  (M,BC));
     -- Function to compute the/one preimage of h under A
     preim := (h,A) -> (
	  -- Take the generators of the kernel of '-h|A' and find an element with 1 as first entry -> the other entrys are a preimage
	  -- vector
	  N := gens(ker(-h|A));
	  N = transpose((ref(transpose(N)))#0);
	  N_{0}^{1..(numgens target N)-1});
     A := C#"halfspaces";
     if (C#"hyperplanes" != 0) then ( A = A || (C#"hyperplanes") || ((-1)*(C#"hyperplanes")));
     A = substitute(A,ZZ);
     -- Use the project and lift algorithm to compute a basis of the space of vectors positive on 'A' whose preimages are the HilbertBasis
     (B,BC) := ref transpose A; 
     H := constructHilbertBasis B;
     BC = inverse(transpose(BC));
     apply(H,h -> preim(BC*h,A)))


-- PURPOSE : Get the pair of incompatible cones if they appear during a fan construction
--   INPUT : 'F',  a fan
--  OUTPUT : 'L',  a list, empty if there is no pair of incompatible cones, otherwise the 
--                 two cones starting with the one already in the fan
incompCones = method(TypicalValue => List)
incompCones Fan := F -> (
     L := {};
     if (F#?"comperror") then (
	  L = F#"comperror");
     L)


-- PURPOSE : Checking if a point is an interior point of a Polyhedron or Cone 
inInterior = method(TypicalValue => Boolean)


--   INPUT : '(p,P)',  where 'p' is a point given by a matrix and 'P' is a Polyhedron
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Polyhedron) := (p,P) -> (
     smallestFace(p,P) == P)

--   INPUT : '(p,C)',  where 'p' is a point given by a matrix and 'C' is a Cone
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Cone) := (p,C) -> (
     smallestFace(p,C) == C)


-- PURPOSE : Computing a point in the relative interior of a cone or Polyhedron 
interiorPoint = method(TypicalValue => Matrix)


--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'p',  a point given as a matrix
interiorPoint Polyhedron := P -> (
     -- Checking for input errors
     if (isEmpty(P)) then (
	  error ("The polyhedron must not be empty"));
     Vm := vertices P;
     n := numgens source Vm;
     ones := matrix toList(n:{1/n});
     -- Take the '1/n' weighted sum of the vertices
     Vm * ones)


-- PURPOSE : Computing an interior vector of a cone
--   INPUT : 'C',  a Cone
--  OUTPUT : 'p',  a point given as a matrix 
interiorVector = method(TypicalValue => Matrix)
interiorVector Cone := C -> (
     if (dim(C) == 0) then (
	  map(QQ^(ambDim(C)),QQ^1,0))
     else (
	  Rm := rays C;
	  ones := matrix toList(numgens source Rm:{1_QQ});
	  -- Take the sum of the rays
	  iv := Rm * ones;
	  d := abs gcd flatten entries iv;
	  (1/d)*iv))


-- PURPOSE : Computing the lattice points of a compact Polyhedron 
--   INPUT : 'P',  a Cone
--  OUTPUT : 'L',  a list containing the lattice points of 'P'
latticePoints = method(TypicalValue => List)
latticePoints Polyhedron := P -> (
     -- Checking for input errors
     if (not isCompact(P)) then (
	  error ("The polyhedron must be compact"));
     -- Computing the Hilbert basis of the cone over 'P' on height 1
     V := (vertices(P)) || (matrix{toList((numgens source vertices(P)):1_QQ)});
     L := hilbertBasis posHull V;
     -- The lattice points are those Hilbert Basis elements on height 1
     L = apply(select(L, l -> (last(flatten(entries(l))) == 1)), v -> (v^{0..(ambDim(P)-1)}));
     L)


-- PURPOSE : Computing the Cone of the Minkowskisummands of a Polyhedron 'P', the minimal 
--           Minkowskisummands, and minimal decompositions
--   INPUT : 'P',  a polyhedron
--  OUTPUT : '(C,L,M)'  where 'C' is the Cone of the Minkowskisummands, 'L' is a list of 
--                      Polyhedra corresponding to the generators of 'C', and 'M' is a 
--                      matrix where the columns give the minimal decompositions of 'P'.
minkSummandCone = method()
minkSummandCone Polyhedron := P -> (
     -- Subfunction to save the two vertices of a compact edge in a matrix where the vertex with the smaller entries comes first
     -- by comparing the two vertices entry-wise
     normvert := M -> ( 
	  M = toList M; 
	  v := (M#0)-(M#1);
	  normrec := w -> ( local i; if ((entries(w))#0#0 > 0) then (i=0) else if ((entries(w))#0#0 < 0) then (i=1) else (w=w^{1..(numgens target w)-1}; i=normrec(w)); i);
          i = normrec(v);
	  if (i == 1) then (M = {M#1,M#0});
	  M);
     -- If the polyhedron is 0 or 1 dimensional itself is its only summand
     if (dim(P) == 0) or (dim(P) == 1) then (
	  (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}}))
     else (
	  -- Extracting the data to compute the 2 dimensional faces and the edges
	  d := P#"ambientDimension";
          dP := P#"polyhedronDimension";
          (HS,v) := halfspaces P;
          (HP,w) := hyperplanes P;
	  F := apply(numRows HS, i -> (intersection(HS,v,(HP||HS^{i}),(w||v^{i}))));
	  F = apply(F, f -> (
		    V := vertices(f);
		    R := rays(f);
		    (set apply(numColumns(V), i -> V_{i}),set apply(numColumns(R), i -> R_{i}))));
	  LS := linSpace P;
	  L := F;
	  i := 1;
	  while (i<(dP-2)) do (
	       L = intersectionwithFacets(L,F);
	       i = i+1);
	  -- Collect the compact edges
	  L1 = select(L, l -> ( l#1 === set{}));
	  -- if the polyhedron is 2 dimensional and not compact then every compact edge with the tailcone is a summand
	  if (dim(P) == 2) and (not isCompact(P)) then (
	       L1 = intersectionwithFacets(L,F);
	       L1 = select(L, l -> ( l#1 === set{}));
	       if (#L1 == 0) or (#L1 == 1) then (
		    (posHull(matrix{{1}}),hashTable {0 => P},matrix{{1}}))
	       else (
		    TailC := rays(P);
		    if (linSpace(P) != 0) then ( TailC = TailC | linSpace(P) | (-1)*(linSpace(P)));
		    (posHull map(QQ^(#L1),QQ^(#L1),1),hashTable apply(#L1, i -> (l := L1#i; i => convexHull((l#0|l#1),TailC))),matrix toList(#L1:{1_QQ}))))
	  else (
	       -- If the polyhedron is compact and 2 dimensional then there is only one 2 faces
	       if (dim(P) == 2) then (
		    L1 = {(set apply(numColumns vertices P, i -> ( ((vertices(P))_{i}))), set {})});
	       edges := {};
	       edgesTable := edges;
	       condmatrix := map(QQ^0,QQ^0,0);
	       scan(L1, l -> (
			 -- for every 2 face we get a couple of rows in the condition matrix for the edges of this 2 face
			 -- for this the edges if set in a cyclic order must add up to 0. These conditions are added to 
			 -- 'condmatrix' by using the indices in edges
			 ledges := apply(intersectionwithFacets({l},F), e -> (normvert(e#0)));
			 scan(ledges, e -> (
				   newedgecounter := 0;
				   -- adding e to edges if not yet a member
				   if (not member(e,edges)) then (
					newedgecounter = newedgecounter +1;
					edges = append(edges,e));
				   -- extending the condmatrix by a column of zeros for the new edge
				   condmatrix = condmatrix | map(QQ^(numRows condmatrix),QQ^(newedgecounter),0)));
			 -- Bring the edges into cyclic order
			 oedges := {(ledges#0,1)};
			 v := ledges#0#1;
			 ledges = drop(ledges,1);
			 while (ledges != {}) do (
			      w := v;
			      e := ledges#0;
			      ledges = drop(ledges,1);
			      if (e#0 == v) then (
				   w = e#1;
				   oedges = append(oedges,(e,1)))
			      else if (e#1 == v) then (
				   w = e#0;
				   oedges = append(oedges,(e,-1)))
			      else (
				   ledges = append(ledges,e));
			      v = w);
			 M := map(QQ^d,QQ^(numColumns condmatrix),0);
			 -- for the cyclic order in oedges add the corresponding edgedirections to condmatrix
			 scan(oedges, e -> (
				   ve := (e#0#1 - e#0#0)*(e#1);
				   j := position(edges, edge -> (edge == e#0));
				   M = M_{0..j-1} | ve | M_{j+1..(numColumns M)-1}));
			 condmatrix = condmatrix || M));
	       -- if there are no conditions then the polyhedron has no compact 2 faces
	       if (condmatrix == map(QQ^0,QQ^0,0)) then (
		    -- collect the compact edges
		    LL := select(faces(dim(P)-1,P), fLL ->(isCompact(fLL)));
		    -- if there is only none or one compact edge then the only summand is the polyhedron itself
		    if (#LL == 0) or (#LL == 1) then (
			 (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}}))
		    -- otherwise we get a summand for each compact edge
		    else (
			 TailCLL := rays(P);
			 if (linSpace(P) != 0) then ( TailCLL = TailCLL | linSpace(P) | (-1)*(linSpace(P)));
			 (posHull map(QQ^(#LL),QQ^(#LL),1),hashTable apply(#LL, i -> (l := LL#i; i => convexHull(vertices(l),TailCLL))),matrix toList(#LL:{1_QQ}))))
	       -- Otherwise we can compute the Minkowski summand cone
	       else (
		    Id := map(QQ^(numColumns condmatrix),QQ^(numColumns condmatrix),1);
		    C := intersection(Id,condmatrix);
		    R := rays(C);
		    TC := (map(QQ^(P#"ambientDimension"),QQ^1,0)) | (P#"rays") | (P#"linealitySpace") | ((-1)*(P#"linealitySpace"));
		    v = (vertices(P))_{0};
		    -- computing the actual summands
		    summList := hashTable apply(numColumns R, i -> (
			      remedges := edges;
			      -- recursive function which takes 'L' the already computed vertices of the summandpolyhedron,
			      -- the set of remaining edges, the current vertex of the original polyhedron, the current 
			      -- vertex of the summandpolyhedron, and the ray of the minkSummandCone. It computes the
			      -- edges emanating from the vertex, scales these edges by the corresponding factor in mi, 
			      -- computes the vertices at the end of those edges (for the original and for the 
			      -- summandpolyhedron) and calls itself with each of the new vertices, if there are edges 
			      -- left in the list
			      edgesearch := (L,v,v0,mi) -> (
				   nedges := {};
				   Lnew := {};
				   scan(remedges, e -> (
					     j := position(edges, edge -> (edge == e));
					     if member(v,e) then (
						  ve := e#0 + e#1;
						  edir := ve - 2*v;
						  vnew := v0 + (((entries(mi))#j#0)*edir);
						  if (vnew != v0) then (L = L | vnew);
						  Lnew = append(Lnew,(v+edir,vnew)))
					     else (nedges = append(nedges,e))));
				   remedges = nedges;
				   scan(Lnew, (u,w) -> (
					     if (remedges =!= {}) then (
						  L = edgesearch(L,u,w,mi))));
				   L);
			      mi := R_{i};
			      v0 := map(QQ^d,QQ^1,0);
			      L := v0;
			      -- Calling the edgesearch function to get the vertices of the summand
			      L = edgesearch(L,v,v0,mi);
			      i => convexHull(L,TC)));
		    -- computing the inclusion minimal decompositions
		     onevec := matrix toList(numgens target R: {1_QQ});
		     negId := map(QQ^(numgens source R),QQ^(numgens source R),-1);
		     zerovec := matrix toList(numgens source R: {0_QQ});
		     Q := intersection(negId,zerovec,R,onevec);
		     (C,summList,vertices(Q))))))
     
     


--oldminkSummandCone = method()
--oldminkSummandCone Polyhedron := P -> (
--     d := P#"ambientDimension";
--     dP := P#"polyhedronDimension";
--     -- Saving the compact edges of 'P' in the list 'edges' and the "normed" pair of vertices in the list 'Kanten'
--     edges := {};
--     scan(faces(dP-1,P), e -> (
--	       if (isCompact(e)) then (edges=append(edges,e))));
--     -- Subfunction to save the two vertices of a compact edge in a matrix where the vertex with the smaller entries comes first
--     -- by comparing the two vertices entry-wise
--     normvert := M -> ( v:= M_{0}-M_{1};
--         normrec := w -> ( local i; if ((entries(w))#0#0 > 0) then (i=0) else if ((entries(w))#0#0 < 0) then (i=1) else (w=w^{1..(numgens target w)-1}; i=normrec(w)); i);
--          i=normrec(v);
--        if (i==1) then (M=M_{1}|M_{0});
--        M);
--     Kanten = apply(edges, k -> (normvert(vertices(k))));
--     -- Saving the compact two dimensional faces of P in the list twofaces
--     twofaces = {};
--     scan(faces(dP-2,P), f -> (
--	       if (isCompact(f)) then (twofaces=append(twofaces,f))));
--     m:=0;
--     -- Generating a hashTable that enumerates the edges in Kanten
--     KantenNr:={};
--     scan(Kanten, k -> (KantenNr = append(KantenNr, k => m); m=m+1));
--     KantenNr = hashTable KantenNr;
--     n := #Kanten;
--     zeromap := map(QQ^d,QQ^1,0);
--     Nfertig := map(QQ^0,QQ^n,0);
--     -- Determining the equations on the variables for the edges given by the two 
--     -- dimensional faces
--     scan(twofaces, f -> (
--	       j := 0;
--	       ordering := {};
--               -- Saving the edges of the active twoface
--	       edgesf := faces(1,f);
--               -- Selecting the first edge and removing it from the list
--	       k := edgesf#0;
--               edgesf = edgesf_{1..(#edgesf)-1};
--               -- Saving the the number of the selected edge from the hashTable KantenNr in a
--	       -- list to order the edges of the active twoface
--	       ordering = append(ordering, KantenNr#(normvert(vertices(k))) => j);
--	       j = j+1;
--	       -- Saving the two vertices of the edge
--	       M := (vertices(k))_{0};
--	       v := (vertices(k))_{1};
--	       -- Scanning through the remaining edges to find the order in which they appear in the active twoface
--	       scan(#edgesf, i -> (nedges := {};
--			 scan(edgesf, e -> (
--				   if contains(e,convexHull(v)) then (
--					k = e;
--					ordering = append(ordering, KantenNr#(normvert(vertices(k))) => j);
--					j = j+1)
--				   else (nedges =append(nedges,e))));
--			 edgesf = nedges;
--			 M = M|v;
--			 vnew := v;
--			 scan(numgens(source(vertices(k))), l -> ( c := (vertices(k))_{l}; 
--				   if (c =!= v) then (vnew = c)));
--			 v = vnew));
--	       -- The matrix M contains all vertices of the active twoface ordered in a "circle"
--	       -- now we take the differences from one colummn with the previous to get edges of 
--	       -- the active two face ordered in a "circle" which gives the equations for the 
--	       -- variables corresponding to these edges
--	       M = (M_{1..(numgens source M)-1} | M_{0}) - M;
--	       -- Turn ordering into a hashTable to find out to which edges the columns in M
--	       -- correspond 
--	       ordering = hashTable ordering;
--	       -- Generating a new set of rows for the final equationmatrix for the cone 
--	       -- by adding zerocolumns into M for those edges not appearing in the 
--	       -- active twoface
--	       L := {};
--	       scan(#Kanten, i -> (
--			 column := zeromap;
--			 scan(keys ordering, ko -> (
--				   if (ko == i) then (column = M_{ordering#ko})));
--			 L = append(L, i => column)));
--	       L = hashTable L;
--	       N := map(QQ^d,QQ^0,0);
--	       scan(#Kanten, l -> ( N = N |(L#l)));
--	       Nfertig = Nfertig || N));
--     -- constructing the Cone by intersecting the equations with the positive orthant
--     Id := map(QQ^n,QQ^n,1);
--     C := intersection(Id,Nfertig);
--     -- Selecting a vertex of P
--     v := (vertices(P))_{0};
--     -- Saving the generators of the cone
--     M := rays C;
--     -- Saving the generators of the tailcone in a matrix
--     TC := (map(QQ^(P#"ambientDimension"),QQ^1,0)) | (P#"rays") | (P#"linealitySpace") | ((-1)*(P#"linealitySpace"));
--     summList := {};
--     -- Computing for each generator of the cone the corresponding summand polyhedron, where the first vertex 
--     -- is the origin
--     scan(numgens source M, i -> (
--	       remedges := edges;
--	       -- recursive function which takes 'L' the already computed vertices of the summandpolyhedron,
--	       -- the set of remaining edges, the current vertex of the original polyhedron, the current 
--	       -- vertex of the summandpolyhedron, and the ray of the minkSummandCone. It computes the
--	       -- edges emanating from the vertex, scales these edges by the corresponding factor in mi, 
--	       -- computes the vertices at the end of those edges (for the original and for the 
--	       -- summandpolyhedron) and calls itself with each of the new vertices, if there are edges 
--	       -- left in the list
--	       edgesearch := (L,v,v0,mi) -> (
--		    nedges := {};
--		    Lnew := {};
--		    scan(remedges, e -> (
--			      j := KantenNr#(normvert(vertices(e)));
--			      if (contains(e,v)) then (
--				   ve := vertices(e);
--				   edir := (ve*(matrix{{1_QQ},{1_QQ}})) - 2*v;
--				   vnew := v0 + (((entries(mi))#j#0)*edir);
--				   L = L | vnew;
--				   Lnew = append(Lnew,(v+edir,vnew)))
--			      else (nedges = append(nedges,e))));
--		     remedges = nedges;
--		     scan(Lnew, (u,w) -> (
--			       if (remedges =!= {}) then (
--				    L = edgesearch(L,u,w,mi))));
--		     L);
--		mi := M_{i};
--		v0 := map(QQ^d,QQ^1,0);
--		L := v0;
--		-- Calling the edgesearch function to get the vertices of the summand
--		L = edgesearch(L,v,v0,mi);
--		summList = append(summList, i => convexHull(L,TC))));
--      summList = hashTable summList;
--      onevec := matrix toList(numgens target M: {1_QQ});
--      negId := map(QQ^(numgens source M),QQ^(numgens source M),-1);
--      zerovec := matrix toList(numgens source M: {0_QQ});
--      Q := intersection(negId,zerovec,M,onevec);
--      (C,summList,vertices(Q)))
 


-- PURPOSE : Computing the 'n'-skeleton of a fan
--   INPUT : (n,F),  where 'n' is a positive integer and
--                   'F' is a Fan
--  OUTPUT : the Fan consisting of the 'n' dimensional cones in 'F'
skeleton = method(TypicalValue => Fan)
skeleton(ZZ,Fan) := (n,F) -> (
     -- Checking for input errors
     if (n < 0) or (dim(F) < n) then (
	  error ("The integer must be between 0 and dim(F)"));
     makeFan(cones(n,F)))


-- PURPOSE : Computing the smallest face of 'P' containing 'p'
--   INPUT : '(p,P)',  where 'p' is a point given as a matrix and
--     	    	       'P' is a polyhedron
--  OUTPUT : The smallest face containing 'p' as a polyhedron
smallestFace = method()
smallestFace(Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if ((numgens source p) =!= 1) or ((numgens target p) =!= (P#"ambientDimension")) then (
	  error ("The point must lie in the same space"));
     local ploc;
     R := ring source p;
     if (R === ZZ) then (
     	  ploc = substitute(p, QQ))
     else if (R === QQ) then (
	  ploc = p)
	else error ("expected halfspaces over ZZ or QQ");
     -- Checking if 'P' contains 'p' at all
     if (contains(P,convexHull(ploc))) then (
	  (M,v) := halfspaces P;
     	  (N,w) := hyperplanes P;
     	  -- Selecting the halfspaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  scan(numgens target M, i -> (
		    if ((M^{i})*ploc == v^{i}) then (
			 N = N||(M^{i});
			 w = w||(v^{i}))));
	  intersection(M,v,N,w))
     else (
	  emptyPolyhedron(P#"ambientDimension"))
     )



--   INPUT : '(p,C)',  where 'p' is point given as a matrix and
--     	    	       'C' is a Cone
--  OUTPUT : The smallest face containing 'p' as a cone
smallestFace(Matrix,Cone) := (p,C) -> (
     -- Checking for input errors
     if ((numgens source p) =!= 1) or ((numgens target p) =!= (C#"ambientDimension")) then (
	  error ("The point must lie in the same space"));
     local ploc;
     R := ring source p;
     if (R === ZZ) then (
     	  ploc = substitute(p, QQ))
     else if (R === QQ) then (
	  ploc = p)
	else error ("expected halfspaces over ZZ or QQ");
     -- Checking if 'C' contains 'p' at all
     if (contains(C,posHull(ploc))) then (
	  M := halfspaces C;
     	  N := hyperplanes C;
     	  -- Selecting the halfspaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  scan(numgens target M, i -> (
		    if ((M^{i})*ploc == 0) then (
			 N = N||(M^{i}))));
	  intersection(M,N))
     else (
	  emptyPolyhedron(C#"ambientDimension"))
     )



-- PURPOSE : Computing the subfan of all smooth cones of the Fan
--   INPUT : 'F',  a Fan
--  OUTPUT : The Fan of smooth cones
smoothSubfan = method(TypicalValue => Fan)
smoothSubfan Fan := F -> (
     -- recursive function that adds the cones of the list 'L' to 'F' if they are smooth
     -- and calls itself with the faces of the cone if the cone is not smooth
     facerecursion := (L,F) -> (
	  scan(L, C -> (
		    if (isSmooth(C)) then (
			 F = unique append(F,C))
		    else (
			 L' := faces(1,C);
			 F = facerecursion(L',F))));
	  F);
     L := F#"generatingCones";
     F = {};
     F = facerecursion(L,F);
     makeFan F)



-- PURPOSE : Computing the tail cone of a given Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : The Cone generated by the rays and the lineality space of 'P'
tailCone = method(TypicalValue => Cone)
tailCone Polyhedron := P -> (
     posHull(P#"rays",P#"linealitySpace"))



-- PURPOSE : Computing the vertex-edge-matrix of a polyhedron
--   INPUT : 'P',  a polyhedron
--  OUTPUT : a matrix, where the columns are indexed by the edges and the rows indexed by the vertices and has 1 as entry
--           if the corresponding edge contains this vertex
vertexEdgeMatrix = method(TypicalValue => Matrix)
vertexEdgeMatrix Polyhedron := P -> (
     -- list the edges and the vertices
     eP := apply(faces(dim(P)-1,P),vertices);
     vp := vertices(P);
     vList := hashTable apply(numgens source vp, i -> ( vp_{i} => i+1));
     d := #vList;
     n := #eP;
     -- Generate the matrix with indeces in the first row and column
     M := (transpose matrix {toList(0..d)}) | ( (matrix {toList(1..n)}) || (matrix toList(d:toList(n:0))));
     -- For every edge add two 1's in the corresponding column
     scan(#eP, i -> (
	       j := vList#((eP#i)_{0});
	       M = M_{0..i} | (M_{i+1}^{0..j-1} || matrix {{1}} || M_{i+1}^{j+1..d}) | M_{i+2..n};
	       j = vList#((eP#i)_{1});
	       M = M_{0..i} | (M_{i+1}^{0..j-1} || matrix {{1}} || M_{i+1}^{j+1..d}) | M_{i+2..n}));
     M);



-- PURPOSE : Computing the vertex-facet-matrix of a polyhedron
--   INPUT : 'P',  a polyhedron
--  OUTPUT : a matrix, where the columns are indexed by the facets and the rows are indexed by the vertices and has 1 as entry
--           if the corresponding facet contains this vertex
vertexFacetMatrix = method(TypicalValue => Matrix)
vertexFacetMatrix Polyhedron := P -> (
     -- list the facets and the vertices
     fP := apply(faces(1,P),vertices);
     vp := vertices(P);
     vList := hashTable apply(numgens source vp, i -> ( vp_{i} => i+1));
     d := #vList;
     n := #fP;
     -- Generate the matrix with indeces in the first row and column
     M := (transpose matrix {toList(0..d)}) | ( (matrix {toList(1..n)}) || (matrix toList(d:toList(n:0))));
     -- For every facet add 1's in the corresponding column
     scan(#fP, i -> (
	       scan(numgens source (fP#i), k -> (
			 j := vList#((fP#i)_{k});
			 M = M_{0..i} | (M_{i+1}^{0..j-1} || matrix {{1}} || M_{i+1}^{j+1..d}) | M_{i+2..n}))));
     M);



-- PURPOSE : Computing the affine hull
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : the affine hull of 'P' as a Polyhedron
affineHull = method(TypicalValue => Polyhedron)
affineHull Polyhedron := P -> (
     M := vertices P;
     -- subtracting the first vertex from all other vertices
     N := (M+M_{0}*(matrix {toList(numgens source M:-1)}))_{1..(numgens source M)-1};
     convexHull(M_{0},(N | (-1)*N)));


-- PURPOSE : Computing the affine image of a polyhedron
affineImage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,v)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space and 'v' is a matrix
--     	    	      	 that gives a vector in the target space of 'A'
--  OUTPUT : The polyhedron which is the affine image of 'P':
--                       A*P+v={A*p+v | p in P}
affineImage(Matrix,Polyhedron,Matrix) := (A,P,v) -> (
     -- Checking for input errors
     local Al,vl;
     R := ring source A;
     if (R === ZZ) then (
	  Al = substitute(A, QQ))
     else if (R === QQ) then (
	  Al = A)
     else error ("Expected matrix over ZZ or QQ");
     R = ring source v;
     if (R === ZZ) then (
	  vl = substitute(v, QQ))
     else if (R === QQ) then (
	  vl = v)
     else error ("expected translation vector over ZZ or QQ");
     A = Al;
     v = vl;
     if (( P#"ambientDimension") =!= (numgens source A)) then (
	  error ("Matrix source must be ambientspace"));
     if ((numgens target A) =!= (numgens target v)) then (
	  error ("Vector must lie in target space of matrix"));
     if ((numgens source v) =!= 1) then (
	  error ("Second argument must be a vector"));
     -- Generating nr of vertices many copies of v
     v = v * (matrix {toList(P#"numVertices":1_QQ)});
     Mv := (A*(vertices P)) + v;
     Mr := A*(rays P);
     if ((numgens source Mr) == 0) then (Mr = matrix toList(numgens target Mv:{0_QQ}));
     convexHull(Mv,Mr))


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space
--  OUTPUT : a Polyhedron, which is the image of 'P' under 'A'
affineImage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     v := map(QQ^(numgens target A),QQ^1,0);
     affineImage(A,P,v))


--   INPUT : '(P,v)',  where 'v' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : a Polyhedron, which is the translation of 'P' by 'v', i.e. {p+v | p in P} 
affineImage(Polyhedron,Matrix) := (P,v) -> (
     -- Generating the identity matrix
     A := map(QQ^(P#"ambientDimension"),QQ^(P#"ambientDimension"),1);
     affineImage(A,P,v))


--   INPUT : '(M,C,v)',  where 'M' is a ZZ or QQ matrix from the ambient space of 
--     	    	      	 the cone 'C' to some target space and 'v' is a matrix
--     	    	      	 that gives a vector in that target space
--  OUTPUT : The polyhedron which is the affine image of 'C':
--                       (M*C)+v={(M*c)+v | c in C}
affineImage(Matrix,Cone,Matrix) := (M,C,v) -> (
     if (v == 0) then (
	  affineImage(M,C))
     else (
	  affineImage(M,coneToPolyhedron(C),v)))


--   INPUT : '(M,C)',  where 'M' is a ZZ or QQ matrix from the 
--     	    	      	 ambient space of the cone 'C' to some target space
--  OUTPUT : The cone which is the affine image of 'C':
--                       M*C={M*c | c in C}
affineImage(Matrix,Cone) := (M,C) -> (
     posHull(affineImage(M,coneToPolyhedron(C))))


--   INPUT : '(C,v)',  where 'C' is a cone and 'v' is a matrix
--     	    	      	 that gives a vector in the ambient space of 'C'
--  OUTPUT : The polyhedron which is the affine image of 'C':
--                       C+v={c+v | c in C}
affineImage(Cone,Matrix) := (C,v) -> (
     affineImage(coneToPolyhedron(C),v))


-- PURPOSE : Computing the affine preimage of a cone or polyhedron
affinePreimage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the polyhedron 'P' and 'b' is a matrix
--     	    	      	 that gives a vector in the ambient space of 'P'
--  OUTPUT : The polyhedron which is the affine preimage of 'P':
--                       {q | (A*q)+b in P}
affinePreimage(Matrix,Polyhedron,Matrix) := (A,P,b) -> (
     -- Checking for input errors
     local Al,bl;
     R := ring source A;
     if (R === ZZ) then (
	  Al = substitute(A, QQ))
     else if (R === QQ) then (
	  Al = A)
     else error ("Expected matrix over ZZ or QQ");
     R = ring source b;
     if (R === ZZ) then (
	  bl = substitute(b, QQ))
     else if (R === QQ) then (
	  bl = b)
     else error ("expected translation vector over ZZ or QQ");
     A = Al;
     b = bl;
     if (( P#"ambientDimension") =!= (numgens target A)) then (
	  error ("Matrix source must be ambientspace"));
     if ((numgens target A) =!= (numgens target b)) then (
	  error ("Vector must lie in target space of matrix"));
     if ((numgens source b) =!= 1) then (
	  error ("Second argument must be a vector"));
     -- Constructing the new halfspaces and hyperplanes
     (M,v) := halfspaces P;
     (N,w) := hyperplanes P;
     v = v - (M * b);
     w = w - (N * b);
     M = M * A;
     N = N * A;
     intersection(M,v,N,w))


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	       ambient space of the polyhedron 'P' 
affinePreimage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     b := map(QQ^(numgens target A),QQ^1,0);
     affinePreimage(A,P,b))


--   INPUT : '(P,b)',  where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : a Polyhedron, which is the negative translation of 'P' by 'b', i.e. {q | q+b in P} 
affinePreimage(Polyhedron,Matrix) := (P,b) -> (
     -- Generating the identity matrix
     A := map(QQ^(P#"ambientDimension"),QQ^(P#"ambientDimension"),1);
     affinePreimage(A,P,b))

--   INPUT : '(A,C,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C' and 'b' is a matrix
--     	    	      	 that gives a vector in the ambient space of 'C'
--  OUTPUT : The polyhedron which is the affine preimage of 'C':
--                       {q | (A*q)+b in C}
--     	     or the cone which is the affine preimage of 'C' if 'b' is 0:
--     	    	         {q | (A*q) in C}
affinePreimage(Matrix,Cone,Matrix) := (A,C,b) -> (
     if (b == 0) then (
	  affinePreimage(A,C))
     else (
	  affinePreimage(A,coneToPolyhedron(C),b)))


--   INPUT : '(A,C)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C'
--  OUTPUT : The cone which is the affine preimage of 'C':
--                       {q | (A*q) in C}
affinePreimage(Matrix,Cone) := (A,C) -> (
     posHull(affinePreimage(A,coneToPolyhedron(C))))


--   INPUT : '(C,b)',   where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the cone 'C'
--  OUTPUT : The polyhedron which is the affine preimage of 'C':
--                       {q | q+b in C}
affinePreimage(Cone,Matrix) := (C,b) -> (
     affinePreimage(coneToPolyhedron(C),b))


-- PURPOSE : Computing the bipyramid over the polyhedron 'P'
--   INPUT : 'P',  a polyhedron 
--  OUTPUT : The polyhedron which is the convex hull of 'P', embedded into ambientdim+1 space and the 
--     	         points (barycenter of 'P',+-1)
bipyramid = method(TypicalValue => Polyhedron)
bipyramid Polyhedron := P -> (
     -- Saving the vertices
     V := vertices P;
     n := numgens source V;
     if (n == 0) then (
	  error ("P must not be empty"));
     -- Computing the barycenter of P
     v := matrix toList(n:{1_QQ,1_QQ});
     v = (1/n)*V*v;
     (M,LS) := P#"homogenizedVertices";
     -- Embedding into n+1 space and adding the two new vertices
     zerorow := map(QQ^1,QQ^(numgens source M),0);
     newvertices := (matrix {{1_QQ,1_QQ}}) || v || (matrix {{1_QQ,-(1_QQ)}});
     M = (M || zerorow) | newvertices;
     LS = LS || (map(QQ^1,QQ^(numgens source LS),0));
     hyperA := fourierMotzkin(M,LS);
     verticesA := fourierMotzkin hyperA;
     polyhedronBuilder(hyperA,verticesA))


-- PURPOSE : Computes the coarsest common refinement of a given set of rays
--   INPUT : 'M'  a Matrix
--  OUTPUT : 'F' the fan which is the coarsest common refinement
ccRefinement = method(TypicalValue => Fan)
ccRefinement Matrix := M -> (
     -- Checking for input errors
     local Ml;
     R := ring source M;
     if (R === ZZ) then (
	  Ml = substitute(M, QQ))
     else if (R === QQ) then (
	  Ml = M)
     else error ("Expected matrix over ZZ or QQ");
     M = Ml;
     -- Extracting data
     n := numRows M;
     m := numColumns M;
     -- Generating all cones generated by 'n' rays in 'M'
     nCones := apply(subsets(m,n), e -> (posHull(M_e)));
     -- Selecting those cones that 'n' dimensional and do not contain any 
     -- of the others
     nConesfd := select(nCones, C -> (dim(C) == n));
     nConesfd = inclMinCones(nConesfd);
     refCones := {};
     added := true;
     while added do (
	  added = false;
	  newCones := {};
	  -- scan through the 'n' dimensional cones and check for each of the cones generated by
	  -- 'n' rays if their intersection is 'n' dimensional and if the first one is not contained 
	  -- in the latter. If true, then their intersection will be saved in the list 'newCones'.
	  -- If false for every cone generated by 'n' rays, then the 'n' dimensional cone will be 
	  -- appended to the list 'refCones'
	  scan(#nConesfd, i -> (
		    keep := true;
		    scan(#nCones, j -> (
			      C := intersection(nCones#j,nConesfd#i);
			      if ((dim(C) == n) and (not contains(nCones#j,nConesfd#i))) then (
				   added = true;
				   keep = false;
				   newCones = append(newCones,C))));
		    if keep then (
			 refCones = append(refCones,nConesfd#i))));
	  -- now, the new intersections will be the 'n' dimensional cones and the same procedure 
	  -- starts over again if this list is not empty
	  nConesfd = unique(newCones));
     -- Compute the fan generated by the 'refCones'
     makeFan(refCones));
      



-- PURPOSE : Converts the Cone 'C' into itself as a Polyhedron 'P'
--   INPUT : 'C'  a Cone
--  OUTPUT : 'P' the cone saved as a polyhedron
coneToPolyhedron = method(TypicalValue => Polyhedron)
coneToPolyhedron(Cone) := C -> (
     M := map(QQ^(C#"ambientDimension"),QQ^1,0);
     N := rays(C);
     convexHull(M,N))



-- PURPOSE : Computing the direct product of two polyhedra in the direct product of their ambient spaces
directProduct = method()

--   INPUT : '(P,Q)',  two polyhedra
--  OUTPUT : A polyhedron which is the direct product
directProduct (Polyhedron,Polyhedron) := (P,Q) -> (
     -- Extracting halfspaces and hyperplanes of P and Q
     (Mp,vp) := halfspaces(P);
     (Np,wp) := hyperplanes(P);
     (Mq,vq) := halfspaces(Q);
     (Nq,wq) := hyperplanes(Q);
     -- Constructing the new halfspaces matrix |Mp 0 | and vector |vp|
     --                                        |0  Mq|            |vq|
     M := Mp | ( map(QQ^(numgens target Mp),QQ^(numgens source Mq),0));
     M = M || ( (map(QQ^(numgens target Mq),QQ^(numgens source Mp),0)) | Mq);
     v := vp || vq;
     -- Constructing the new hyperplanes matrix |Np 0 | and vector |wp|
     --                                         |0  Nq|            |wq|
     N := Np | ( map(QQ^(numgens target Np),QQ^(numgens source Nq),0));
     N = N || ( (map(QQ^(numgens target Nq),QQ^(numgens source Np),0)) | Nq);
     w := wp || wq;
     intersection(M,v,N,w))


--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : A cone which is the direct product
directProduct (Cone,Cone) := (C1,C2) -> (
     -- Extracting halfspaces and hyperplanes of P and Q
     Mp := halfspaces(C1);
     Np := hyperplanes(C1);
     Mq := halfspaces(C2);
     Nq := hyperplanes(C2);
     -- Constructing the new halfspaces matrix |Mp 0 |
     --                                        |0  Mq|
     M := Mp | ( map(QQ^(numgens target Mp),QQ^(numgens source Mq),0));
     M = M || ( (map(QQ^(numgens target Mq),QQ^(numgens source Mp),0)) | Mq);
     -- Constructing the new hyperplanes matrix |Np 0 |
     --                                         |0  Nq|
     N := Np | ( map(QQ^(numgens target Np),QQ^(numgens source Nq),0));
     N = N || ( (map(QQ^(numgens target Nq),QQ^(numgens source Np),0)) | Nq);
     intersection(M,N))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : A polyhedron which is the direct product
directProduct (Cone,Polyhedron) := (C,P) -> (
     -- Converting the Cone into a Polyhedron
     Q := coneToPolyhedron C;
     directProduct(Q,P))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : A polyhedron which is the direct product
directProduct (Polyhedron,Cone) := (P,C) -> (
     -- Converting the Cone into a Polyhedron
     Q := coneToPolyhedron C;
     directProduct(P,Q))


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : A fan which is the direct product
directProduct (Fan,Fan) := (F1,F2) -> (
     -- computing the direct products of all pairs of generating cones
     L := {};
     scan((F1#"generatingCones"), C1 -> (
	       scan((F2#"generatingCones"), C2 -> (
			 L = append(L,directProduct(C1,C2))))));
     makeFan L)



Polyhedron * Polyhedron := (P1,P2) -> directProduct(P1,P2)
Polyhedron * Cone := (P,C) -> directProduct(P,C)
Cone * Polyhedron := (C,P) -> directProduct(C,P)
Cone * Cone := (C1,C2) -> directProduct(C1,C2)
Fan * Fan := (F1,F2) -> directProduct(F1,F2)


-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
	genrays := (transpose(C#"halfspaces"),transpose(C#"hyperplanes"));
	dualgens := ((-1)*(C#"rays"),C#"linealitySpace");
	coneBuilder(genrays,dualgens))
   
   
-- PURPOSE : Computing the image fan of a cone
--   INPUT : '(M,C)',  a Matrix 'M' and a Cone 'C'
--  OUTPUT : The Fan which is the common refinement of the images of all faces of
--     	     'C' under 'M'
imageFan = method(TypicalValue => Fan)
imageFan (Matrix,Cone) := (M,C) -> (
     local Ml;
     R := ring source M;
     if (R === ZZ) then (
	  Ml = substitute(M, QQ))
     else if (R === QQ) then (
	  Ml = M)
     else error ("Expected matrix over ZZ or QQ");
     if (numColumns(M) != ambDim(C)) then (
	  error ("The source space of the matrix must be the ambient space of the cone"));
     M = Ml;
     -- Extracting data
     m := numRows M;
     n := dim(C);
     -- Compute the images of all 'm' dimensional faces and select those that are again 
     -- 'm' dimensional
     L := apply(faces(n-m,C), e -> (affineImage(M,e)));
     L = select(L, e -> (dim(e) == m));
     print(L);
     -- Compute their common refinement
     refineCones(L));
     


-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambientspace
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     -- Checking for input errors
     if (P1#"ambientDimension" =!= P2#"ambientDimension") then (
	  error ("Polyhedra must lie in the same space"));
     -- Saving the vertices and rays
     V1 := vertices P1;
     V2 := vertices P2;
     R := (rays P1)|(rays P2)|(matrix toList(numgens target V1:{0_QQ}));
     Vnew := map(QQ^(numgens target V1),QQ^0,0);
     -- Collecting all sums of vertices of P1 with vertices of P2
     scan(numgens source V1, i -> ( 
	       scan(numgens source V2, j -> (
			 Vnew = Vnew | (V1_{i}+V2_{j})))));
     convexHull(Vnew,R))


--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : The Minkowskisum as a cone
minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if (C1#"ambientDimension" =!= C2#"ambientDimension") then (
	  error ("Cones must lie in the same space"));
     -- Saving the vertices and rays
     R := (C1#"rays")|(C2#"rays");
     LS := (C1#"linealitySpace")|(C2#"linealitySpace");
     posHull(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
     -- Checking for input errors
     if (C#"ambientDimension" =!= P#"ambientDimension") then (
	  error ("Cone and polyhedron must lie in the same space"));
     -- Saving the vertices and rays
     V := P#"vertices";
     R := (P#"rays")|(C#"rays")|(C#"linealitySpace")|((-1)*(C#"linealitySpace"));
     convexHull(V,R))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
     -- Checking for input errors
     if (C#"ambientDimension" =!= P#"ambientDimension") then (
	  error ("Cone and polyhedron must lie in the same space"));
     -- Saving the vertices and rays
     V := P#"vertices";
     R := (P#"rays")|(C#"rays")|(C#"linealitySpace")|((-1)*(C#"linealitySpace"));
     convexHull(V,R))



Polyhedron + Polyhedron := (P1,P2) -> minkowskiSum(P1,P2)
Polyhedron + Cone := (P,C) -> minkowskiSum(P,C)
Cone + Polyhedron := (C,P) -> minkowskiSum(P,C)
Cone + Cone := (C1,C2) -> minkowskiSum(C1,C2)


-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, which is the outer normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
     -- Saving the vertices
     vm := vertices P;
     L := {};
     -- For every vertex translate P by -this vertex and take the dual cone of the positive hull of it
     scan(numgens source vm, i -> (
	       Q := affineImage(P,(-1)*(vm_{i}));
	       L = append(L,dualCone(posHull(Q)))));
     makeFan(L))



-- PURPOSE : Computing the polar of a given polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : a Polyhedron, which is the set { v | v*p<=1 forall p in P}
polar = method(TypicalValue => Polyhedron)
polar Polyhedron := P -> (
     d := P#"ambientDimension";
     -- Make the 'd'-dimensional identity
     M := map(QQ^d,QQ^d,-1);
     -- make the block matrix of -1 and the 'd'identity
     M = (matrix{{-1_QQ}} | map(QQ^1,QQ^d,0))||(map(QQ^d,QQ^1,0) | M);
     hyperA := P#"homogenizedVertices";
     hyperA = (M*(hyperA#0),hyperA#1);
     verticesA := fourierMotzkin hyperA;
     polyhedronBuilder(hyperA,verticesA))


-- PURPOSE : Computing the pyramid over the polyhedron 'P'
--   INPUT : 'P',  a polyhedron 
--  OUTPUT : The polyhedron which is the convex hull of 'P', embedded into ambientdim+1 space and the 
--     	         point (0,...,0,1)
pyramid = method(TypicalValue => Polyhedron)
pyramid Polyhedron := P -> (
     (M,LS) := P#"homogenizedVertices";
     -- Embedding into n+1 space and adding the new vertex
     zerorow := map(QQ^1,QQ^(numgens source M),0);
     newvertex := (matrix {{1_QQ}}) || map(QQ^((numgens target M)-1),QQ^1,0) || (matrix {{1_QQ}});
     M = (M || zerorow) | newvertex;
     LS = LS || (map(QQ^1,QQ^(numgens source LS),0));
     hyperA := fourierMotzkin(M,LS);
     verticesA := fourierMotzkin hyperA;
     polyhedronBuilder(hyperA,verticesA))


-- PURPOSE : Generating the 'd'-dimensional crosspolytope with edge length 2*'s'
crossPolytope = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a strictly positive rational number which is the distance of the vertices to the
--     	    	       origin
--  OUTPUT : The 'd'-dimensional crosspolytope with vertex-origin distance 's'
crossPolytope(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if (d < 1) then (
	  error ("dimension must at least be 1"));
     if (s <= 0) then (
	  error ("size of the crosspolytope must be positive"));
     M := map(QQ^d,QQ^d,s) | map(QQ^d,QQ^d,-s);
     convexHull M)


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a strictly positive integer which is the distance of the vertices to the origin
crossPolytope(ZZ,ZZ) := (d,s) -> (
     s = substitute(s,QQ);
     crossPolytope(d,s))


--   INPUT :  'd',  where 'd' is a strictly positive integer which gives the dimension of the polytope
crossPolytope ZZ := d -> (
     s := 1_QQ;
     crossPolytope(d,s))



-- PURPOSE : Computing the cyclic polytope of n points in QQ^d
--   INPUT : '(d,n)',  two positive integers
--  OUTPUT : The polyhedron which is the convex hull of 'n' points on the moment curve in 'd' space 
-- COMMENT : The moment curve is defined by t -> (t,t^2,...,t^d) in QQ^d, if we say we take 'n' points 
--            on the moment curve, we mean the images of 0,...,n-1
cyclicPolytope = method(TypicalValue => Polyhedron)
cyclicPolytope(ZZ,ZZ) := (d,n) -> (
     -- Checking for input errors
     if (d < 1) then (
	  error ("The dimension must be positiv"));
     if (n < 1) then (
	  error ("There must be a positive number of points"));
     M := map(ZZ^d, ZZ^n, (i,j) -> j^(i+1));
     convexHull M)


-- PURPOSE : Generating the empty polyhedron in n space
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The empty polyhedron in 'n'-space
emptyPolyhedron = method(TypicalValue => Polyhedron)
emptyPolyhedron ZZ := n -> (
     -- Checking for input errors
     if (n < 1) then (
	  error ("The ambient dimension must be positive"));
     zeromap := map(QQ^n,QQ^0,0);
     verticesA := (zeromap,zeromap);
     hyperA := fourierMotzkin verticesA;
     polyhedronBuilder(hyperA,verticesA));



-- PURPOSE : Computing the cone of the Hirzebruch surface H_r
--   INPUT : 'r'  a positive integer
--  OUTPUT : The Hirzebruch surface H_r
hirzebruch = method(TypicalValue => Fan)
hirzebruch ZZ := (r) -> (
     -- Checking for input errors
     if (r < 0) then (
	  error ("Input must be a positive integer"));
     normalFan convexHull matrix {{0,1,0,1+r},{0,0,1,1}})



-- PURPOSE : Generating the 'd'-dimensional hypercube with edge length 2*'s'
hypercube = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a positive rational number which is half of the edge length
--  OUTPUT : The 'd'-dimensional hypercube with edge length 2*'s' as a polyhedron
hypercube(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if (d < 1) then (
	  error ("dimension must at least be 1"));
     if (s <= 0) then (
	  error ("size of the hypercube must be positive"));
     -- Generating halfspaces matrix and vector
     M := map(QQ^d,QQ^d,1);
     M = M || (-M);
     v :=  matrix toList(2*d:{s});
     intersection(M,v))


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a positive integer which is half of the edge length
hypercube(ZZ,ZZ) := (d,s) -> (
     s = substitute(s,QQ);
     hypercube(d,s))

     
--   INPUT : 'd',  is a strictly positive integer which gives the dimension of the polytope 
hypercube ZZ := d -> (
     -- Setting the edge length 1
     s := 1_QQ;
     hypercube(d,s))



-- PURPOSE : Computing the Newton polytope for a given polynomial
--   INPUT : 'p',  a RingElement
--  OUTPUT : A polyhedron which has the exponentvectors of the monomials of 'p' as vertices
newtonPolytope = method(TypicalValue => Polyhedron)
newtonPolytope RingElement := p -> (
     M := transpose matrix exponents p;
     convexHull M)


-- PURPOSE : Generating the positive orthant in n-space as a cone
--   INPUT : 'n",  a strictly positive integer
--  OUTPUT : The cone which is the positive orthant in n-space
posOrthant = method(TypicalValue => Cone)
posOrthant ZZ := n -> (
     M := map(QQ^n,QQ^n,1);
     posHull M)



-- PURPOSE : Computing the state polytope of the ideal 'I'
--   INPUT : 'I',  a homogeneous ideal with resect to some strictly psoitive grading
--  OUTPUT : The state polytope as a polyhedron
statePolytope = method(TypicalValue => Polyhedron)
statePolytope Ideal := I -> (
     -- Compute an interior vector of the cone 'C'
     intVec := C -> (
	  local v;
	  if ((numgens source rays C) == 0) then (v = map(QQ^(ambDim(C)),QQ^1,0))
	  else (
	       v=rays(C)*(matrix toList(numgens source rays C:{1})));
	  v);
     -- Check if there exists a strictly positive grading such that 'I' is homogeneous with
     -- respect to this grading
     homogeneityCheck := I -> (
	  -- Generate the matrix 'M' that spans the space of the differeneces of the 
	  -- exponent vectors of the generators of 'I'
	  L := flatten entries gens I;
	  lt := apply(L, leadTerm);
	  M := {};
	  scan(#L, i -> ( scan(exponents(L#i), e -> (M = append(M,flatten(exponents(lt#i))-e)))));
	  M = matrix M;
	  -- intersect the span of 'M' with the positive orthant
	  C := intersection(map(QQ^(numgens source M),QQ^(numgens source M),1),M);
	  -- Check if an interior vector is strictly positive
	  v := intVec(C);
	  homog := true;
	  scan(flatten entries v, e -> (homog = (homog and (e>0))));
	  (homog,v));
     -- Compute the Groebner cone
     gCone := (g,lt) -> (
	  -- for a given groebner basis compute the reduced Groebner basis
	  -- note: might be obsolete, but until now (Jan2009) groebner bases appear to be not reduced
	  reduceGB := g -> (
	       gl = flatten entries gens g;
	       L := {};
	       scan(gl, l -> (L=append(L,((l-leadTerm(l))% g)+leadTerm(l))));
	       L);
	  L := {};
	  g = reduceGB g;
	  -- collect the differences of the exponent vectors of the groebner basis
	  lt = flatten entries lt;
	  scan(#g, i -> ( scan(exponents(g#i), e -> (L = append(L,flatten(exponents(lt#i))-e)))));
	  -- intersect the differences
	  intersection matrix L);
     wLeadTerm := (w,I) -> (
	  -- Compute the Groebner basis and their leading terms of 'I' with respect to the weight 'w'
	  w = flatten entries w;
	  R := ring I;
	  --L := apply(gens R, degree);
	  --posVec := (transpose L)#0;
	  --m := min w;
	  --if (m < 0) then (
	  --     p := minPosition w;
	  --     m = m/(posVec#p);
	  --     w = w-(m*posVec));
	  leastcm := 1;
	  -- Resize w to a primitive vector in ZZ
	  scan(w, e -> (if (e != 0) then leastcm = ((denominator e)/(gcd((denominator e),leastcm)))*leastcm));
	  w = leastcm*w;
	  w = apply(w, e -> (substitute(e,ZZ)));
	  -- geneerate the new ring with weight 'w'
	  S := (coefficientRing R)[(gens R), MonomialOrder => {Weights => w}, Global => false];
	  f := map(S,R);
	  --f' := map(R,S);
	  -- map 'I' into 'S' and compute Groebner basis and leadterm
	  I' := f I;
	  g := gb I';
	  lt := leadTerm I';
	  gbRemove I';
	  --g = f' g;
	  --lt = f' lt;
	  (g,lt));
     -- computes the symmetric difference of the two lists
     sortIn := (L1,L2) -> (
	  L := L1;
	  scan(L2, l -> (
		    stopper := false;
		    i := 0;
		    while (not stopper) do (
			 if (i >= #L) then (
			      L = append(L,l);
			      stopper = true)
			 else if (((L#i)#0) == (l#0)) then (
			      L = drop(L,{i,i});
			      stopper = true)
			 else i = i+1)));
	  L);
     --Checking for homogeneity
     (noError,posv) := homogeneityCheck(I);
     if (not noError) then (
	  error ("The ideal must be homogeneous w.r.t. some strictly positive grading"));
     -- Compute a first Groebner basis to start with
     g := gb I;
     lt := leadTerm I;
     -- Compute the Groebner cone
     C := gCone(g,lt);
     gbRemove I;
     -- Generate all facets of 'C'
     facets := faces(1,C);
     -- Save each facet by an interior vector of it, the facet itself and the cone from 
     -- which it has been computed
     facets = apply(facets, f -> (intVec(f),f,C));
     --Save the leading terms as the first vertex
     verts := {lt};
     -- Scan the facets
     while (facets != {}) do (
	  local omega',f;
	  (omega',f,C) = facets#0;
	  -- compute an interior vector of the big cone 'C' and take a small 'eps'
	  omega := intVec C;
	  eps := 1/10;
	  stopper := false;
	  -- reduce 'eps' until the Groebner cone generated by omega'-(eps*omega) is 
	  -- adjacent to the big cone 'C'
	  while (not stopper) do (
	       omega1 := omega'-(eps*omega);
	       (g,lt) = wLeadTerm(omega1,I);
	       C' := gCone(g,lt);
	       if (equals(intersection(C,C'),f)) then (
		    C = C';
		    stopper = true)
	       else (eps = eps * 1/10));
	  -- save the new leadterms as a new vertex
	  verts = append(verts,lt);
	  -- Compute the facets of the new Groebner cone and save them in the same way as before
	  newfacets := faces(1,C);
	  newfacets = apply(newfacets, f -> (intVec(f),f,C));
	  -- Save the symmetric difference into 'facets'
	  facets = sortIn(facets,newfacets));
     posv = substitute(posv,ZZ);
     R := ring I;
     -- generate a new ring with the strictly positive grading computed by the homogeneity check
     S := QQ[(gens R), Degrees => (entries posv)];
     -- map the vertices into the new ring 'S'
     verts = apply(verts, el -> (T:=ring el; f := map(S,T); f el));
     -- Compute the maximal degree of the vertices
     L := flatten apply(verts, l -> (flatten entries l));
     d := (max apply(flatten L, degree))#0;
     vertmatrix := {};
     -- compute the vertices of the state polytope
     scan(verts, v -> (
	       VI := ideal flatten entries v;
	       SI := S/VI;
	       v = {};
	       scan(d, i -> ( v = join(v,flatten entries basis(i+1,SI))));
	       vertmatrix = append(vertmatrix,flatten sum apply(v,exponents))));
     -- Compute the state polytope
     P := convexHull transpose matrix vertmatrix;
     (verts,P));
	  
	  
-- PURPOSE : Generating the 'd'-dimensional standard simplex in QQ^(d+1)
--   INPUT : 'd',  a positive integer
--  OUTPUT : The 'd'-dimensional standard simplex as a polyhedron
stdSimplex = method(TypicalValue => Polyhedron)
stdSimplex ZZ := d -> (
     -- Checking for input errors
     if (d < 0) then (
	  error ("dimension must not be negative"));
     -- Generating the standard basis
     M := map(QQ^(d+1),QQ^(d+1),1);
     convexHull M)


-- PURPOSE : Saving the actual Session of Polyhedra (and PPDivisor)
--   INPUT : 'F',  a String, the filename
--  OUTPUT : The file F
--COMMENTS : This function saves not the complete Session, but it saves every convex polyhedral objects assigned to 
--     	     a Symbol, i.e. all Cones, Polyhedra, Fans as well as Matrices and if the additional package 
--     	     "PPDivisor" is loaded it saves also all PolyhedralDivisors. Furthermore all lists and sequences
--     	     that contain any of the types above (to arbitrary depth of lists and sequences) are also saved 
--     	     to the file. But keep in mind that this works only for such objects assigned to a Symbol! The session 
-- 	     can be reovered by calling
--     	     load F
-- 	     It is not neccessary to load Polyhedra before loading the saved session, because if not yet loaded it will
--     	     load Polyhedra. Also if PPDivisor was loaded when the session has been saved it will also be loaded.
saveSession = method()
saveSession String := F -> (
     -- Creating and opening the output file
     F = openOut(F);
     -- Make sure Polyhedra is loaded when the session is recovered
     F << "needsPackage \"Polyhedra\"" << endl;
     -- Check if PPDivisor has been loaded
     PPDivisorPackageLoaded :=  PackageDictionary#?"PPDivisor";
     if (PPDivisorPackageLoaded) then (
	  -- if so, make sure it will also be loaded when the session is recovered
	  F << "needsPackage \"PPDivisor\"" << endl);
     --Save all Matrices to the file
     L := userSymbols Matrix;
     scan(L, s -> (
	       M := value(s);
	       F << s << " = ";
	       F << putMatrix(M);
	       F << endl)); 
     -- Save all Polyhedra to the file
     L = userSymbols Polyhedron;
     scan(L, s -> (
	       P := value(s);
	       F << s << " = ";
	       F << putPolyhedron(P);
	       F << endl));
     -- Save all Cones to the file
     L = userSymbols Cone;
     scan(L, s -> (
	       C := value(s);
	       F << s << " = ";
	       F << putCone(C);
	       F << endl));
     -- Save all Fans to the file
     L = userSymbols Fan;
     scan(L, s -> (
	       Fa := value(s);
	       F << s << " = ";
	       F << putFan(Fa);
	       F << endl));
     -- if PPdivisor is loaded save all PolyhedralDivisors to the file
     if PPDivisorPackageLoaded then (
	  L = userSymbols ();
	  L = select(L, e -> ((toString class value e) == "PolyhedralDivisor"));
	  scan(L, s -> (
		    PD := value(s);
		    F << s << " = ";
		    F << putPolyhedralDivisor(PD);
		    F << endl)));
     -- Save all Lists and Sequences containing only convex polyhedral objects and/or lists of them to the file
     L = (userSymbols Sequence) | (userSymbols List);
     -- Auxiliary function to handle lists of lists
     listrec := method();
     listrec List := LS -> (
	  S := "";
	  i := 0;
	  while (S != "stop") and (i < #LS) do (
	       e := LS#i;
	       if (i > 0) then (
		    S = S | ",");
	       if instance(e,List) then (
		    S1 := listrec(toList e);
		    if (S1 == "stop") then (S = S1)
		    else (S = S | "{" | S1 | "}"))
	       else if instance(e,Sequence) then (
		    S2 := listrec(toList e);
		    if (S2 == "stop") then (S = S2)
		    else (S = S | "(" | S2 | ")"))
	       else if instance(e,Matrix) then (
		    S = S | putMatrix(e))
	       else if ((toString class e) == "Polyhedron") then (
		    S = S | putPolyhedron(e))
	       else if ((toString class e) == "Cone") then (
		    S = S | putCone(e))
	       else if ((toString class e) == "Fan") then (
		    S = S | putFan(e))
	       else if ((toString class e) == "PolyhedralDivisor") then (
		    S = S | putPolyhedralDivisor(e))
	       else ( S = "stop");
	       i = i+1);
	  S);
     listrec String := LS -> (
	  listrec(toList LS));
     scan(L, l -> (
	       l1 := toList value(l);
	       lstring := listrec(l1);
	       if (lstring != "stop") then (
		    if instance(value(l),List) then (
			 F << l << " = {" << lstring << "}" << endl)
		    else (
			 F << l << " = (" << lstring << ")" << endl))));
     -- close the file
     close F);



---------------------------------------
-- DECLARING AUXILIARY FUNCTIONS
-- >> not public <<
---------------------------------------

-- PURPOSE : select those cones in a list that do not contain any other cone of that list
--   INPUT : 'L',  a list of cones
--  OUTPUT : The list of cones that don't contain any of the other
inclMinCones = L -> (
     newL := {};
     -- Scanning the list
     while (L != {}) do (
	  C := L#0;
	  L = drop(L,1);
	  keep := true;
	  i := 0;
	  -- check, if 'C' contains any cone remaining in 'L'
	  while (keep and (i < #L)) do (
	       keep = not contains(C,L#i);
	       i = i+1);
	  -- if not, then check if 'C' contains any of the cones already in the final list
	  if keep then (
	       i = 0;
	       while (keep and (i < #newL)) do (
		    keep = not contains(C,newL#i);
		    i = i+1);
	       -- if not again, then add 'C' to the final list.
	       if keep then (
		    newL = append(newL,C))));
     newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections which
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of Sequences each containing a set of vertices and a set of rays giving the faces of a 
--     	    	   certain dimension of a polyhedron
--     	     'F', a list of Sequences each containing a set of vertices and a set of rays giving the facets 
--     	    	   of the same polyhedron
--  OUTPUT : a list of Sequences each containing a set of vertices and a set of rays giving the faces 
--     	    	   of the same polyhedron one dimension lower then the ones in 'L'
intersectionwithFacets = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> (
	       valid := false;
	       if (e#0 =!= set{}) then (
		    valid = (e =!= l));
	       valid);
	  newL:={};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := ( (l#0)*(f#0),(l#1)*(f#1));
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			      if isValid(e,l) then (
				   localL := {};
				   i := 0;
				   stopper := false;
				   while (not stopper) and (i < #newL) do (
					g := newL#i;
					if (isSubset(e#0,g#0)) and (isSubset(e#1,g#1)) then (
					     stopper = true)
					else if not ((isSubset(g#0,e#0)) and (isSubset(g#1,e#1))) then (
					     localL = append(localL,g));
					i = i+1);
				   if (not stopper) then (
					newL = append(localL,e)))))));
	  newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections which
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of sets each containing the rays of the faces of a certain dimension of a polyhedron
--     	     'F', a list of sets each containing the rays of the facets of the same polyhedron
--  OUTPUT : a list of sets each containing the rays of the faces of the same polyhedron one dimension lower 
--     	     then the ones in 'L'
intersectionwithFacetsCone = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> (
	       valid := false;
	       if (e =!= set{}) then (
		    valid = (e =!= l));
	       valid);
	  newL:={};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := l*f;
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			     if isValid(e,l) then (
				   localL := {};
				   i := 0;
				   stopper := false;
				   while (not stopper) and (i < #newL) do (
					g := newL#i;
					if (isSubset(e,g)) then (
					     stopper = true)
					else if not (isSubset(g,e)) then (
					     localL = append(localL,g));
					i = i+1);
				   if (not stopper) then (
					newL = append(localL,e)))))));
	  newL);


-- PURPOSE : Computes the common refinement of a list of cones
--   INPUT : 'L',  a list of cones
--  OUTPUT : The fan which is the common refinement of the cones
refineCones = L -> (
     -- Collecting the rays of all cones
     R := rays(L#0);
     n := numRows(R);
     R = apply(numColumns(R), i -> (R_{i}));
     L1 := drop(L,1);
     scan(L1, C -> (
	       RC := rays(C);
	       R = unique(R | apply(numColumns(RC), i -> (RC_{i})))));
     -- Writing the rays into one matrix
     M := map(QQ^n,QQ^0,0);
     scan(R, r -> (M = M|r));
     -- Compute the coarsest common refinement of these rays
     F := ccRefinement(M);
     -- Collect for each cone of the ccRef the intersection of all original cones, that contain
     -- the interior of that cone
     newCones := apply(genCones(F), C -> (
	       v := interiorVector(C);
	       Lint := select(L, c -> (contains(c,v)));
	       intersection(Lint)));
     makeFan(newCones));


-- PURPOSE : Write the Cone to a string so that reading the string generates the cone without calculating it
--   INPUT : 'C',  a Cone
--  OUTPUT : 'S',  a String, the command to generate the cone from its generators and dual generators
putCone = C -> (
     local n,B;
     genrays := C#"genrays";
     dualgens := C#"dualgens";
     S := "coneBuilder((";
     S = S | putMatrix(genrays#0) | "," | putMatrix(genrays#1) | "),(" | putMatrix(dualgens#0) | "," | putMatrix(dualgens#1) | "))";
     S);


-- PURPOSE : Write the Fan to a string so that reading the string generates the Fan without calculating it
--   INPUT : 'F',  a Fan
--  OUTPUT : 'S',  a String, the command to generate the Fan from its generating cones, rays, boundary faces and comperror
putFan = F -> (
     S := "fanBuilder({";
     L := F#"generatingCones";
     scan(#L, i -> (
	       C := L#i;
	       S = S | putCone(C);
	       if (i < #L-1) then (
		    S = S | ",")));
     S = S | "},{";
     L = F#"rays";
     scan(#L, i -> (
	       r := L#i;
	       S = S | putMatrix(r);
	       if (i < #L-1) then (
		    S = S | ",")));
     S = S | "},{";
     Ffaces := F#"faces";
     scan(#Ffaces, i -> (
	       C := Ffaces#i;
	       S = S | putCone(C);
	       if (i < #Ffaces-1) then (
		    S = S | ",")));
     S = S | "},";
     if (F#?"comperror") then (
	  S = S | "{";
     	  S = S | putCone((F#"comperror")#0);
     	  S = S | ",";
	  S = S | putCone((F#"comperror")#1);
     	  S = S | "})")
     else (
	  S = S | "{})");
     S);


-- PURPOSE : Write the Matrix to a string so that reading the string generates the Matrix again
--   INPUT : 'M',  a Matrix
--  OUTPUT : 'S',  a String, the command to generate the Matrix again over the same ring
putMatrix = M -> (
     local n;
     if (numColumns(M) == 0) then (
	  n = numRows(M);
	  "map(" | toString(ring(M)) | "^" | toString(n) | "," | toString(ring(M)) | "^0,0)")
     else if (numRows(M) == 0) then (
	  n = numColumns(M);
	  "map(" | toString(ring(M)) | "^0," | toString(ring(M)) | "^" | toString(n) | ",0)")
     else (
	  "substitute(matrix " | toString(entries(M)) | "," | toString(ring(M)) | ")"));



-- PURPOSE : Write the Polyhedron to a string so that reading the string generates the Polyhedron again without calculating it
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'S',  a String, the command to generate the Polyhedron again from its homogenized halfspaces and vertices
putPolyhedron = P -> (
     local n,B;
     hyperA := P#"homogenizedHalfspaces";
     verticesA := P#"homogenizedVertices";
     S := "polyhedronBuilder((" | putMatrix(hyperA#0) | "," | putMatrix(hyperA#1) | "),(" | putMatrix(verticesA#0) | "," | putMatrix(verticesA#1) | "))";
     S);



-- PURPOSE : Write the PolyhedralDivisor to a string so that reading the string generates the Polyhedraldivisor again without calculations
--   INPUT : 'PD',  a PolyhedralDivisor
--  OUTPUT : 'S',  a String, the command to generate the PolyhedralDivisor again from its fan, ppcoefficients and tailcone
putPolyhedralDivisor = PD -> (
     S := "makePolDiv(";
     S = S | putFan(PD#"fan");
     S = S | ",{";
     ppcoeff := PD#"polyhedralCoefficients";
     scan(#ppcoeff, i -> (
	       (M,P) := toSequence ppcoeff#i;
	       S = S | "{" | putMatrix(M) | "," | putPolyhedron(P) | "}";
	       if (i < #ppcoeff - 1) then (
		    S = S | ",")));
     S = S | "},";
     S = S | putCone(PD#"tailCone");
     S = S | ")";
     S)





---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     	Key => Polyhedra,
	Headline => "for computations with convex polyhedra, cones, and fans",
	
	"A rational convex ", TO Polyhedron, " is the intersection of finitely many affine halfspaces 
	over ", TO QQ, " or equivalently, the convex hull of a finite set of vertices and rays. 
	A rational convex polyhedral ", TO Cone, " is the intersection of finitely many linear halfspaces 
	over ", TO QQ, " or equivalently, the positive hull of a finite set of rays. A ", TO Fan, " is 
	a finite collection of cones such that for each cone all its faces are in the fan and for two cones 
	in the fan the intersection is a face of each.",
	
	PARA{}, TT "Polyhedra", " uses the ", TO FourierMotzkin, " package by ", 
	HREF("http://www.mast.queensu.ca/~ggsmith", "Gregory G. Smith"), ". Each polyhedron or cone is 
	saved in both descriptions and a fan is saved as the list of its generating cones.",
	
	PARA{}, "Here are some examples illustrating the main uses of this package.",
	UL {
	     {TO "Working with polyhedra"},
	     {TO "Working with cones"},
	     {TO "Working with fans"}
	     },
	
	PARA{}, "For an introduction to polyhedra and cones, we recommend ",
	HREF("http://www.math.tu-berlin.de/~ziegler/", "Gunter
	M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
	Texts in Mathematics 152, Springer-Verlag, New York, 1995."
	
	}
   
document {
     Key => "Working with polyhedra",
     
     "We start with a polyhedron in 2-space which is the ",TO convexHull," of a given set of points",
     
     EXAMPLE {
	  " V = matrix {{0,2,-2,0},{-1,1,1,1}}",
	  " P = convexHull V"
	  },
     
     PARA{}, "This gives an overview of the characteristics of the polyhedron. If we want to know 
     more details, we can just ask for them",
     
     EXAMPLE {
	  " vertices P"
	  },
     
     PARA{}, "and see that the point (0,1) is not a vertex and it is actually a triangle.",
     
     EXAMPLE {
	  " (HS,v) = halfspaces P"
	  },
     
     PARA{}, "gives the defining affine halfspaces, i.e. ",TT "P"," is given by all ",TT "p"," such 
     that ",TT "HS*p =< v"," and that lie in the defining affine hyperplanes. But since",
     
     EXAMPLE {
	  " hyperplanes P"
	  },
     
     PARA{}, "there are none, so the polyhedron is of full dimension. It is also compact, since",
     
     EXAMPLE {
	  " rays P",
	  " linSpace P"
	  },
     
     PARA{}, "Furthermore, we can construct the convex hull of a set of points and a set of rays",
     
     EXAMPLE {
	  " R = matrix {{1},{0},{0}}",
	  " V1 = V || matrix {{1,1,1,1}}",
	  " P1 = convexHull(V1,R)",
	  " vertices P1"
	  },
     
     PARA{}, "This polyhedron is not compact anymore and also not of full dimension",
     
     EXAMPLE {
	  " rays P1",
	  " hyperplanes P1"
	  },
     
     PARA{}, "On the other hand we can construct a polyhedron as the ",TO intersection," of affine 
     halfspaces and affine hyperplanes.",
     
     EXAMPLE {
	  " HS = transpose (V || matrix {{-1,2,0,1}})",
	  " v = matrix {{1},{1},{1},{1}}",
	  " HP = matrix {{1,1,1}}",
	  " w = matrix {{3}}",
	  " P2 = intersection(HS,v,HP,w)"
	  },
     
     PARA{}, "This is a triangle in 3-space with",
     
     EXAMPLE {
	  " vertices P2"
	  },
     
     PARA{}, "If we don't intersect with the hyperplane we get",
     
     EXAMPLE {
	  " P3 = intersection(HS,v)",
	  " vertices P3",
	  " linSpace P3"
	  },
     
     PARA{}, "Note that the vertices are given modulo the lineality space. Besides constructing 
     polyhedra by hand, there are also some basic polyhedra implemented such as 
     the ",TO hypercube," ",
     
     EXAMPLE {
	  " P4 = hypercube(3,2)",
	  " vertices P4"
	  },
     
     PARA{}, "in this case with edge-length 4, the ",TO crossPolytope," ",
     
     EXAMPLE {
	  " P5 = crossPolytope(3,3)",
	  " vertices P5"
	  },
     
     PARA{}, "in this case with diameter 6, or the standard simplex 
     (",TO stdSimplex,") ",
     
     EXAMPLE {
	" P6 = stdSimplex 2",
	" vertices P6"
	},
   
     PARA{}, "Now that we can construct polyhedra, we can turn to the functions 
     that can be applied to polyhedra. First of all, we can apply the ",TO convexHull," 
     function also to a pair of polyhedra:",
     
     EXAMPLE {
	  " P7 = convexHull(P4,P5)",
	  " vertices P7"
	  },
     
     PARA{}, "Or we can intersect them by using ",TO intersection,":",
     
     EXAMPLE {
	  " P8 = intersection(P4,P5)",
	  " vertices P8"
	  },
     
     PARA{}, "Furthermore, both functions can be applied to a list containing any number 
     of polyhedra and matrices defining vertices/rays or affine halfspaces/hyperplanes 
     that are in the same ambient space. For example:",
     
     EXAMPLE {
	  " P9 = convexHull {(V1,R),P2,P6}",
	  " vertices P9"
	  },
     
     PARA{}, "Further functions are for example the Minkowski sum (",TO minkowskiSum,") of 
     two polyhedra",
     
     EXAMPLE {
	  " Q = convexHull (-V)",
	  " P10 = P + Q",
	  " vertices P10"
	  },
     
     PARA{}, "In the other direction, we can also determine all Minkowski summands 
     (see ",TO minkSummandCone,") of a polyhedron.",
     
     EXAMPLE {
	  " (C,L,M) = minkSummandCone P10",
	  " apply(values L, vertices)"
	  },
     
     PARA{}, " Here the polyhedra in the hashTable ",TT "L"," are all possible Minkowski 
     summands up to skalar multiplication and the columns of ",TT "M"," give the minimal 
     decompositions. So the hexagon ",TT "P4"," is not only the sum of two triangles but also the sum 
     of three lines. Furthermore, we can take the direct product of two polyhedra",
     
     EXAMPLE {
	  " P11 = P * Q",
	  " vertices P11"
	  },
     
     PARA{}, "which is in QQ^4",
     
     EXAMPLE {
	  "ambDim P11"
	  },
     
     PARA{}, "To find out more about this polyhedron use for example",
     
     EXAMPLE {
	  " fVector P11"
	  },
     
     PARA{}, "which gives the number of faces of each dimension, so it has 9 
     vertices, 18 edges , .. and so on. We can access the
     faces of a certain codimension via",
     
     EXAMPLE {
	  " L = faces(1,P11)",
	  " apply(L,vertices)"
	  },
     
     PARA{}, "and even compute all lattice points of the polyhedron",
     
     EXAMPLE {
	  " L = latticePoints P11",
	  " #L"
	  },
     
     PARA{}, "and also the tail/recession cone of a polyhedron",
     
     EXAMPLE {
	  " C = tailCone P1",
	  " rays C"
	  },
     
     PARA{}, "Finally, there is also a function to compute the polar of a 
     polyhedron, i.e. all points in the dual space that are greater than -1 on 
     all points of the polyhedron:",
     
     EXAMPLE {
	  " P12 = polar P11",
	  " vertices P12"
	  }
     }

document {
     Key => "Working with cones",
     
     "We start with a cone in 2-space which is the positive hull (",TO posHull,") of a given set of rays",
     
     EXAMPLE {
	  " R = matrix {{1,1,2},{2,1,1}}",
	  " C = posHull R",
	  " ambDim C"
	  },
     
     PARA{}, "This gives an overview of the characteristics of the cone. If we want to know 
     more details, we can just ask for them",
     
     EXAMPLE {
	  " rays C"
	  },
     
     PARA{}, "and see that (1,1) is not an extremal ray of the cone.",
     
     EXAMPLE {
	  " HS = halfspaces C"
	  },
     
     PARA{}, "gives the defining linear halfspaces, i.e. ",TT "C"," is given by all ",TT "p"," in 
     the defining linear hyperplanes that satisfy ",TT "HS*p >= 0",". But since",
     
     EXAMPLE {
	  " hyperplanes C"
	  },
     
     PARA{}, "there are none, so the polyhedron is of full dimension. Furthermore, we can construct 
     the positive hull of a set of rays and a linear subspace",
     
     EXAMPLE {
	  " R1 = R || matrix {{0,0,0}}",
	  " LS = matrix {{1},{1},{1}}",
	  " C1 = posHull(R1,LS)",
	  " rays C1"
	  },
     
     PARA{}, "Note that the rays are given modulo the lineality space. On the other hand we can 
     construct cones as the ",TO intersection," of linear halfspaces and hyperplanes.",
     
     EXAMPLE {
	  " HS = transpose R1",
	  " HP = matrix {{1,1,1}}",
	  " C2 = intersection(HS,HP)"
	  },
     
     PARA{}, "This is a two dimensional cone in 3-space with",
     
     EXAMPLE {
	  " rays C2"
	  },
     
     PARA{}, "If we don't intersect with the hyperplane we get",
     
     EXAMPLE {
	  " C3 = intersection HS",
	  " rays C3",
	  " linSpace C3"
	  },
     
     PARA{}, "Again, the rays are given modulo the lineality space. Also, one can use 
     given cones, for example the positive orthant (",TO posOrthant,"):",
     
     EXAMPLE {
	  " C4 = posOrthant 3",
	  " rays C4"
	  },
     
     PARA{}, "Now that we can construct cones, we can turn to the functions 
     that can be applied to cones. First of all, we can apply the ",TO intersection," 
     function also to a pair of cones in the same ambient space:",
     
     EXAMPLE {
	  " C5 = intersection(C1,C2)",
	  " rays C5"
	  },
     
     PARA{}, "Or we can take their positive hull by using ",TO posHull,":",
     
     EXAMPLE {
	  " C6 = posHull(C1,C2)",
	  " rays C6",
	  " linSpace C6"
	  },

     PARA{}, "Furthermore, both functions (",TO intersection," and ",TO posHull,") can 
     be applied to a list containing any number of cones and matrices defining 
     rays/linSpace or linear halfspaces/hyperplanes that are in the same ambient 
     space. For example:",
     
     EXAMPLE {
	  " R2 = matrix {{2,-1},{-1,2},{-1,-1}}",
	  " C7 = posHull {R2,C3,C4}",
	  " rays C7",
	  " linSpace C7"
	  },
     
     PARA{}, "But since they are all cones their positive hull is the same as their 
     Minkowski sum, so in fact:",
     
     EXAMPLE {
	  " C6 == C1 + C2"
	  },
     
     PARA{}, "But we can take the Minkowski sum of a cone and a polyhedron. For this, 
     both objects must lie in the same ambient space and the resulting object is then 
     a polyhedron:",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " P1 = C6 + P",
	  " (vertices P1,rays P1)"
	  },
     
     PARA{}, "Furthermore, we can take the direct product (",TO directProduct,") of 
     two cones",
     
     EXAMPLE {
	  " C8 = C * C1",
	  " rays C8",
	  " linSpace C8"
	  },
     
     PARA{}, "which is in QQ^5",
     
     EXAMPLE {
	  "ambDim C8"
	  },
     
     PARA{}, "To find out more about this cone use for example",
     
     EXAMPLE {
	  " fVector C8"
	  },
     
     PARA{}, "which gives the number of faces of each dimension, so it has 1 
     vertex, the origin, 1 line, 4 two dimensional faces, .. and so on. We can access the
     faces of a certain codimension via",
     
     EXAMPLE {
	  " L = faces(1,C8)",
	  " apply(L,rays)"
	  },
     
     PARA{}, "We can also check if the cone is smooth:",
     
     EXAMPLE {
	  " isSmooth C8"
	  },
     
     PARA{}, "or even compute the Hilbert basis of the cone",
     
     EXAMPLE {
	  " L = hilbertBasis C8",
	  " #L"
	  },
     
     PARA{}, "Finally, there is also a function to compute the dual cone, i.e. 
     the set of all points in the dual space that are positive on the cone.",
     
     EXAMPLE {
	  " C9 = dualCone C8",
	  " rays C9"
	  }
     
     }

document {
     Key => "Working with fans",
     
     "We start by constructing a fan that consists of a single cone and all of its 
     faces:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " F = makeFan C"
	  },
     
     PARA{}, "By this, we have already constructed the fan consisting of the 
     positive orthant and all of its faces. The package saves the generating cones 
     of the fan, that can be accessed by:",
     
     EXAMPLE {
	  "genCones F"
	  },
     
     PARA{}, "Now we can expand the fan by adding more cones:",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0,0},{1,1,0},{0,0,-1}}",
	  " F = addCone(C1,F)"
	  },
     
     PARA{}, "Ok, in this case we can not, because the two cones are not compatible, 
     i.e. their intersection is not a face of each. So, when one tries to add a cone 
     to a fan that is not compatible to one of the generating cones of the fan, the 
     function ",TO addCone," prompts the above message and returns the fan as it was, 
     before trying to add the new cone (see ",TO addCone,"). For two cones one can 
     check if their intersection is a common face by using ",TO commonFace,":",
     
     EXAMPLE {
	  " commonFace(C,C1)"
	  },
     
     PARA{}, "Since the intersection of both is already computed in this function 
     there is a different function that also returns the intersection, to save 
     computation time when one needs the intersection afterwards anyway:",
     
     EXAMPLE {
	  " (b,C2) = areCompatible(C,C1)",
	  " rays C2"
	  },
     
     PARA{}, "So let us correct the cone and add it to the fan.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}}",
	  " F = addCone(C1,F)"
	  },
     
     PARA{}, "Instead of creating a fan with one cone and then adding more cones, we 
     can also just make a fan out of a list of cones:",
     
     EXAMPLE {
	  " C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " C3 = posHull matrix {{-1,0,0},{0,1,0},{0,0,-1}};",
	  " C4 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,1}};",
	  " C5 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = makeFan {C2,C3,C4,C5}"
	  },
     
     PARA{}, "Or we could add a list of cones to an existing fan:",
     
     EXAMPLE {
	  " C6 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};",
	  " C7 = posHull matrix {{1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = addCone( {C6,C7}, F1)",
	  },
     
     PARA{}, "Finally, we can add a whole fan to another fan:",
     
     EXAMPLE {
	  " F1 = addCone(F,F1)"
	  },
     
     PARA{}, "So, ",TO makeFan," and ",TO addCone," are the methods to construct 
     fans ''from scratch'', but there are 
     also methods to get fans directly, for example ",TO normalFan,":",
     
     EXAMPLE {
	  " P = hypercube 4",
	  " F2 = normalFan P"
	  },
     
     PARA{}, "Now we have seen how to construct fans, so we turn to functions on fans, 
     for example the direct product (",TO directProduct,":",
     
     EXAMPLE {
	  " F3 = makeFan {posHull matrix {{1}},posHull matrix {{-1}}}",
	  " F1 = F3 * F1"},
     
     PARA{}, "in the direct product of the ambient spaces",
     
     EXAMPLE {
	  " ambDim F1"
	  },
     
     PARA{}, " Of course, we can check if two fans are the same:",
     
     EXAMPLE {
	  " F1 == F2"
	  },
     
     PARA{}, "Now we construct a new fan to show some other functions.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};",
	  " C2 = posHull matrix {{1,1,1},{0,1,-1},{-1,1,1}};",
	  " C3 = posHull matrix {{-1,-1,-1},{0,1,-1},{-1,1,1}};",
	  " C4 = posHull matrix {{1,-1},{0,0},{-1,-1}};",
	  " F = makeFan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "This is not a ''very nice'' fan, as it is neither complete nor 
     of pure dimension:",
     
     EXAMPLE {
	  " isComplete F",
	  " isPure F"
	  },
     
     PARA{}, "But if we make the fan complete,",
     
     EXAMPLE {
	  " C5 = posHull matrix {{1,-1,1,-1},{-1,-1,0,0},{1,1,-1,-1}};",
	  " C6 = posHull matrix {{1,-1,1,-1},{1,1,0,0},{1,1,-1,-1}};",
	  " F = addCone({C5,C6},F)",
	  " isComplete F"
	  },
     
     PARA{}, "we can even check if the fan is projective:",
     
     EXAMPLE {
	  " isProjective F"
	  },
     
     PARA{}, "If the fan is projective, the function returns a polyhedron of which 
     the fan is the normalFan. This means our fan is projective."
     
     }
        
document {     
     Key => Polyhedron,
     Headline => "the class of all convex polyhedra",
          
     "A Polyhedron represents a rational polyhedron. It can be bounded or unbounded, 
     need not be full dimensional or may contain a proper affine subspace. It can 
     be empty or zero dimensional. It is saved as a mutable hashtable which contains 
     the vertices, generating rays, and the basis of the lineality space of the 
     Polyhedron as well as the defining affine halfspaces and hyperplanes. The 
     output of a Polyhedron looks like this:",
     
     EXAMPLE {
	  " convexHull(matrix {{0,0,-1,-1},{2,-2,1,-1},{0,0,0,0}},matrix {{1},{0},{0}})",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Polyhedron. 
     Note that the number of rays and vertices are modulo the lineality space. So 
     for example a line in QQ^2 has one vertex and no rays. However, one can not
     access the above information directly, because this is just a virtual hashtable 
     generated for the output. The informations of a Polyhedron are extracted 
     by the functions included in this package. A Polyhedron can be constructed as 
     the convex hull of a set of points and a set of rays or as the intersection of 
     a set of affine halfspaces and affine hyperplanes.",
     
     PARA{}, "For example, consider the square and the square with an emerging ray 
     for the convex hull:",
     
     EXAMPLE {
	  " V = matrix {{1,1,-1,-1},{1,-1,1,-1}}",
	  " convexHull V",
	  " R = matrix {{1},{1}}",
	  " convexHull(V,R)"
	  },
     
     PARA{}, "Or the crosspolytope for the intersection:",
     
     EXAMPLE {
	  " HS = transpose V",
	  " v = R || R",
	  " P = intersection(HS,v)",
	  " vertices P"
	  },
     
     PARA{}, "which we can for example embed in 3-space on height 1:",
     
     EXAMPLE {
	  " HS = HS | matrix {{0},{0},{0},{0}}",
	  " HP = matrix {{0,0,1}}",
	  " w = matrix {{1}}",
	  " P = intersection(HS,v,HP,w)",
	  " vertices P"
	  },
     
     PARA{}, "See also ",TO "Working with polyhedra","."
     
     }

document {     
     Key => Cone,
     Headline => "the class of all rational convex polyhedral cones",
     
     "A Cone represents a rational convex polyhedral cone. It need not be full 
     dimensional or may contain a proper linear subspace. It can be zero 
     dimensional, i.e. the origin. It is saved as a mutable hashtable which 
     contains the generating rays and the basis of the lineality space of the 
     cone as well as the defining halfspaces and hyperplanes. The output of a 
     Cone looks like this:",
     
     EXAMPLE {
	  " posHull matrix {{0,0,-1,-1,1},{2,-2,1,-1,0},{1,1,1,1,0}}",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Cone. The number 
     of rays is modulo the lineality space. However, one can not access the above information 
     directly, because this is just a virtual hashtable generated for the output. The 
     informations of a Cone are extracted by the functions included in this package. A Cone 
     can be constructed as the positive hull of a set of rays or as the intersection of a set 
     of linear halfspaces and linear hyperplanes.",
     
     PARA{}, "As examples for the positive hull consider the following cones:",
     
     EXAMPLE {
	  " R = matrix{{1,2,3,1},{2,3,1,1},{3,1,2,1}}",
	  " C = posHull R",
	  " rays C",
	  " LS = matrix{{1},{1},{-2}}",
	  " C = posHull(R,LS)",
	  " rays C"
	  },
     
     PARA{}, "Or for the intersection:",
     
     EXAMPLE {
	  " HS = transpose R",
	  " C = intersection HS",
	  " rays C",
	  " HP = transpose LS",
	  " C = intersection(HS,HP)",
	  " rays C"
	  },
     
     PARA{}, "See also",TO "Working with cones","."
     
     }

document {     
     Key => Fan,
     Headline => "the class of all fans",
     
     "A Fan represents a fan of rational convex polyhedral cones, i.e. a collection of cones, 
     such that for every cone in the fan all faces are in the fan and for every two cones in 
     the fan their intersection is a face of each (intersection condition). 
     It need not be full dimensional or pure, and the cones need not be pointed. It is saved 
     as a mutable hashtable which contains a list of the generating cones of the fan starting 
     with those of maximal dimension. So for every cone in this list all faces are considered 
     to be in the fan. The output of a Fan looks like this:",
     
     EXAMPLE {
	  " normalFan crossPolytope 3",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Fan. 
     However, one can not access the above information directly, because this 
     is just a virtual hashtable generated for the output. The informations of a Fan 
     are extracted by the functions included in this package. A Fan can be constructed by 
     collecting Cones satisfying the intersection condition. Every cone that is added to 
     a Fan is always considered as the collection of the Cone and all of its faces.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{2,2},{1,-1}};",
	  " C2 = posHull matrix {{2,-2},{1,1}};",
	  " C3 = posHull matrix {{-2,-2},{1,-1}};",
	  " C4 = posHull matrix {{-2,2},{-1,-1}};",
	  " F = makeFan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "is for example the normalFan of the flattened crosspolytope in 2-space.",
     
     PARA{}, " See also ",TO "Working with fans","."
     
     }

document {
     Key => {convexHull, (convexHull,Matrix), (convexHull,Matrix,Matrix), 
	  (convexHull,Polyhedron,Polyhedron), (convexHull,List)},
     Headline => "computing the convex hull of points, rays and polyhedra",
     Usage => " P = convexHull M \nP = convexHull(M,N) \nP = convexHull(P1,P2) \nP = convexHull L",
     Inputs => {
	  "M" => Matrix => {"with entries in", TO ZZ," or ", TO QQ},
	  "N" => Matrix => {"with entries in", TO ZZ," or ", TO QQ},
	  "P1" => Polyhedron,
	  "P2" => Polyhedron,
	  "L" => List
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, TT "convexHull", " computes the convex hull of the input. 
     In the first two cases it considers the columns of ", TT "M", " 
     as a set of points and the columns of ", TT "N", " (if given) as 
     a set of rays and computes the polyhedron which is the convex hull 
     of the points plus the rays. The two matrices must have the same 
     number of rows, i.e. the colmuns must lie in the same ambient space. 
     If ", TT "N", " is not given or equal to 0, then the resulting 
     polyhedron is compact and hence a polytope. The points need not 
     be the vertices of the polyhedron. In the third case it computes 
     the convex hull of ", TT "P1", " and ", TT "P2", " if they lie 
     in the same ambient space. Finally, it computes the convex hull 
     of a list ", TT "L"," where the list may contain a combination 
     of the following in any order.",
     
     UL {
	  {"Vertices, given by a matrix ", TT "M", " over ", TO ZZ, " 
	       or ", TO QQ},
	  {"Vertices and rays, given by a sequence ", TT "(V,R)", "of two 
	       matrices over ", TO ZZ, " or ", TO QQ},
	  {TO Cone},
	  {TO Polyhedron}
	},
     
     PARA{}, "Then ", TT "convexHull", " computes the convex hull of all 
     inserted objects, if they are in the same ambient space, i.e. all matrices 
     must have the same number of rows which must equal the ambient dimension 
     of all cones and polyhedra."
     
     }

document {
     Key => {posHull, (posHull,Cone,Cone), (posHull,Matrix), (posHull,Matrix,Matrix), (posHull,Polyhedron), 
	  (posHull,List)},
     Headline => "computes the positive hull of rays, cones, and the cone over a polyhedron",
     Usage => " C = posHull M \nC = posHull(M,N) \nC = posHull(C1,C2) \nC = posHull P \nC = posHull L",
     Inputs => {
	  "M" => Matrix => {"with entries in", TO ZZ," or ", TO QQ},
	  "N" => Matrix => {"with entries in", TO ZZ," or ", TO QQ},
	  "C1" => Cone,
	  "C2" => Cone,
	  "P" => Polyhedron,
	  "L" => List 
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, TT "posHull", " computes the positive hull of the input. In the 
     first two cases it considers the columns of ", TT "M", " as a set of rays 
     and the columns of ", TT "N", " (if given) as generators of the lineality 
     space and computes the cone which is the positive hull of the rays plus 
     the lineality space. The two matrices must have the same number of rows, 
     i.e. the columns must lie in the same ambient space. If ", TT "N", " is 
     not given or equal to 0 then the resulting cone is pointed. The rays need 
     not be a minimal generating set of the cone. If two cones are inserted it 
     computes their positive hull if they lie in the same ambient space. In the 
     case of a polyhedron it computes the cone given by all positive multiples 
     of points of the polyhedron. If applied to a list, it may contain a 
     combination of the following in any order.",
     
     UL {
	  {"Rays, given by a matrix ", TT "R", " over ", TO ZZ, " 
	       or ", TO QQ},
	  {"Rays and a lineality space, given by a sequence ", TT "(R,LS)", " of two 
	       matrices over ", TO ZZ, " or ", TO QQ},
	  {TO Cone},
	  {TO Polyhedron}
	},
     
     PARA{}, "Then ", TT "posHull", " computes the positive hull of all 
     inserted objects, if they are in the same ambient space, i.e. all matrices 
     must have the same number of rows which must equal the ambient dimension 
     of all cones and polyhedra."
     
     }

document {
     Key => {intersection, (intersection,Cone,Cone), (intersection,List), (intersection,Matrix), 
	  (intersection,Matrix,Matrix), (intersection,Matrix,Matrix,Matrix,Matrix), (intersection,Polyhedron,Polyhedron)},
     Headline => "computes the intersection of halfspaces, hyperplanes, cones, and polyhedra",
     Usage => " P = intersection L \nC = intersection M \nC = intersection(M,N) \nP = intersection(M,v) \nP = intersection(M,v,N,w) \nC = intersection(C1,C2) \nP = intersection(P1,P2)",
     Inputs => {
	  "L" => List => {"containing any of the inputs below"},
	  "M" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "N" => Matrix => {"with entries in ", TO ZZ," or ", TO QQ},
	  "v" => Matrix => {"with only one column and entries in ", TO ZZ," or ", TO QQ},
	  "w" => Matrix => {"with only one column and entries in ", TO ZZ," or ", TO QQ},
	  "C1" => Cone,
	  "C2" => Cone,
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     
     PARA{}, "When inserting any of the combination of matrices into ", 
     TT "intersection", ", it considers the given matrices as defining 
     inequalities and equalities. Thus, it either computes the polyhedron ",
     TT "P = {p | M*p <= v and N*p = w }",". Therefore, ", TT "M", " and ",
     TT "N", " must have the same number of columns, which will be the 
     dimension of the ambient space, and ", TT "M", " and ", TT "v", " as 
     well as ", TT "N", " and ", TT "w", " must have the same number of 
     rows respectively. If ", TT "N", " and ", TT "w", " are omitted then 
     the polyhedron is just given by the inequalities. If ", TT "v", " 
     and ", TT "w", " are omitted then they are considered to be 0 so 
     that the intersection is a cone and thus the output is of class Cone.",
     
     PARA{}, "If two polyhedra or two cones are inserted, then the 
     intersection of both arguments is computed if both arguments lie in 
     the same ambient space. If both arguments are cones then the output 
     is again a cone. Otherwise intersection returns a polyhedron.",
     
     PARA{}, "If ", TT "intersection", " is called for a list ", TT "L", ", 
     then the list may contain a combination of the following in any order.",
     UL {
	  {"Inequalities, given by a sequence ", TT "(M,v)", " of matrices 
	    over ", TO ZZ, " or ", TO QQ, " determining inequalities as above"},
	  {"Equalities, given by a list ", TT "{N,w}", " of matrices 
	    over ", TO ZZ, " or ", TO QQ, " determining equalities as above"},
	  {TO Cone},
	  {TO Polyhedron}
	},
   
     PARA{}, "Then ", TT "intersection", " computes the intersection of all 
     inserted objects, if they are in the same ambient space, i.e. all matrices 
     must have the same number of rows which must equal the ambient dimension 
     of all cones and polyhedra."
     
     }

document {
     Key => {makeFan, (makeFan,Cone), (makeFan,List)},
     Headline => "generates a Fan",
     Usage => " F = makeFan C \nF = makeFan L",
     Inputs => {
	  "C" => Cone,
	  "L" => List => {"with elements of class ", TO Cone, " or ", TO Fan}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, " If ",TT "makeFan", " is applied to a ", TO Cone, " it generates 
     the ", TO Fan, " given by the the Cone and all of its faces. If applied to 
     a ", TO List, " the list must only contain Cones and Fans in the same 
     ambient space. Then it adds the Cones in the List and the generating Cones 
     of the Fans in the List one by one to the Fan, checking each time if the 
     new Cone is compatible with the cones that have already been added, i.e. 
     that the intersection with each of them is a face of both Cones 
     (intersection condition).",
     
     PARA{}, "If one of the cones is in the wrong ambient space, there will be an 
     error and no fan will be returned. If the intersection condition fails, the 
     function will prompt that there are two incompatible cones and return the 
     fan that has been constructed so far without adding the incompatible cone. 
     The pair of incompatible cones can be accessed with the 
     function ",TO incompCones,".",
          
     EXAMPLE {
	  " C = posHull matrix {{1,-1},{0,-1}}",
	  " F = makeFan C",
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{0,-1},{1,-1}};",
	  " F = makeFan {C,C1,C2}"
	  }
     
     }

document {
     Key => {addCone, (addCone,Cone,Fan), (addCone,List,Fan), (addCone,Fan,Fan)},
     Headline => "adds a cones to a Fan",
     Usage => " F1 = addCone(C,F) \nF1 = addCone(L,F)",
     Inputs => {
	  "C" => Cone,
	  "L" => List => {"with elements of class ", TO Cone, " or ", TO Fan},
	  "F" => Fan
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, "If ",TT "addCone", " is applied to a ", TO Cone, " and a ",TO Fan, " 
     it adds the Cone to the Fan if they are in the same ambient space, if the Cone is 
     compatible to every generating Cone of ",TT "F", ", but is not a face of one 
     of them. If the first condition fails, there will be an error and no fan will be
     returned. If the second condition fails, the function will prompt that there are 
     two incompatible cones and return the fan without adding the incompatible cone. 
     The pair of incompatible cones can be accessed with the function ",TO incompCones,". 
     If the last condition fails, then the cone is already in the fan as a face of one 
     of the cones, so it does not have to be added. The conditions are checked in this 
     order.",
     
     PARA{}, "If ",TT "addCone"," is applied to a ",TO List," and a ",TO Fan,", then 
     the function adds the list cone by cone and stops if one of the three conditions 
     fails for one of the cones. There is again an error for the first condition. If 
     the second fails, then it prompts the incompatability and returns the fan 
     constructed so far. The pair of incompatible cones can again be retrieved using ",
     TO incompCones,".",
     
     PARA{}, "If applied to a pair of cones it adds the generating cones of the first 
     fan to the second fan, again checking for the same conditions as above",
     
     
     PARA{}, " As an example, we make a fan consisting of the following cone and 
     try to add an adjacent orthant.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,1},{0,0,1}};",
	  " F = makeFan C",
	  " C = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " addCone(C,F)"
	  },
     
     PARA{}, "This shows that the two cones do not intersect in a common face, but 
     if we divide C up into two parts, we get a fan.",
     
     EXAMPLE {
	  " C1 = intersection {C, (matrix {{0,1,-1}}, matrix {{0}})};",
	  " C2 = intersection {C, (matrix {{0,-1,1}}, matrix {{0}})};",
	  " F = addCone({C1,C2},F)"
	  }
     
     }

document {
     Key => {symmDiff, (symmDiff,List,List)},
     Headline => "computes the symmetric difference",
     Usage => " L = symmDiff(L1,L2)",
     Inputs => {
	  "L1" => List,
	  "L2" => List
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "symmDiff", " is an auxiliary function to compute the symmetric 
     difference of two lists, i.e. the list of all elements contained in either 
     of the lists but not in both.",
     
     EXAMPLE {
	  " L1 = {f,a,s,t}",
	  " L2 = {s,t,n,c,y}",
	  " symmDiff(L1,L2)"
	  }
     }

document {
     Key => {union, (union,List,List)},
     Headline => "computes the union of two lists",
     Usage => " L = union(L1,L2)",
     Inputs => {
	  "L1" => List,
	  "L2" => List
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "union", " is an auxiliary function to compute the union 
     of two lists."
     
     }

document {
     Key => {ambDim, (ambDim,Cone), (ambDim,Fan), (ambDim,Polyhedron)},
     Headline => "ambient dimension of a Polyhedron, Cone or Fan",
     Usage => "d = ambDim P \nd = ambDim C \nd = ambDim F",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone,
	  "F" => Fan
	  },
     Outputs => {
	  "d" => ZZ => {" which is the dimension of the ambient space"}
	  },
     
     PARA{}, TT "ambDim", " returns the dimension of the ambient space 
     either of the ", TO Polyhedron," ",TT "P", ", of the ", TO Cone," ",TT "C", " 
     or the ", TO Fan," ",TT "F", ".",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0},{0,1}}",
	  " ambDim P",
	  }
     }

document {
     Key => {cones, (cones,ZZ,Fan)},
     Headline => "computes all cones of a fan of a certain dimension",
     Usage => " L = cones(d,F)",
     Inputs => {
	  "d" => ZZ => {" between 0 and the dimension of the fan"},
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "cones", " computes the ", TO List, " of all Cones in ",
     TT "F", " of dimension ", TT "d", ".",
     
     EXAMPLE {
	  " F = normalFan hypercube 3",
	  " L = cones(2,F)",
	  },
     
     PARA{}, "To actually see the cones of the fan we can look at their 
     rays for example:",
     
     EXAMPLE {
	  " apply(L,rays)"
	  }
     
     }

document {
     Key => {genCones, (genCones,Fan)},
     Headline => "displays the generating Cones of a Fan",
     Usage => " L = genCones F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "genCones", " displays the ", TO List, " of generating cones 
     of the ", TO Fan, ", i.e. all Cones that are not a face of one 
     of the other cones. These are all of the same dimension if and only if 
     the Fan is pure (see: ", TO isPure,").",
     
     EXAMPLE {
	  " F = normalFan crossPolytope 3",
	  " L = genCones F",
	  " apply(L,rays)"
	  }
     
     }

document {
     Key => {halfspaces, (halfspaces,Cone), (halfspaces,Polyhedron)},
     Headline => "computes the defining halfspaces of a Cone or a Polyhedron",
     Usage => " M = halfspaces C \n(M,v) = halfspaces P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix => {"with entries over ", TO QQ},
	  "v" => Matrix => {"with entries over ", TO QQ, " and only one column"}
	  },
     
     PARA{}, TT "halfspaces", " returns the defining affine halfspaces. For a 
     polyhedron ", TT "P", " the output is ", TT "(M,v)", ", where the source 
     of ", TT "M", " has the dimension of the ambient space of ", TT "P", " 
     and ", TT "v", " is a one column matrix in the target space 
     of ", TT "M", " such that ",TT "P = {p in H | M*p =< v}", " where 
     ", TT "H", " is the intersection of the defining affine hyperplanes.",
     
     PARA{}, " For a cone ", TT "C", " the output is ", TT "M", " which is the 
     same matrix as before but ", TT "v", " is ommited since it is 0, 
     so ", TT "C = {c in H | M*c =< 0}", " and ", TT "H", " is the intersection 
     of the defining linear hyperplanes.",
     
     EXAMPLE {
	  " R = matrix {{1,1,2,2},{2,3,1,3},{3,2,3,1}};",
	  " V = matrix {{1,-1},{0,0},{0,0}};",
	  " C = posHull R",
	  " halfspaces C"
	  },
     
     PARA{}, "Now we take this cone over a line and get a polyhedron.",
     
     EXAMPLE {
	  " P = convexHull(V,R)",
	  " halfspaces P"
	  }
     
     }

document {
     Key => {hyperplanes, (hyperplanes,Cone), (hyperplanes,Polyhedron)},
     Headline => "computes the defining hyperplanes of a Cone or a Polyhedron",
     Usage => " N = hyperplanes C \n(N,w) = hyperplanes P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "N" => Matrix => {"with entries over ", TO QQ},
	  "w" => Matrix => {"with entries over ", TO QQ, " and only one column"}
	  },
     
     PARA{}, TT "hyperplanes", " returns the defining affine hyperplanes for a 
     polyhedron ", TT "P", ". The output is ", TT "(N,w)", ", where the source 
     of ", TT "N", " has the dimension of the ambient space of ", TT "P", " 
     and ", TT "w", " is a one column matrix in the target space 
     of ", TT "N", " such that ",TT "P = {p in H | N*p = w}", " where 
     ", TT "H", " is the intersection of the defining affine halfspaces.",
     
     PARA{}, " For a cone ", TT "C", " the output is ", TT "N", " which is the 
     same matrix as before but ", TT "w", " is ommited since it is 0, 
     so ", TT "C = {c in H | N*c = 0}", " and ", TT "H", " is the intersection 
     of the defining linear halfspaces.",
     
     EXAMPLE {
	  " P = stdSimplex 2",
	  " hyperplanes P",
	  " C = posHull matrix {{1,2,4},{2,3,5},{3,4,6}}",
	  " hyperplanes C"
	  }
     
     }

document {
     Key => {linSpace, (linSpace,Cone), (linSpace,Fan), (linSpace,Polyhedron)},
     Headline => "computes a basis of the lineality space",
     Usage => " LS = linSpace C \nLS = linSpace F \nLS = linSpace P",
     Inputs => {
	  "C" => Cone,
	  "F" => Fan,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "LS" => Matrix
	  },
     
     PARA{}, TT "linSpace", " returns a basis of the lineality space of the 
     input as the columns of the matrix ", TT "LS", ". The lineality space of a 
     Fan is the lineality space of any Cone of the Fan, since they all have the 
     same lineality space.",
     
     EXAMPLE {
	  " M = matrix {{1,1,1},{0,1,0},{-1,1,-1},{-1,-1,-1},{0,-1,0},{1,-1,1}};",
	  " v = matrix {{2},{1},{2},{2},{1},{2}};",
	  " P = intersection(M,v)",
	  " linSpace P",
	  " C = dualCone intersection M",
	  " linSpace C"
	  }
     
     }

document {
     Key => {rays, (rays,Cone), (rays,Fan), (rays,Polyhedron)},
     Headline => "displays all rays of a Cone, a Fan, or a Polyhedron",
     Usage => " R = rays C \nR = rays F \nR = rays P",
     Inputs => {
	  "C" => Cone,
	  "F" => Fan,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "R" => Matrix
	  },
     
     PARA{}, TT "rays", " returns the rays of the input as the columns of the 
     matrix ", TT "R", ".",
     
     EXAMPLE {
	  " P = convexHull(matrix {{1,-1,2,-2},{1,1,2,2}}, matrix {{0},{1}})",
	  " rays P",
	  " C = posHull P",
	  " rays C",
	  " F = normalFan P",
	  " rays F"
	  }
     
     }

document {
     Key => {vertices, (vertices,Polyhedron)},
     Headline => "displays the vertices of a Polyhedron",
     Usage => " V = vertices P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "V" => Matrix
	  },
     
     PARA{}, TT "vertices", " returns the vertices of the Polyhedron ", TT "P", " 
     as the columns of the Matrix ", TT "V",".",
     
     EXAMPLE {
	  " P = intersection(matrix{{1,-1},{0,-1},{-1,-1},{0,1}}, matrix{{0},{-1},{0},{1}})",
	  " vertices P"
	  }
     
     }

document {
     Key => {areCompatible, (areCompatible,Cone,Cone)},
     Headline => "checks if the intersection of two cones is a face of each",
     Usage => " (b,C) = areCompatible(C1,C2)",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face of each cone, 
	       and ", TO false, " otherwise."},
	  "C" => Cone => {" the intersection of both Cones."}
	  },
     
     PARA{}, TT "areCompatible", " is an extension of ", TO commonFace, " for Cones. It 
     also checks if the intersection ", TT "C", " of ", TT "C1", " and ", TT "C2", " is a 
     face of each and the answer is given by ", TT "b", ". Furhtermore, the intersection 
     is given for further calculations.",
     
     PARA{}, "For example, consider the following three cones",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-1}};",
	  " C3 = posHull matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "that might form a fan, but if we check if they are compatible, we see they 
     don't:",
     
     EXAMPLE {
	  " areCompatible(C1,C2)",
	  " areCompatible(C2,C3)",
	  " areCompatible(C3,C1)",
	  }
     
     }

document {
     Key => {commonFace, (commonFace,Cone,Cone), (commonFace,Polyhedron,Polyhedron)},
     Headline => "checks if the intersection is a face of both Cones or Polyhedra",
     Usage => " b = commonFace(C1,C2) \nb = commonFace(P1,P2)",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone,
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face both, 
	       and ", TO false, " otherwise."}
	  },
     
     PARA{}, TT "commonFace", " checks if the intersection of ", TT "C1", " 
     and ", TT "C2", " or the intersection of ", TT "P1", " and ", TT "P2", " is 
     a face of both.",
     
     PARA{}, "For example, consider the follwing three cones",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-1}};",
	  " C3 = posHull matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "and check if their intersection is a common face:",
     
     EXAMPLE {
	  " commonFace(C1,C2)",
	  " commonFace(C2,C3)",
	  " commonFace(C3,C1)",
	  }
     
     }

document {
     Key => {contains, (contains,Cone,Cone), (contains,Cone,Matrix), (contains,Cone,Polyhedron), 
	  (contains,Fan,Cone), (contains,List,Cone), (contains,List,Polyhedron), (contains,Polyhedron,Cone), 
	  (contains,Polyhedron,Matrix), (contains,Polyhedron,Polyhedron)},
     Headline => "checks if the first argument is contained in the second",
     Usage => " b = contains(C,X) \nb = contains(P,X) \nb = contains(F,C) \nb = contains(L,C) \nb = contains(L,P)",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron,
	  "F" => Fan,
	  "L" => List,
	  "X" => {"either a ", TO Cone,", a ", TO Polyhedron,", or a ", TO Matrix," with only one 
	       column giving a point."}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the first argument contains the second argument, ", 
	       TO false, " otherwise."}
	 },
    
    PARA{}, TT "contains", " determines if the first argument contains the second argument. 
    Both arguments have to lie in the same ambient space. When the first argument is a Cone or 
    Polyhedron, it tests if the equations of the first argument are satisfied by the generating 
    points/rays of the second argument.",
    
    PARA{}, "For example we can check if the 3 dimensional crosspolytope contains the hypercube 
    or the other way around:",
    
    EXAMPLE {
	" P = hypercube 3",
	" Q = crossPolytope 3",
	" contains(Q,P)",
	" contains(P,Q)"
	},
    
    PARA{}, "Or we can check if the hypercube lies in the positive orthant.",
    
    EXAMPLE {
	 " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};",
	 " contains(C,P)",
	 " P = affineImage(P,matrix{{1},{1},{1}})",
	 " contains(C,P)"
	 }
        
    }

document {
     Key => {equals, (equals,Cone,Cone), (equals,Fan,Fan), (equals,Polyhedron,Polyhedron)},
     Headline => "checks equality for convex objects",
     Usage => " b = equals(X,Y)",
     Inputs => {
	  "X" => Cone => {TO Fan, " or ", TO Polyhedron},
	  "Y" => {"an element of the same class as ", TT "X"}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the arguments are equal, ",TO false, " otherwise"}
	  },
     
     PARA{}, "The function ", TT "equals", " is necessary, because the hashTables by 
     which the two objects are given cannot be compared and also the order of the columns 
     for example in the vertices matrix is not unique. It uses the 
     function ", TT "contains", " in both directions. Both objects must be contained in the 
     same ambient space, so for example the positive orthant in ", TO QQ, "^2 and the cone 
     in ", TO QQ, "^3 spanned by ", TT "e_1", " and ", TT "e_2", " are not considered to be 
     equal in ", TT "Polyhedra", ".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " Q = crossPolytope 3",
	  " equals(P,polar Q)"
	  }
     
     }

document {
     Key => {isCompact, (isCompact,Polyhedron)},
     Headline => "checks compactness of a Polyhedron",
     Usage => " b = isCompact P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Polyhedron, " is compact, ", TO false, " otherwise"}
	  },
     
     PARA{}, TT "isCompact", " tests whether ", TT "P"," is compact, i.e. a polytope, by checking if the 
     rays and lineality space matrices are 0.",
     
     EXAMPLE {
	  " P = intersection(matrix{{1,1,1},{0,1,0},{-1,-1,-1},{-1,-1,-1},{0,-1,0},{1,-1,1}},matrix{{2},{1},{2},{2},{1},{2}})",
	  " isCompact P"
	  }
     
     }

document {
     Key => {isComplete, (isComplete,Fan)},
     Headline => "checks completeness of a Fan",
     Usage => " b = isComplete F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Fan, " is complete, ", TO false, " otherwise"}
	  },
     
     PARA{}, TT "isComplete"," just calls an entry in the hashTable of the Fan. The check for completeness 
     is done while generating the fan. Whenever a full dimensional Cone is added (see ", TO makeFan," 
     or ", TO addCone,") the set of faces of codimension 1 that appear only in one full dimensional Cone 
     is updated. The Fan is then complete if and only if this set is empty and there are full dimensional 
     Cones.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-2}};",
	  " C3 = posHull matrix {{0,-2},{1,-1}};",
	  " F = makeFan {C1,C2,C3}",
	  " isComplete F"
	  },
     
     PARA{}, "Hence this fan is not complete, but we can add the missing cone:",
     
     EXAMPLE {
	  " C4 = posHull matrix {{-1,-2},{-2,-1}};",
	  " F = addCone(C4,F)",
	  " isComplete F"
	  }
     
     }

document {
     Key => {isEmpty, (isEmpty,Polyhedron)},
     Headline => "checks if a Polyhedron is empty",
     Usage => " b = isEmpty P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Polyhedron, " is empty, ", TO false, " otherwise"}
	  },
     
     PARA{}, "The polyhedron is empty if the dimension is -1.",
     
     EXAMPLE {
	  " P = intersection(matrix{{1,0},{0,1},{-1,-1}},matrix{{-1},{1},{-1}})",
	  " isEmpty P"
	  }
     
     }

document {
     Key => {isFace, (isFace,Cone,Cone), (isFace,Polyhedron,Polyhedron)},
     Headline => "tests if the first argument is a face of the second",
     Usage => "b = isFace(X,Y)",
     Inputs => {
	  "X" => Cone => {" or ", TO Polyhedron},
	  "Y" => {"an element of the same class as ", TT "X"}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if ", TT "X", " is a face of ",TT "Y",", false otherwise"}
	  },
     
     PARA{}, "Both arguments must lie in the same ambient space. Then ", TT "isFace", " computes all 
     faces of ",TT "Y"," with the dimension of ",TT "X"," and checks if one of them is ",TT "X",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " Q = convexHull matrix{{1,1,1},{1,1,-1},{1,-1,1}}",
	  " isFace(Q,P)"
	  },
     
     PARA{}, "Thus, it is not a face, but we can extend it to a face.",
     
     EXAMPLE {
	  " v = matrix{{1},{-1},{-1}};",
	  " Q = convexHull{Q,v}",
	  " isFace(Q,P)"
	  }
     
     }

document {
     Key => {isPointed, (isPointed,Cone), (isPointed,Fan)},
     Headline => "checks if a Cone or Fan is pointed",
     Usage => "b = isPointed C \nb = isPointed F",
     Inputs => {
	  "C" => Cone,
	  "F" => Fan
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the ",TO Cone," or the ",TO Fan," is pointed, false otherwise"}
	  },
     
     PARA{}, "Tests if a Cone is pointed, i.e. the lineality space is 0. A Fan is pointed if one of its 
     Cones is pointed which is equivalent to all Cones being pointed.",
     
     EXAMPLE {
	  " C = intersection(matrix{{1,1,-1},{-1,-1,-1}})",
	  " isPointed C",
	  " C = intersection{C,(matrix{{1,-1,-1}},0)}",
	  " isPointed C"
	  }
     
     }

document {
     Key => {isProjective, (isProjective,Fan)},
     Headline => "checks if a Fan is projective",
     Usage => "b = isProjective F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "b" => {TO false," if the ", TO Fan," is not projective and a ", TO Polyhedron," of which it is 
	       the normalFan if it is projective"}
	  },
     
     PARA{}, "If ",TT "F"," is projective, then there exists a polyhedron ",TT "P"," such that ",TT "F"," 
     is the normalFan of ",TT "P",". This means every codimension 1 cone of the Fan corresponds exactly to 
     an edge of the polytope. So consider ", TO QQ," to the number of all edges. This can be consider as the 
     space of all edge lengths. If we take arbitrary lengths now for every edge we do not get a polytope. But 
     every codimension 2 cone of the fan corresponds to a 2 dimensional face of the polytope and if the edges 
     belonging to this face add up to 0 zero, they form in fact a 2 dimensional face. This gives linear 
     equations on the space of edge lengths and if we intersect these equations with the positive orthant in 
     the space of edge lengths we get a Cone. Thus, there exists such a polytope if and only if there is a 
     vector in this cone with strictly positive entries, since every edge has to appear in the polytope.",
     
     PARA{}, "Note that the function first checks if the fan is complete.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-2}};",
	  " C3 = posHull matrix {{0,-2},{1,-1}};",
	  " C4 = posHull matrix {{-1,-2},{-2,-1}};",
	  " F = makeFan{C1,C2,C3,C4}",
	  " isProjective F"
	  }
     
     }

document {
     Key => {isPure, (isPure,Fan)},
     Headline => "checks if a Fan is of pure dimension",
     Usage => " b = isPure F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Fan," is of pure dimension, ",TO false," otherwise"}
	  },
     
     PARA{}, TT "isPure", " tests if the ", TO Fan," is pure by checking if the first and the last entry in 
     the list of generating Cones are of the same dimension.",
     
     PARA{}, "Let us construct a fan consisting of the positive orthant and the ray ",TT "v"," which is the 
     negative sum of the canonical basis, which is obviously not pure:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " v = posHull matrix {{-1},{-1},{-1}}",
	  " F = makeFan {C,v}",
	  " isPure F",
	  },
     
     PARA{}, "But we can make the fan pure if we choose any two dimensional face of the positive 
     orthant and takt the cone generated by this face and ",TT "v"," and add it to the cone:",
     
     EXAMPLE {
	  " C1 = posHull{(faces(1,C))#0,v}",
	  " F = addCone(C1,F)",
	  " isPure F"
	  }
     
     }

document {
     Key => {isSmooth, (isSmooth,Cone), (isSmooth,Fan)},
     Headline => "checks if a Cone or Fan is smooth",
     Usage => " b = isSmooth C \nb = isSmooth F",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Cone,"/",TO Fan," is smooth, ",TO false," otherwise"}
	  },
     
     PARA{}, TT "isSmooth"," checks for a ",TO Cone," if the rays are a subset of a basis of the 
     lattice. For a ",TO Fan," it checks smoothness for every ",TO Cone,".",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  " isSmooth C",
	  " F = hirzebruch 3",
	  " isSmooth F"
	  }
     
     }

document {
     Key => {faces, (faces,ZZ,Cone), (faces,ZZ,Polyhedron)},
     Headline => "computes all faces of a certain codimension of a Cone or Polyhedron",
     Usage => " L = faces(k,C) \nL = faces(k,P)",
     Inputs => {
	  "k" => ZZ,
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "L" => List => {"containing the faces of codimension ",TT "k"}
	  },
     
     PARA{}, TT "faces"," computes the faces of codimension ",TT "k"," of the given ",TO Cone," 
     or ",TO Polyhedron,", where ",TT "k"," must be between 0 and the dimension of the second 
     argument. The faces will be of the same class as the original convex object.",
     
     PARA{}, "For example, we look at the edges of the cyclicPolytope with 5 vertices in 3 space",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " L = faces(2,P)"
	  },
     
     PARA{}, "Since this is only a list of polyhedra we look at their vertices:",
     
     EXAMPLE {
	  " apply(L,vertices)"
	  }
     }

document {
     Key => {fVector, (fVector,Cone), (fVector,Polyhedron)},
     Headline => "computes the f-vector of a Cone or Polyhedron",
     Usage => " f = fVector C \nf = fVector P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "L" => List => {"containing the number of faces for each codimension"}
	  },
     
     PARA{}, "The ",TT "i","-th entry of the f-vector of ",TT "P"," is the number of dimension ",
     TT "i","-1 faces of ",TT "P",", so it starts with the number vertices, has 
     dim(",TT "P",")+1 entries, and the last entry is 1 for ",TT "P"," itself. It is the same for 
     a Cone ",TT "C",".",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " fVector P"
	  }
     
     }

document {
     Key => {hilbertBasis, (hilbertBasis,Cone)},
     Headline => "computes the Hilbert basis of a Cone",
     Usage => " HB = hilbertBasis C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "L" => List => {"containing the elements of the Hilbert basis"}
	  },
     
     PARA{}, "The Hilbert basis of the cone ",TT "C"," is computed by the 
     Project-and-Lift-algorithm by Raymond Hemmecke (see below). It computes a Hilbert basis of 
     the cone modulo the lineality space, so it returns a list of one column matrices which give 
     the Hilbert basis of the Cone if one adds the basis of the lineality space and its negative. 
     For the Project-and-Lift-algorithm see: ",
     
     PARA{}, HREF("http://www.hemmecke.de/raymond/", "Raymond Hemmecke's"), " ", EM "On the 
     computation of Hilbert bases of cones", ", in A. M. Cohen, X.-S. Gao, and N. Takayama, 
     editors, Mathematical Software, ICMS 2002, pages 307317. World Scientic, 2002.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2},{2,1}}",
	  " hilbertBasis C"
	  }
     
     }

document {
     Key => {incompCones, (incompCones,Fan)},
     Headline => "returns the pair of incompatible cones",
     Usage => " L = incompCones F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "If the functions ",TO makeFan," or ",TO addCone," encounter a pair of incompatible 
     cones during their construction of the fan, ",TT "incompCones"," retrieves the pair of 
     incompatible cones from the last computation. The first cone is the cone that was already 
     in the fan and the second cone is the one that should have been added. If there were no 
     incompatible cones, then the ",TO List," is empty.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{-1,0},{0,1}};",
	  " C3 = posHull matrix {{2,2},{1,-1}};",
	  " C4 = posHull matrix {{-1,0},{0,-1}};",
	  " F = makeFan {C1,C2,C3,C4}",
	  " L = incompCones F",
	  " L == {C1,C3}"
	  }
     
     }
 
document {
     Key => {inInterior, (inInterior,Matrix,Cone), (inInterior,Matrix,Polyhedron)},
     Headline => "checks if a point lies in the relative interior of a Cone/Polyhedron",
     Usage => " b = inInterior(p,C) \nb = inInterior(p,P)",
     Inputs => {
	  "p" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if ",TT "p"," lies in the relative interior of the 
	       Cone/Polyhedron, ", TO false," otherwise"}
	  },
     
     PARA{}, TT "inInterior", " checks if the smallest face of the ",TO Cone," or 
     the ",TO Polyhedron," containing ",TT "p"," is the ",TO Cone," or 
     the ",TO Polyhedron," itself. For this the number of rows of ",TT "p"," must 
     equal the ambient dimension of the second argument.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " p = matrix{{2},{4},{8}}",
	  " q = matrix{{2},{6},{20}}",
	  " inInterior(p,P)",
	  " inInterior(q,P)"
	  }
     
     }

document {
     Key => {interiorPoint, (interiorPoint,Polyhedron)},
     Headline => "computes a point in the relative interior of the Polyhedron",
     Usage => " p = interiorPoint P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "p" => Matrix => {" over ",TO QQ," with only one column representing a point"}
	  },
     
     PARA{}, TT "interiorPoint", " takes the vertices of the ",TO Polyhedron," and computes the sum 
     multiplied by ",TT "1/n",", where ",TT "n"," is the number of vertices.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " interiorPoint P"
	  }
     
     }

document {
     Key => {interiorVector, (interiorVector,Cone)},
     Headline => "computes a vector in the relative interior of a Cone",
     Usage => " p = interiorVector C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "p" => Matrix => {" over ",TO QQ," with only one column representing a vector"}
	  },
     
     PARA{}, TT "interiorVector", " takes the rays of the ",TO Cone,", computes the sum and 
     divides by the gcd to get a primitive vector.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,4)",
	  " C = posHull P",
	  " interiorVector C"
	  }
     
     }

document {
     Key => {latticePoints, (latticePoints,Polyhedron)},
     Headline => "computes the lattice points of a polytope",
     Usage => " L = latticePoints P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "L" => List => {"containing the lattice points as matrices over ",TO ZZ," with only 
	       one column"}
	  },
     
     PARA{}, TT "latticePoints"," can only be applied to polytopes, i.e. compact polyhedra. It 
     embeds the polytope on height 1 in a space of dimension plus 1 and takes the Cone over 
     this polytope. Then it projects the elements of height 1 of the Hilbert basis back again.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " latticePoints P",
	  " Q = cyclicPolytope(2,4)",
	  " latticePoints Q"
	  }
     
     }

document {
     Key => {minkSummandCone, (minkSummandCone,Polyhedron)},
     Headline => "computes the Cone of all Mikowski summands and the minimal decompositions",
     Usage => " (C,L,M) = minkSummandCone P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C" => Cone,
	  "L" => List => {" containing Polyhedra"},
	  "M" => Matrix
	  },
     
     PARA{}, "For the Minkowski summand cone one takes ",TO QQ,"^d where d is the number 
     of edges of the input polyhedron ",TT "P",". Every Minkowski summand of ",TT "P"," has 
     only edges that are edges of ",TT "P",", so it can be constructed by rescaling every 
     edge of ",TT "P",", i.e. is represented by a point in ",TO QQ,"^d. But not every point 
     in ",TO QQ,"^d gives a polyhedron via this method. This is the case if on the one 
     hand the point lies in the positive orthant and on the other hand for every compact two 
     dimensional face of ",TT "P"," the rescaled edges of such a face give a two dimensional 
     polytope, i.e. the sum of the ordered rescaled edge directions is zero. Therefore, every 
     compact two dimensional face of ",TT "P"," gives a set of linear equalities on a part of 
     the variables in ",TO QQ,"^d. The Minkowski Summand Cone ",TT "C"," is the intersection 
     of the positive orthant with these equations. Thus, every point in ",TT "C"," corrsponds 
     to a Minkowski summand (probably with rescaling). The corresponding polyhedron to every 
     minimal generator of ",TT "C"," is saved in the hashTable ",TT "L",". Finally, all possible 
     minimal decompositions of ",TT "P"," are saved as columns in the matrix ",TT "M",".",
     
     PARA{}, "For example, consider the Minkowski summand cone of the hexagon",
     
     EXAMPLE {
	  " P = convexHull matrix{{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}}",
	  " (C,L,M) = minkSummandCone P"
	  },
     
     PARA{}, "Thus, we see that the minimal Minkowski summands of the hexagon are two triangles 
     and three lines and that there are two minimal decompositions, i.e. the hexagon is the sum 
     of the two triangles or the three lines:",
     
     EXAMPLE {
	  " rays C",
	  " apply(values L,vertices)",
	  " M"
	  }
	  
     
     }

document {
     Key => {skeleton, (skeleton,ZZ,Fan)},
     Headline => "computes the k-skeleton of a Fan",
     Usage => " F1 = skeleton(k,F)",
     Inputs => {
	  "k" => ZZ,
	  "F" => Fan
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, "For an integer ",TT "k"," between 0 and the dimension of ",TT "F",", 
     ",TT "skeleton"," computes the ",TT "k","-skeleton of the ",TO Fan," ",TT "F",", 
     i.e. the ",TO Fan," ",TT "F1"," generated by all cones of dimension 
     ",TT "k"," in ",TT "F",".",
     
     PARA{}, "For example, we can look at the 2-skeleton of the fan of projective 
     3-space:",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0,0,0},{0,1,0,0},{0,0,1,0}}",
	  " F = normalFan P",
	  " F1 = skeleton(2,F)",
	  " apply(genCones F1,rays)"
	  }
     
     }

document {
     Key => {smallestFace, (smallestFace,Matrix,Cone), (smallestFace,Matrix,Polyhedron)},
     Headline => "determines the smallest face of the Cone/Polyhedron containing a point",
     Usage => " C1 = smallestFace(p,C) \nP1 = smallestFace(p,P)",
     Inputs => {
	  "p" => Matrix => {"over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C1" => Cone => {" or"},
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "p"," is considered to be a point in the ambient space of the second argument, so 
     the number of rows of ",TT "p"," must equal the dimension of the ambient space of the 
     second argument. The function computes the smallest face of the second argument which 
     contains ",TT "p",". If the second argument is a ",TO Polyhedron," the output is a 
     ",TO Polyhedron," and if it is a ",TO Cone," the output is a ",TO Cone,". In both cases, 
     if the point is not contained in the second argument then the output is the empty 
     polyhedron.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " p = matrix {{1},{0},{0}}",
	  " smallestFace(p,P)"
	  }
     
     }

document {
     Key => {smoothSubfan, (smoothSubfan,Fan)},
     Headline => "computes the subfan of all smooth cones",
     Usage => " F1 = smoothSubfan F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, " For a given ",TO Fan," ",TT "F"," the function computes the subfan ",TT "F1"," of 
     all smooth cones.",
     
     PARA{}, "Let's consider the fan consisting of the following three dimensional cone and all 
     of its faces:",
     
     EXAMPLE {
	  " C = posHull  matrix {{1,-1,0},{1,1,0},{1,1,1}}",
	  " F = makeFan C"
	  },
     
     PARA{}, "This cone is not smooth, so also the fan is not. But if we remove the interior and one 
     of the two dimensional faces the resulting subfan is smooth.",
     
     EXAMPLE {
	  " F1 = smoothSubfan F",
	  " apply(genCones F1, rays)"
	  }
     
     }

document {
     Key => {tailCone, (tailCone,Polyhedron)},
     Headline => "computes the tail/recession cone of a polyhedron",
     Usage => " C = tailCone P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Every polyhedron ",TT "P"," can be uniquely decomposed into the sum of a 
     polytope and a cone, the tail or recession cone of ",TT "P",". Thus, it is the cone 
     generated by the non-compact part, i.e. the rays and the lineality space 
     of ",TT "P",". If ",TT "P"," is a polytope then the tail cone is the origin in the 
     ambient space of ",TT "P",".",
     
     EXAMPLE {
	  " P = intersection(matrix{{-1,0},{1,0},{0,-1},{-1,-1},{1,-1}},matrix{{2},{2},{-1},{0},{0}}) ",
	  " C = tailCone P",
	  " rays C"
	  }
     
     }

document {
     Key => {vertexEdgeMatrix, (vertexEdgeMatrix,Polyhedron)},
     Headline => "computes the vertex-edge-relations matrix",
     Usage => " M = vertexEdgeMatrix P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix
	  },
     
     PARA{}, TT "vertexEdgeMatrix"," computes the matrix ",TT "M"," where the columns are indexed 
     by the edges and the rows are indexed by the vertices of ",TT "P"," and has 1 as an entry 
     if the corresponding edge contains this vertex and 0 otherwise.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertexEdgeMatrix P"
	  }
     
     }

document {
     Key => {vertexFacetMatrix, (vertexFacetMatrix,Polyhedron)},
     Headline => "computes the vertex-facet-relations matrix",
     Usage => " M = vertexFacetMatrix P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix
	  },
     
     PARA{}, TT "vertexFacetMatrix"," computes the matrix ",TT "M"," where the columns are indexed 
     by the facets and the rows are indexed by the vertices of ",TT "P"," and has 1 as an entry 
     if the corresponding facet contains this vertex and 0 otherwise.",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertexFacetMatrix P"
	  }
     
     }

document {
     Key => {affineHull, (affineHull,Polyhedron)},
     Headline => "computes the affine hull of a polyhedron",
     Usage => " Q = affineHull P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the affine hull of a polyhedron which is the affine subspace with the same 
     dimension as the polyhedron, containing the polyhedron.",
     
     EXAMPLE {
	  " P = stdSimplex 3",
	  " Q = affineHull P",
	  " linSpace Q"
	  }
     
     }

document {
     Key => affineImage,
     Headline => "computes the affine image of a cone or polyhedron"
     }

document {
     Key => {(affineImage,Matrix,Cone,Matrix), (affineImage,Matrix,Cone),
	  (affineImage,Cone,Matrix)},
     Headline => "computes the affine image of a cone",
     Usage => " C1 = affineImage(A,C,b) \nC1 = affineImage(A,C) \nC1 = affineImage(C,b)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "C" => Cone,
	  "b" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "C1" => {" of class ",TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, TT "A"," must be a matrix from the ambient space of the cone ",TT "C"," to some 
     other target space and ",TT "b"," must be a vector in that target space, i.e. the number of 
     columns of ",TT "A"," must equal the ambient dimension of ",TT "C"," and ",TT "A"," and ",TT "b"," 
     must have the same number of rows. ",TT "affineImage"," then computes the 
     polyhedron ",TT "{(A*c)+b | c in C}"," and the cone ",TT "{A*c | c in C}"," when ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is the omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "which can be send to the positive orthant:",
     
     EXAMPLE {
	  " A = matrix  {{-5,7,1},{1,-5,7},{7,1,-5}}", 
	  " C1 = affineImage(A,C)",
	  " rays C1",
	  }
     
     }

document {
     Key => {(affineImage,Matrix,Polyhedron,Matrix), (affineImage,Matrix,Polyhedron), 
	  (affineImage,Polyhedron,Matrix)},
     Headline => "computes the affine image of a polyhedron",
     Usage => " P1 = affineImage(A,P,v) \nP1 = affineImage(A,P) \nP1 = affineImage(P,v)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "P" => Polyhedron,
	  "v" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "A"," must be a matrix from the ambient space of the polyhedron ",TT "P"," to some 
     other target space and ",TT "v"," must be a vector in that target space, i.e. the number of 
     columns of ",TT "A"," must equal the ambient dimension of ",TT "P"," and ",TT "A"," and ",TT "v"," 
     must have the same number of rows. ",TT "affineImage"," then computes the 
     polyhedron ",TT "{(A*p)+v | p in P}"," where ",TT "v"," is set to 0 if omitted and ",TT "A"," is the 
     identity if omitted.",
     
     PARA{}, "For example, consider the following two dimensional polytope",
     
     EXAMPLE {
	  " P = convexHull matrix {{-2,0,2,4},{-8,-2,2,8}}",
	  },
     
     PARA{}, "which is of course nothing more than an affine image of the square:",
     
     EXAMPLE {
	  " A = matrix {{-5,2},{3,-1}}",
	  " v = matrix {{5},{-3}}",
	  " Q = affineImage(A,P,v)",
	  " vertices Q",
	  }
          
     }

document {
     Key => affinePreimage,
     Headline => "computes the affine preimage of a cone or polyhedron"
     }

document {
     Key => {(affinePreimage,Matrix,Cone,Matrix), (affinePreimage,Matrix,Cone), 
	  (affinePreimage,Cone,Matrix)},
     Headline => "computes the affine preimage of a cone",
     Usage => " C1 = affinePreimage(A,C,b) \nC1 = affinePreimage(A,C) \nC1 = affinePreimage(C,b)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "C" => Cone,
	  "b" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "C1" => {" of class ",TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, TT "A"," must be a matrix from some source space to the ambient space of ",TT "C"," and ",TT "b"," must be 
     a vector in that ambient space, i.e. the number of rows of ",TT "A"," must equal the ambient dimension of ",TT "C"," 
     and the number of rows of ",TT "b",". ",TT "affinePreimage"," then computes the 
     polyhedron ",TT "{q | (A*q)+b in C}"," and the cone ",TT "{q | (A*q) in C}"," when ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is the omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "and its preimage under the following map:",
     
     EXAMPLE {
	  " A = matrix  {{-5,7,1},{1,-5,7},{7,1,-5}}", 
	  " C1 = affinePreimage(A,C)",
	  " rays C1",
	  }
     
     }

document {
     Key => {(affinePreimage,Matrix,Polyhedron,Matrix), (affinePreimage,Matrix,Polyhedron),  
	  (affinePreimage,Polyhedron,Matrix)},
     Headline => "computes the affine preimage of a polyhedron",
     Usage => " P1 = affinePreimage(A,P,v) \nP1 = affinePreimage(A,P) \nP1 = affinePreimage(P,v)",
     Inputs => {
	  "A" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ},
	  "P" => Polyhedron,
	  "v" => Matrix => {" with entries in ",TO ZZ," or ",TO QQ," and only one column representing a vector"}
	  },
     Outputs => {
	  "P1" => Polyhedron
	  },
     
     PARA{}, TT "A"," must be a matrix from some source space to the ambient space of the polyhedron ",TT "P"," 
     and ",TT "v"," must be a vector in that ambient space, i.e. the number of 
     rows of ",TT "A"," must equal the ambient dimension of ",TT "P"," and the number of rows 
     of ",TT "v",". ",TT "affinePreimage"," then computes the polyhedron ",TT "{q | (A*q)+v in P}"," 
     where ",TT "v"," is set to 0 if omitted and ",TT "A"," is the identity if omitted.",
     
     PARA{}, "For example, consider the following two dimensional polytope",
     
     EXAMPLE {
	  " P = convexHull matrix {{-2,0,2,4},{-8,-2,2,8}}",
	  },
     
     PARA{}, "and its affine preimage under the following map:",
     
     EXAMPLE {
	  " A = matrix {{-5,2},{3,-1}}",
	  " v = matrix {{5},{-3}}",
	  " Q = affinePreimage(A,P,v)",
	  " vertices Q",
	  }
     
     }

document {
     Key => {bipyramid, (bipyramid,Polyhedron)},
     Headline => "computes the bipyramid over a polyhedron",
     Usage => " Q = bipyramid P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "The ",TT "bipyramid"," over a ",TO Polyhedron," in n-space is constructed by 
     embedding the Polyhedron into (n+1)-space, computing the barycentre of the vertices, 
     which is a point in the relative interior, and taking the convex hull of the embedded 
     Polyhedron and the barycentre ",TT "x {+/- 1}",".",
     
     PARA{}, "As an example, we construct the octahedron as the bipyramide over the square 
     (see ",TO hypercube,").",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " Q = bipyramid P",
	  " vertices Q",
	  }
     
     }

document {
     Key => {ccRefinement, (ccRefinement,Matrix)},
     Headline => "computes the coarsest common refinement of a set of rays",
     Usage => " F = ccRefinment R",
     Inputs => {
	  "R" => Matrix
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The coarsest common refinement of a set of rays ",TT "R"," is the common refinement 
     of all possible triangulations  of the rays.",
     
     PARA{}, "for example, consider a three dimensional cone with four rays:",
     
     EXAMPLE {
	  " R = matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}"
	  },
     
     PARA{}, "The coarsest common refinement has a fifth ray and consists of four cones.",
     
     EXAMPLE {
	  " F = ccRefinement R",
	  " rays F"
	  }
     
     }

document {
     Key => {coneToPolyhedron, (coneToPolyhedron,Cone)},
     Headline => "converts a cone to class Polyhedron",
     Usage => " P = coneToPolyhedron C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "Every ",TO Cone," is in particular a ",TO Polyhedron,". ",TT "coneToPolyhedron"," 
     converts the cone into the same cone but of class ",TO Polyhedron,"."
     
     }

document {
     Key => directProduct,
     Headline => "computes the direct product of two convex objects",
     
     }

document {
     Key => {(directProduct,Cone,Cone), (directProduct,Cone,Polyhedron), 
	  (directProduct,Polyhedron,Cone), (directProduct,Polyhedron,Polyhedron)},
     Headline => "computes the direct product of polyhedra and cones",
     Usage => " P = directProduct(X,Y)",
     Inputs => {
	  "X" => {TO Cone," or ",TO Polyhedron},
	  "Y" => {TO Cone," or ",TO Polyhedron}
	  },
     Outputs => {
	  "P" => {TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, "The ", TT "directProduct"," of ",TT "X"," and ",TT "Y"," is the polyhedron 
     ",TT "{(x,y) | x in X, y in Y}"," in the direct product of the ambient spaces. If 
     ",TT "X"," and ",TT "Y"," are both cones, then the direct product is again a cone 
     and the output is then also given as a ",TO Cone,", otherwise as a ",TO Polyhedron,".",
     
     EXAMPLE {
	  " P = hypercube 1",
	  " Q = hypercube 2",
	  " directProduct(P,Q) == hypercube 3"
	  },
     
     PARA{}, "See also ",TO (symbol *,Cone,Cone),", ",TO (symbol *,Cone,Polyhedron),", ",
              TO (symbol *,Polyhedron,Cone),", and ",TO (symbol *,Polyhedron,Polyhedron),"."
     
     }

document {
     Key => (directProduct,Fan,Fan),
     Headline => "computes the direct product of two fans",
     Usage => " F = directProduct(F1,F2)",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "directProduct"," of two fans is the fan given by ",TT "C = C1 x C2"," 
     for all cones ",TT "C1 in F1"," and ",TT "C2 in F2"," in the direct product of the 
     ambient spaces.",
     
     EXAMPLE {
	  " F1 = normalFan hypercube 1",
	  " F2 = normalFan hypercube 2",
	  " F = directProduct(F1,F2)",
	  " F == normalFan hypercube 3"
	  },
     
     PARA{}, "See also ", TO (symbol *,Fan,Fan),"."
     
     }

document {
     Key => {dualCone, (dualCone,Cone)},
     Headline => " computes the dual Cone",
     Usage => " Cv = dualCone C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "Cv" => Cone
	  },
     
     PARA{}, "The dual cone of ",TT "C"," in ",TO QQ,"^n is the cone in the dual ambient 
     space (",TO QQ,"^n)^*, given 
     by ",TT "{p in (",TO QQ,TT "^n)^* | p*c >= 0 for all c in C}",".",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2},{2,1}}",
	  " Cv = dualCone C",
	  " rays Cv"
	  }
     
     }

document {
     Key => { imageFan, (imageFan,Matrix,Cone)},
     Headline => " computes the fan of the image",
     Usage => " F = imageFan(M,C)",
     Inputs => {
	  "M" => Matrix,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, TT "M"," must be a matrix from the ambient space of the ",TO Cone," ",TT "C"," to some 
     target space. The ",TT "imageFan"," is the common refinement of the images of all faces of ",TT "C",".",
     
     EXAMPLE {
	  " C = posHull matrix {{2,1,-1,-3},{1,1,1,1},{0,1,-1,0}}",
	  " M = matrix {{1,0,0},{0,1,0}}",
	  " F = imageFan(M,C)",
	  " rays F"
	  }
     
     }

document {
     Key => { minkowskiSum, (minkowskiSum,Cone,Cone), (minkowskiSum,Cone,Polyhedron), 
	  (minkowskiSum,Polyhedron,Cone), (minkowskiSum,Polyhedron,Polyhedron)},
     Headline => " computes the Minkowski sum of two convex objects",
     Usage => " Q = minkowskiSum(X,Y)",
     Inputs => {
	  "X" => {TO Cone," or ",TO Polyhedron},
	  "Y" => {TO Cone," or ",TO Polyhedron}
	  },
     Outputs => {
	  "Q" => {TO Cone," or ",TO Polyhedron}
	  },
     
     PARA{}, "The Minkowski sum of ",TT "X"," and ",TT "Y"," is the polyhdedron 
     ",TT "X + Y = {x + y | x in X, y in Y}",". If ",TT "X"," and ",TT "Y"," are both 
     cones, then their Minkowski sum is their positive hull which is a cone, so the 
     output is a ",TO Cone,". Otherwise the output is a ",TO Polyhedron,". ",TT "X"," 
     and ",TT "Y"," have to lie in the same ambient space.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{0,1,-1},{0,-1,-1}}",
	  " P2 = convexHull matrix {{0,1,-1},{0,1,1}}",
	  " Q = minkowskiSum(P1,P2)",
	  " vertices Q"
	  },
     
     PARA{}, "See also ",TO (symbol +,Cone,Cone),", ",TO (symbol +,Cone,Polyhedron),", ",
              TO (symbol +,Polyhedron,Cone),", and ",TO (symbol +,Polyhedron,Cone),"."
     
     }

document {
     Key => {normalFan, (normalFan,Polyhedron)},
     Headline => "computes the normalFan of a polyhedron",
     Usage => " F = normalFan P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "normalFan"," of a ",TO Polyhedron," is the fan generated by the 
     cones ",TT "C_v"," for all vertices ",TT "v"," of the ",TO Polyhedron,", 
     where ",TT "C_v"," is the dual Cone of the positive Hull of ",TT "P-v",". 
     If ",TT "P"," is compact, i.e. a polytope, then the normalFan is complete.",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0,0},{0,1,0}}",
	  " F = normalFan P",
	  " apply(genCones F,rays)"
	  }
     
     }

document {
     Key => {polar, (polar,Polyhedron)},
     Headline => " computes the polar of a polyhedron",
     Usage => " Pv = polar P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Pv" => Polyhedron
	  },
     
     PARA{}, "The polar polyhedron of ",TT "P"," in n-space is the polyhedron in the dual 
     space given by ",TT "{v in (QQ^n)^* | v*p >= -1 for all p in P}",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " Q = polar P",
	  " Q == crossPolytope 3"
	  }
     
     }

document {
     Key => {pyramid, (pyramid,Polyhedron)},
     Headline => "computes the pyramid over a polyhedron",
     Usage => " Q = pyramid P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, TT "pyramid"," takes the polyhedron ",TT "P"," with ambient dimension n 
     and embeds it into ",TO QQ,"^(n+1) on height 0 with respect to the new last variable. 
     Then it computes the convex hull of the embedded ",TT "P"," and the point (0,...,0,1).", 
     
     EXAMPLE {
	  " P = hypercube 2",
	  " Q = pyramid P",
	  " vertices Q"
	  }
     
     }

document {
     Key =>  {crossPolytope, (crossPolytope,ZZ), (crossPolytope,ZZ,QQ), (crossPolytope,ZZ,ZZ)},
     Headline => "computes the d-dimensional crosspolytope with diameter 2s",
     Usage => " P = crossPolytope(d,s)",
     Inputs => {
	  "d" => ZZ => {" strictly positive"},
	  "s" => {TO ZZ," or ",TO QQ,", positive (optional)"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional ",TT "crossPolytope"," with diameter ",TT "s"," is the 
     convex hull of ",TT "+/- s"," times the standard basis in ",TO QQ,"^d. If ",TT "s"," is omitted 
     it is set to 1.",
     
     EXAMPLE {
	  " P = crossPolytope(3,3/2)",
	  " vertices P"
	  }
     
     }

document {
     Key => {cyclicPolytope, (cyclicPolytope,ZZ,ZZ)},
     Headline => "computes the d dimensional cyclic polytope with n vertices",
     Usage => " P = cyclicPolytope(d,n)",
     Inputs => {
	  "d" => ZZ => {"strictly positive"},
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional ",TT "cyclicPolytope"," with ",TT "n"," vertices 
     is the convex hull of ",TT "n"," points on the moment curve in ",TO QQ,"^",TT "d",". The 
     moment curve is defined by ",TT "t -> (t,t^2,...,t^d)"," and the function takes the 
     points ",TT "{0,...,n-1}",".",
     
     EXAMPLE {
	  " P = cyclicPolytope(3,5)",
	  " vertices P"
	  }
     
     }

document {
     Key => {emptyPolyhedron, (emptyPolyhedron,ZZ)},
     Headline => "generates the empty polyhedron in n-space",
     Usage => " P = emptyPolyhedron n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "Generates the empty polyhedron in ",TT "n","-space.",
     
     EXAMPLE {
	  " P = emptyPolyhedron 3"
	  }
     
     }

document {
     Key => {hirzebruch, (hirzebruch,ZZ)},
     Headline => "computes the fan of the r-th Hirzebruch surface",
     Usage => " F = hirzebruch r",
     Inputs => {
	  "r" => ZZ => {"positive"}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The ",TT "r","-th Hirzebruch surface is the ",TO Fan," in ",TO QQ,"^2 generated 
     by the cones <e_1,e_2>, <e_1,-e_2>, <-e_1+r*e_2,-e_2> and <-e_1+r*e_2,e_2>.",
     
     EXAMPLE {
	  " F = hirzebruch 3",
	  " apply(genCones F,rays)"
	  }
     
     }

document {
     Key => {hypercube, (hypercube,ZZ), (hypercube,ZZ,QQ), (hypercube,ZZ,ZZ)},
     Headline => "computes the d-dimensional hypercube with edge length 2*s",
     Usage => " P = hypercube(d,s)",
     Inputs => {
	  "d" => ZZ => {", strictly positive"},
	  "s" => {TO ZZ," or ",TO QQ,", positive (optional)"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional ",TT "hypercube"," with edge length 2*",TT "s"," is 
     the convex hull of all points in ",TT "{+/- s}^d"," in ",TO QQ,"^d. If ",TT "s"," is omitted 
     it is set to 1.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " vertices P"
	  }
     
     }

document {
     Key => {newtonPolytope, (newtonPolytope,RingElement)},
     Headline => "computes the Newton polytope of a polynomial",
     Usage => "P = newtonPolytope f",
     Inputs => {
	  "f" => RingElement
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "newtonPolytope"," of ",TT "f"," is the convex hull of its 
     exponent vectors in n-space, where n is the number of variables in the ring.",
     
     }

document {
     Key => {posOrthant, (posOrthant,ZZ)},
     Headline => "generates the positive orthant in n-space",
     Usage => " C = posOrthant n",
     Inputs => {
	  "n" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Generates the positive orthant in the n dimensional space as a cone.",
     
     EXAMPLE {
	  " C = posOrthant 3",
	  " rays C"
	  }
     }

document {
     Key => {statePolytope, (statePolytope,Ideal)},
     Headline => "computes the state polytope of a homogeneous ideal",
     Usage => " P = statePolytope I",
     Inputs => {
	  "I" => Ideal => {"which must be homogeneous"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "A ",TT "statePolytope"," of an Ideal ",TT "I"," has as normalFan 
     the Groebner fan of the ideal. We use the construction by Sturmfels, see Algorithm 3.2 in ", 
     HREF("http://math.berkeley.edu/~bernd/index.html", "Bernd Sturmfels'"), " ", EM "Groebner Bases and 
     Convex Polytopes", ", volume 8 of University Lecture Series. American Mathematical Society, 
     first edition, 1995."
     
     }

document {
     Key => {stdSimplex, (stdSimplex,ZZ)},
     Headline =>  "generates the d-dimensional standard simplex",
     Usage => " P = stdSimplex d",
     Inputs => {
	  "d" => ZZ => {"strictly positive"}
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "The ",TT "d","-dimensional standard simplex is the convex hull of the 
     standard basis in ",TO QQ,"^(d+1).",
     
     EXAMPLE {
	  " P = stdSimplex 2",
	  " vertices P"
	  }
     
     }

document {
     Key => (symbol *,Cone,Cone),
     Headline => "computes the direct product of two cones",
     Usage => "  C = C1 * C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Computes the direct product of ",TT "C1"," and ",TT "C2",", which is the cone 
     ",TT "{(x,y) | x in C1, y in C2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO directProduct,"."
     
     }

document {
     Key => (symbol *,Cone,Polyhedron),
     Headline => "computes the direct product of a cone and a polyhedron",
     Usage => "  Q = C * P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "C"," and ",TT "P",", which is the 
     polyhedron ",TT "{(c,p) | c in C, p in P}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO directProduct,"."
     
     }

document {
     Key => (symbol *,Polyhedron,Cone),
     Headline => "computes the direct product of a polyhedron and a cone",
     Usage => "  Q = P * C",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "P"," and ",TT "C",", which is the polyhedron 
     ",TT "{(p,c) | p in P, x in C}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO directProduct,"."
     
     }

document {
     Key => (symbol *,Polyhedron,Polyhedron),
     Headline => "computes the direct product of two polyhedra",
     Usage => "  Q = P1 * P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the direct product of ",TT "P1"," and ",TT "P2",", which is the polyhedron 
     ",TT "{(x,y) | x in P1, y in P2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO directProduct,"."
     
     }

document {
     Key => (symbol *,Fan,Fan),
     Headline => "computes the direct product",
     Usage => "  F = F1 * F2",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Computes the direct product of two fans, which is the fan given by ",TT "C=C1 x C2"," 
     for all cones ",TT "C1 in F1"," and ",TT "C2 in F2",", in the direct product of the 
     ambient spaces.",
     
     PARA{}, "See also ",TO (directProduct,Fan,Fan),"."
          
     }

document {
     Key => (symbol +,Cone,Cone),
     Headline => "computes the Minkowski sum of two cones",
     Usage => "  C = C1 + C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "C1"," and ",TT "C2",", which is the cone 
     ",TT "C1 + C2 = {x + y | x in C1, y in C2}",". Note that ",TT "C1"," and ",TT "C2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,"."
     
     }

document {
     Key => (symbol +,Cone,Polyhedron),
     Headline => "computes the Minkowski sum of a cone and a polyhedron",
     Usage => "  Q = C + P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "C"," and ",TT "P",", which is the polyhedron 
     ",TT "C + P = {c + p | c in C, p in P}",". Note that ",TT "C"," and ",TT "P"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,"."
     
     }

document {
     Key => (symbol +,Polyhedron,Cone),
     Headline => "computes the Minkowski sum of a polyhedron and a cone",
     Usage => "  Q = P + C",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "P"," and ",TT "C",", which is the polyhdedron 
     ",TT "P + C = {p + c | p in P, c in C}",". Note that ",TT "P"," and ",TT "C"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,"."
     
     }

document {
     Key => (symbol +,Polyhedron,Polyhedron),
     Headline => "computes the Minkowski sum of two polyhedra",
     Usage => "  Q = P1 + P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Computes the Minkowski sum of ",TT "P1"," and ",TT "P2",", which is the polyhdedron 
     ",TT "P1 + P2 = {x + y | x in P1, y in P2}",". Note that ",TT "P1"," and ",TT "P2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,"."
          
     }

document {
     Key => (symbol ==,Cone,Cone),
     Headline => "equality",
     Usage => " C1 == C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  }
     
     }

document {
     Key => (symbol ==,Fan,Fan),
     Headline => "equality",
     Usage => " F1 == F2",
     Inputs => {
	  "F1" => Fan,
	  "F2" => Fan
	  }
     
     }

document {
     Key => (symbol ==,Polyhedron,Polyhedron),
     Headline => "equality",
     Usage => " P1 == P2",
     Inputs => {
	  "P1" => Polyhedron,
	  "P2" => Polyhedron
	  }
     
     }

document {
     Key => (dim,Cone),
     Headline => "computes the dimension of a cone",
     Usage => " d = dim C",
     Inputs => {
	  "C" => Cone
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, "Returns the dimension of a cone."
     
     }

document {
     Key => (dim,Fan),
     Headline => "computes the dimension of a fan",
     Usage => " d = dim F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, "Returns the dimension of a fan. This 
     is the maximal dimension of any of the cones of 
     the fan."
     
     }

document {
     Key => (dim,Polyhedron),
     Headline => "computes the dimension of a polyhedron",
     Usage => " d = dim P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, "Returns the dimension of a polyhedron."
     
     }

document {
     Key => (net,Cone),
     Headline => "displays characteristics of a cone",
     Usage => " net C",
     Inputs => {
	  "C" => Cone
	  },
          
     PARA{}, "Displays an overview of the properties of the 
     cone, the ambient dimension, the dimension of the lineality 
     space, the dimension of the cone, the number of facets, and 
     the number of rays.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{2,3,1},{3,1,2},{1,0,1}};",
	  " net C"
	  }
     
     }

document {
     Key => (net,Fan),
     Headline => "displays characteristics of a fan",
     Usage => " net F",
     Inputs => {
	  "F" => Fan
	  },
     
     PARA{}, "Displays an overview of the properties of the 
     Fan, the ambient dimension, the number of generating 
     cones, the number of rays, and the top dimension of 
     the cones.",
     
     EXAMPLE {
	  " F = normalFan cyclicPolytope(3,5);",
	  " net F"
	  }
     
     }

document {
     Key => (net,Polyhedron),
     Headline => "displays characteristics of a polyhedron",
     Usage => " net P",
     Inputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "Displays an overview of the properties of the 
     Polyhedron, the ambient dimension, the dimension of the 
     lineality space, the dimension of the polyhedron, the 
     number of facets, the number of rays, and the number of 
     vertices.",
     
     EXAMPLE {
	  " P = cyclicPolytope(4,6);",
	  " net P"
	  }
     
     }

document {
     Key => {saveSession,(saveSession,String)},
     Headline => "save the actual Polyhedra session to a file",
     Usage => " saveSession F",
     Inputs => {
	  "F" => String
	  },
     
     PARA{}, "All convex polyhedral objects (",TO Cone,",",TO Fan,",",TO Polyhedron,") that have been assigned to
     to a ",TO Symbol," will be saved into the file ",TT "F",". If the package ",TT "PPDivisor"," is loaded, then 
     also all ",TT "PolyhedralDivisors"," are saved into ",TT "F",". Also every ",TO List," or ",TO Sequence," 
     containing any of the above types or lists and sequences of them in arbitrary nested depth is saved.",
     
     PARA{}, "To recover the session simply call ",TT "load F",". It is not necessary that ",TT "Polyhedra"," is already 
     loaded (if not, it will be) and also ",TT "PPDivisor"," is loaded if it was loaded when the session had been saved."
     
     }

document {
     Key => {coneBuilder,(coneBuilder,Sequence,Sequence)},
     Headline => "Internal function, not for public use",
     
     }

document {
     Key => {fanBuilder,(fanBuilder,List,List,List,List)},
     Headline => "Internal function, not for public use",
     
     }

document {
     Key => {polyhedronBuilder,(polyhedronBuilder,Sequence,Sequence)},
     Headline => "Internal function, not for public use",
     
     }



TEST ///

P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
assert(P#"numVertices" == 3)
assert(dim(P) == 2)
assert(ambDim(P) == 3)
assert(rays(P) == 0)
assert(linSpace(P) == 0)
M = matrix {{3_QQ,4,1}};
v = matrix {{10_QQ}};
assert((hyperplanes(P) == (M,v)) or (hyperplanes(P) == (-M,-v)))
///

TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
P = convexHull {P,(matrix{{4},{0},{-2}},matrix{{1,0,0},{0,1,-1},{0,0,0}})};
assert(dim(P) == 3)
assert(image(linSpace(P)) == image(matrix {{0},{1_QQ},{0}}))
assert(hyperplanes(P) == (0,0))
///

TEST ///
P = convexHull (matrix{{1},{1}},matrix{{1,0},{0,1}});
M1 = matrix {{-1,0_QQ},{0,-1}};
M2 = matrix {{0,-1_QQ},{-1,0}};
v = matrix {{-1},{-1_QQ}};
assert((halfspaces(P) == (M1,v)) or (halfspaces(P) == (M2,v)))
///

TEST ///
P2 =  convexHull matrix {{1,-2,-1,2},{2,1,-2,-1}};
M = matrix{{3,1},{-3,-1},{1,-3},{-1,3}};
v = matrix{{5},{5},{5},{5}};
assert(intersection(M,v) == P2)
///

TEST ///
P = intersection (matrix{{1,0},{0,1},{-1,0},{0,-1}},matrix{{1},{2},{3},{4}});
V1 = vertices(P);
V1 = set apply(numColumns V1, i -> (V1_{i}));
V2 = set {matrix{{1_QQ},{2}},matrix{{1_QQ},{-4}},matrix{{-3_QQ},{2}},matrix{{-3_QQ},{-4}}};
assert(isSubset(V1,V2) and isSubset(V2,V1))
///

TEST ///
C = intersection matrix {{1,2},{2,1}};
R1 = rays C;
R1 = set apply(numColumns R1, i -> (R1_{i}));
R2 = set {matrix{{2_QQ},{-1}},matrix{{-1_QQ},{2}}};
assert(isSubset(R1,R2) and isSubset(R2,R1))
assert(linSpace(C) == 0)
assert(dim(C) == 2)
assert(ambDim(C) == 2)
///

TEST ///
C = intersection matrix {{1,2,1},{2,1,1}};
assert(image(linSpace(C)) == image(matrix{{1_QQ},{1_QQ},{-3}}))
assert(ambDim(C) == 3)
///

TEST ///
C = posHull (matrix{{1,0},{0,1},{0,0}},matrix{{0},{0},{1}});
assert((halfspaces(C) == matrix{{1_QQ,0,0},{0,1,0}}) or (halfspaces(C) == matrix{{0_QQ,1,0},{1,0,0}}))
assert(C#"numRays" == 2)
///

TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
F1 = faces(1,P);
F2 = {convexHull matrix{{-1,0},{1,1}},convexHull matrix{{0,1},{1,0}},convexHull matrix{{1,1},{0,-1}},convexHull matrix{{1,0},{-1,-1}},convexHull matrix{{0,-1},{-1,0}},convexHull matrix{{-1,-1},{0,1}}};
assert(equalLists(F1,F2))
(C,L,M) = minkSummandCone(P);
assert(rays(C)*M == matrix{{1_QQ,1},{1,1},{1,1},{1,1},{1,1},{1,1}})
L1 = {convexHull matrix{{0,1},{0,0}},convexHull matrix{{0,0},{0,1}},convexHull matrix{{0,1},{0,-1}},convexHull matrix{{0,0,1},{0,1,0}},convexHull matrix{{0,1,1},{0,0,-1}}};
assert(equalLists(values L,L1))
///

TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
P = bipyramid(P);
F1 = set apply(faces(3,P), f -> (vertices(f)));
F2 = set {matrix{{-1_QQ},{0},{0}},matrix{{1_QQ},{0},{0}},matrix{{0_QQ},{1},{0}},matrix{{0_QQ},{-1},{0}},matrix{{1_QQ},{-1},{0}},matrix{{-1_QQ},{1},{0}},matrix{{0_QQ},{0},{1}},matrix{{0_QQ},{0},{-1}}};
assert(isSubset(F1,F2) and isSubset(F2,F1))
assert(fVector(P) == {8,18,12,1})
///

end