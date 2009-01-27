--*- coding: utf-8 -*-
---------------------------------------------------------------------------
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : April 2008
---------------------------------------------------------------------------
newPackage("Polyhedra",
    Headline => "A package for computations with convex polyhedra",
    Version => "0.5",
    Date => "December 10, 2008",
    Authors => {
         {Name => "René Birkner", Email => "rbirkner@mi.fu-berlin.de"}},
    DebuggingMode => true,
    AuxiliaryFiles=>true,
    Configuration => {}
    )

export {Polyhedron, Cone, Fan, convexHull, posHull, intersection, makeFan, addCone,
        symmDiff, union, 
        ambdim, cones, genCones, halfspaces, hyperplanes, linspace, rays, vertices,
        areCompatible, commonFace, contains, equals, isCompact, isComplete, isEmpty, isFace, isPointed, isProjective, isPure, isSmooth,
	faces, fVector, hilbertBasis, inInterior, interiorPoint, interiorVector, latticePoints, minkSummandCone, skeleton, smallestFace, 
	smoothSubfan, tailCone, vertexEdgeMatrix, vertexFacetMatrix,
	affineHull, affineImage, bipyramid, coneToPolyhedron, directProduct,  dualCone, 
	minkowskiSum, normalfan, polar, pyramid, 
	crosspolytope, cyclicPolytope, hirzebruch, hypercube, newtonPolytope, statePolytope, stdSimplex}

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
net Polyhedron := P -> ( info:= hashTable{"ambient dimension" => P#"ambientdimension",
							"dimension of polyhedron" => P#"polyhedronDimension",
							"dimension of lineality space" => P#"linealityspacedimension",
							"number of facets" => P#"facetsnr", 
							"number of vertices" => P#"verticesnr",
							"number of rays" => P#"raysnr"};
				horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply(pairs info,(k,v) -> (net k, " => ", net v))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( info:= hashTable{"ambient dimension" => C#"ambientDimension",
							"dimension of the cone" => C#"coneDimension",
							"dimension of lineality space" => C#"linSpaceDim",
							"number of facets" => C#"facetsnr", 
							"number of rays" => C#"raysnr"};
				horizontalJoin flatten ( 
				"{", 
	  			-- the first line prints the parts vertically, second: horizontally
				stack (horizontalJoin \ sort apply(pairs info,(k,v) -> (net k, " => ", net v))),
				-- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
				"}" ))


-- Modifying the standard output for a Cone to give an overview of its characteristica
net Fan := F -> ( info:= hashTable{"ambient dimension" => F#"ambientDimension",
							"top dimension of the cones" => F#"topDimension",
							"number of generating cones" => F#"noGenCones",
							"number of rays" => F#"noRays"};
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
polyhedronBuilder := (hyperA,verticesA) -> (
        -- Checking if the polyhedron is empty
	test = matrix join({{1}},toList((numgens target verticesA#0)-1:{0_QQ}));
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
	test := matrix join({{-1}},toList((numgens target (hyperA#0))-1:{0_QQ}));
	scan(numgens source (hyperA#0), i -> ( if (test=!=(hyperA#0)_{i}) then H=H || transpose((hyperA#0)_{i})));
	-- Determine the lineality space
	LS := verticesA#1;
	LS = LS^{1..(numgens target LS)-1};
	-- Determine the defining hyperplanes
	HP := transpose(hyperA#1);
	HP = (HP_{1..(numgens source HP)-1},(-1)*HP_{0});
	-- Defining P
	P := new Polyhedron;
	P#"ambientdimension" = (numgens target B)-1;
	P#"polyhedronDimension" =  ((numgens target B)-1)-(rank(hyperA#1));
	P#"linealityspacedimension" = numgens source LS;
	P#"linealityspace" = LS;
	P#"verticesnr" = numgens source B;
	P#"raysnr" = numgens source C;
	P#"vertices" = B^{1..(numgens target B)-1};
	P#"rays" = C^{1..(numgens target C)-1};
	P#"facetsnr" = numgens target H;
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
coneBuilder := (genrays,dualgens) -> (
      -- Sorting into rays, lineality space generators, supporting halfspaces, and hyperplanes
	RM := genrays#0;
	LS := genrays#1;
	HS := transpose((-1)*(dualgens#0));
	HP := transpose(dualgens#1);
	-- Defining C
	C := new Cone;
	C#"ambientDimension" = numgens target RM;
	C#"coneDimension" = (numgens target RM)-(rank HP);
	C#"linSpaceDim" = numgens source LS;
	C#"linealityspace" = LS;
	C#"raysnr" = numgens source RM;
	C#"rays" = RM;
	C#"facetsnr" = numgens target HS;
	C#"halfspaces" = HS;
	C#"hyperplanes" = HP;
	C#"genrays" = genrays;
	C#"dualgens" = dualgens;
	C)




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
	-- Generating the zero ray R
	R := matrix toList(numgens target M:{0_QQ});
	convexHull(M,R))


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if ((P1#"ambientdimension") =!= (P2#"ambientdimension")) then (
	     error ("Polyhedra must lie in the same ambientspace"));
	M := (P1#"homogenizedVertices")#0 | (P2#"homogenizedVertices")#0;
	LS := (P1#"homogenizedVertices")#1 | (P2#"homogenizedVertices")#1;
	hyperA := fourierMotzkin(M,LS);
	verticesA := fourierMotzkin hyperA;
	polyhedronBuilder(hyperA,verticesA))



-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
posHull = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'linspace'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining halfspaces and hyperplanes.
posHull(Matrix,Matrix) := (Mrays,linspace) -> (
	-- checking for input errors
     	if ((numgens target Mrays) =!= (numgens target linspace)) then (
	     error ("rays and linspace generators must lie in the same space"));
	local Mr;
	local Mls;
	R := ring source Mrays;
	if (R === ZZ) then (
		Mr = substitute(Mrays, QQ))
	else if (R === QQ) then (
		Mr = Mrays)
	else error ("expected rays over ZZ or QQ");
	R = ring source linspace;
	if (R === ZZ) then (
		Mls = substitute(linspace, QQ))
	else if (R === QQ) then (
		Mls = linspace)
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
	M := (C1#"rays") | (C2#"rays");
	LS := (C1#"linealityspace") | (C2#"linealityspace");
	dualgens := fourierMotzkin(M,LS);
	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))


--   INPUT : '(C1,C2)'  two cones
posHull(Polyhedron) := (P) -> (
     Mrays := (P#"vertices") | (P#"rays");
     Mlinspace := P#"linealityspace";
     posHull(Mrays,Mlinspace))



-- PURPOSE : Computing a polyhedron as the intersection of affine halfspaces and hyperplanes
intersection = method(TypicalValue => Polyhedron)

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


--   INPUT : '(M,N)',  two matrices (although v is only a vector), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix) := (M,N) -> (
	-- Checking for input errors
	if ((((numgens source M) =!= (numgens source N)) and ((numgens source N) =!= 1)) or 
		(((numgens source N) == 1) and ((numgens target M) =!= (numgens target N)))) then (
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
	if ((numgens source Ml) == (numgens source Nl)) then (
		Ml = (-1)*(transpose Ml);
		Nl = transpose Nl;
		genrays = fourierMotzkin(Ml,Nl);
		dualgens = fourierMotzkin genrays;
		P = coneBuilder(genrays, dualgens))
	else if ( Nl == (0*Nl)) then (
		Ml = (-1)*(transpose Ml);
		genrays = fourierMotzkin Ml;
		dualgens = fourierMotzkin genrays;
		P = coneBuilder(genrays,dualgens))
	else (
		-- Computing generators of the cone and its dual cone
		Ml = ((-1)*Nl) | Ml;
		Ml = (transpose Ml)|(matrix join({{-1}},toList((numgens source Ml)-1:{0_QQ})));
		verticesA := fourierMotzkin Ml;
		hyperA := fourierMotzkin verticesA;
		P = polyhedronBuilder(hyperA,verticesA));
	P)


--   INPUT : '(P1,P2)',  two polyhedra 
--  OUTPUT : 'P', the polyhedron which is the intersection of both
intersection(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- checking if P1 and P2 lie in the same space
	if ((P1#"ambientdimension") =!= (P2#"ambientdimension")) then (
		error ("Polyhedra must lie in the same ambientspace"));
	M := ((halfspaces P1)#0)||((halfspaces P2)#0);
	v := ((halfspaces P1)#1)||((halfspaces P2)#1);
	N := ((hyperplanes P1)#0)||((hyperplanes P2)#0);
	w := ((hyperplanes P1)#1)||((hyperplanes P2)#1);
	intersection(M,v,N,w))


--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
intersection(Matrix) := M -> (
	-- checking for input errors
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
	-- checking if C1 and C2 lie in the same space
	if ((C1#"ambientDimension") =!= (C2#"ambientDimension")) then (
		error ("Cones must lie in the same ambientspace"));
	M := (halfspaces C1)||(halfspaces C2);
	N := (hyperplanes C1)||(hyperplanes C2);
	intersection(M,N))


intersection List := L -> (
     -- Checking for input errors
     C := L#0;
     if (((class C) != Cone) and ((class C) != Polyhedron)) then (
	  error ("The input must be cones and/or polyhedra"));
     n := C#"ambientDimension";
     local M,v,N,w;
     if ((class C) == Cone) then (
	  M = halfspaces C;
	  v = map(QQ^(numgens target halfspaces C),QQ^1,0);
	  N = hyperplanes C;
	  w = map(QQ^(numgens target hyperplanes C),QQ^1,0))
     else (
	  M = (halfspaces C)#0;
	  v = (halfspaces C)#1;
	  N = (hyperplanes C)#0;
	  w = (hyperplanes C)#1);
     L = drop(L,1);
     scan(L, C1 -> (
	       -- Checking for further input errors
	       if (((class C1) != Cone) and ((class C1) != Polyhedron)) then (
		    error ("The input must be cones and/or polyhedra"));
	       if ((ambdim C1) != n) then (
		    error ("All Cones and Polyhedra must be in the same ambient space"));
	       if ((class C1) == Cone) then (
		    M = M || halfspaces C1;
		    v = v || map(QQ^(numgens target halfspaces C1),QQ^1,0);
		    N = N || hyperplanes C1;
		    w = w || map(QQ^(numgens target hyperplanes C1),QQ^1,0))
	       else (
		    M = M || (halfspaces C1)#0;
		    v = v || (halfspaces C1)#1;
		    N = N || (hyperplanes C1)#0;
		    w = w || (hyperplanes C1)#1)));
     if ((v == 0*v) and (w == 0*w)) then (
	  C = intersection(M,N))
     else (
	  C = intersection(M,v,N,w));
     C)
     


makeFan = method(TypicalValue => Fan)
makeFan List := L -> (
     -- Checking for input errors
     if (L=={}) then (error ("List of cones must not be empty"));
     if (class(L#0) != Cone) then ( error ("input must be a list of cones"));
     C := L#0;
     ad := C#"ambientDimension";
     rm := rays C;
     rayList := {};
     scan(numgens source rm, i -> (rayList = append(rayList,rm_{i})));
     F := new Fan;
     F#"generatingCones" = {C};
     F#"ambientDimension" = ad;
     F#"topDimension" = C#"coneDimension";
     F#"noGenCones" = 1;
     F#"rays" = rayList;
     F#"noRays" = #rayList;
     F#"isPure" = true;
     F#"faces" = {};
     F#"isComplete" = false;
     if (ad == C#"ambientDimension") then (
	       F#"faces" = toList symmDiff(F#"faces",faces(1,C));
	       if (F#"faces" == {}) then (F#"isComplete" = true));
     scan(L, C -> (
	       if (class(C) != Cone) then ( error ("input must be a list of cones"));
	       F = addCone(C,F);
	       if (F#?"comperror") then (error ("Cones are not compatible"))));
     F)


makeFan Cone := C -> (
     makeFan({C}));



addCone = method(TypicalValue => Fan)
addCone (Cone,Fan) := (C,F) -> (
     -- Checking for input errors
     if ((C#"ambientDimension") != (F#"ambientDimension")) then (error ("Cones must lie in the same ambient space"));
     GC := F#"generatingCones";
     d := C#"coneDimension";
     inserted := false;
     stopper := false;
     newGC := {};
     i := 0;
     while ((not stopper) and (i < #GC)) do (
	  Cf := GC#i;
	  dimCf := Cf#"coneDimension";
	  local a,b;
	  if (dimCf >= d) then (
	       (a,b) = areCompatible(Cf,C);
	       if (not a) then (
		    newGC = GC;
		    F#"comperror" = {Cf,C};
		    stopper = true)
	       else if (equals(b,C)) then (
		    newGC = GC;
		    stopper = true)
	       else (newGC = append(newGC,Cf)))
	  else (
	       if (not inserted) then (
		    newGC=append(newGC,C);
		    inserted = true);
	       (a,b) = areCompatible(Cf,C);
	       if (not a) then (
		    newGC = GC;
		    F#"comperror" = {Cf,C};
		    stopper = true)
	       else if (not equals(b,Cf)) then (
		    newGC = append(newGC,Cf)));
	  i = i+1);
     if ((not stopper) and (not inserted)) then (
	  newGC = append(newGC,C);
	  inserted = true);
     if inserted then (
	  if (d == C#"ambientDimension") then (
	       F#"faces" = toList symmDiff(F#"faces",faces(1,C));
	       if (F#"faces" == {}) then (F#"isComplete" = true));
	  rayList := F#"rays";
	  rm := rays C;
	  scan(numgens source rm, i -> (rayList = append(rayList,rm_{i})));
	  F#"rays" = unique rayList;
	  F#"noRays" = #(F#"rays"));
     F#"isPure" = (dim(first(newGC)) == dim(last(newGC)));
     F#"generatingCones" = newGC;
     F#"topDimension" = dim(newGC#0);
     F#"noGenCones" = #newGC;
     F)

addCone (Cone,List) := (C,L) -> (
     local F;
     if (L == {}) then (
	  F = makeFan(C))
     else (
	  F = makeFan(prepend(C,L)));
     F)
     

addCone (List,Fan) := (L,F) -> (     
    if (#L == 1) then (F = addCone(L#0,F))
    else if (#L > 1) then ( C := L#0;
	 F = addCone(drop(L,1),addCone(C,F)));
    F)


symmDiff = method(TypicalValue => List)
symmDiff(List,List) := (S1,S2) -> (
     L := {};
     L2 := set {};
     scan(S1, s -> (
	       insert := true;
	       scan(#S2, i -> ( if (s == S2#i) then (
			      insert = false;
			      L2 = L2 + set{i})));
	       if insert then ( L = append(L,s))));
     scan(#S2, i -> (if (not member(i,L2)) then ( L = append(L,S2#i))));
     L)

union = method(TypicalValue => List)
union(List,List) := (L1,L2) -> (
     L := {};
     scan(L1, C -> (
	       insert := true;
	       i := 0;
	       while (insert and (i < #L2)) do (if (C == L2#i) then (insert = false); i=i+1);
	       if insert then L = append(L,C)));
     L2|L)


-- PURPOSE : Giving the defining affine hyperplanes
ambdim = method(TypicalValue => ZZ)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambdim(Polyhedron) := P -> (
	P#"ambientdimension")

--   INPUT : 'C'  a Cone 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambdim(Cone) := C -> (
	C#"ambientDimension")

--   INPUT : 'F'  a Fan 
--  OUTPUT : an integer, which is the dimension of the ambient space
ambdim(Fan) := F -> (
	F#"ambientDimension")



-- PURPOSE : Giving the k dimensionial Cones of the Fan
--   INPUT : (k,F)  where 'k' is a positive integer and F is a Fan 
--  OUTPUT : a List of Cones
cones = method(TypicalValue => List)
cones(ZZ,Fan) := (k,F) -> (
	L := {};
	i := 0;
	stopper := false;
	while (i < F#"noGenCones") and (not stopper) do (
	     C := (F#"generatingCones")#i;
	     if (dim(C) >= k) then (L=union(L,faces(dim(C)-k,C)))
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
halfspaces = method(TypicalValue => (Matrix,Matrix))
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
hyperplanes = method(TypicalValue => (Matrix,Matrix))
hyperplanes(Polyhedron) := P -> (
	P#"hyperplanes")


--   INPUT : 'C'  a Cone
hyperplanes(Cone) := C -> (
	C#"hyperplanes")



-- PURPOSE : Giving a basis of the lineality space
linspace = method(TypicalValue => Matrix)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linspace(Polyhedron) := P -> (
	P#"linealityspace")


--   INPUT : 'C'  a Cone
linspace(Cone) := C -> (
	C#"linealityspace")



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
areCompatible = method(TypicalValue => (Boolean,Cone))

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 
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
	if ((P#"ambientdimension") == (Q#"ambientdimension")) then (
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
      if (P#"ambientdimension" =!= Q#"ambientdimension") then (
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
      -- Checking if rays of C2 satisfy the equations of C1
      scan(flatten entries E, e -> (test = test and (e<=0)));
      test = test and (A*D == 0*A*D) and (B*C == 0*B*C) and (B*D == 0*B*D);
      test)
 

 
-- PURPOSE : Check if 'C' contains 'P'
--   INPUT : '(C,P)'  a Cone and a Polyhedron
contains(Cone,Polyhedron) := (C,P) -> (
      -- checking for input errors
      if (C#"ambientDimension" =!= P#"ambientdimension") then (
	   error ("Cone and Polyhedron must lie in the same ambient space"));
      -- Saving the equations of C and vertices/rays of P
      M := (P#"vertices") | (P#"rays");
      C1:= posHull M;
      contains(C,C1))



-- PURPOSE : Check if 'P' contains 'C'
--   INPUT : '(P,C)'  a Polyhedron and a Cone
contains(Polyhedron,Cone) := (P,C) -> (
      -- checking for input errors
      if (C#"ambientDimension" =!= P#"ambientdimension") then (
	   error ("Polyhedron and Cone must lie in the same ambient space"));
      -- Saving the equations of C and vertices/rays of P
      Q := coneToPolyhedron(C);
      contains(P,Q))



-- PURPOSE : Check if 'P' contains 'p'
--   INPUT : '(P,p)'  a Polyhedron 'P' and a point 'p' given as a matrix
contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if (P#"ambientdimension" =!= (numgens target p)) then (
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



-- PURPOSE : Check if 'L' contains 'C'
--   INPUT : '(L,C)'  a List 'L' and a Cone 'C'
contains(List,Cone) := (L,C) -> (
      i := 0;
      stopper := false;
      while (not stopper) and (i < #L) do (
	   C' := L#i;
	   -- checking for input errors
	   if (class(C') != Cone) then (
		error ("Input must be a list of Cones"));
	   if (ambdim(C) == ambdim(C')) then (stopper = equals(C,C'));
	   i = i+1);
      stopper)
 
 
 -- PURPOSE : Check if 'F' contains 'C'
--   INPUT : '(F,C)'  a Fan 'F' and a Cone 'C'
contains(Fan,Cone) := (F,C) -> (
      -- Checking for input errors
      if (ambdim(F) != ambdim(C)) then (
	   error ("Fan and Cone must lie in the same ambient space"));
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

Cone == Cone := (C1,C2) -> (equals(C1,C2))

Fan == Fan := (F1,F2) -> (equals(F1,F2))


-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact(Polyhedron) := P -> (
     ((P#"linealityspace" == 0) and (P#"rays" == 0)))


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
     if ((P#"ambientdimension") == (Q#"ambientdimension")) then (
	  c := (Q#"polyhedronDimension")-(P#"polyhedronDimension");
	  if (c >= 0) then (
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
     if ((C1#"ambientDimension") == (C2#"ambientDimension")) then (
	  c := (C2#"coneDimension")-(C1#"coneDimension");
	  if (c >= 0) then (
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
     (rank(C#"linealityspace") == 0))


--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     isPointed((F#"generatingCones")#0))



-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isProjective = method(TypicalValue => Boolean)
isProjective(Fan) := F -> (
     test := false;
     if (isComplete(F)) then (
	  i := 0;
	  L := hashTable apply(F#"generatingCones", l -> (i=i+1; i=>l));
	  n := F#"ambientDimension";
	  edges := cones(n-1,F);
	  edgeTCTable := hashTable apply(edges, e -> ( select(1..(#L), j -> (contains(L#j,e))) => e));
	  i = 0;
	  edgeTable := apply(pairs edgeTCTable, e -> (i=i+1; 
		    v := transpose hyperplanes (e#1);
		    if (not (contains(dualCone(L#((e#0)#0)),v))) then (v = (-1)*v);
		    (e#0, e#1, i, v)));
	  edgeTCNoTable := hashTable apply(edgeTable, e -> (e#0 => (e#2,e#3)));
	  edgeTable = hashTable apply(edgeTable, e -> (e#1 => (e#2,e#3)));
	  corrList := {};
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
	  m = #(keys edgeTable);
	  HP = map(QQ^0,QQ^m,0);
	  NM = map(QQ^n,QQ^m,0);
	  scan(#corrList, j -> (
		    v := corrList#j#1;
		    HPnew := NM;
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
			      HPnew = (HPnew_{0..a-2})|(b)|(HPnew_{a..m-1});
			      C1 = C2)
			 else (v = append(v,C2)));
		    C'' := intersection(C,C1);
		    scan(keys edgeTable, k -> ( if (k == C'') then (a,b)=edgeTable#k));
		    if (not contains(dualCone(C),b)) then ( b = (-1)*b);
		    HPnew = (HPnew_{0..a-2})|(b)|(HPnew_{a..m-1});
		    HP = HP || HPnew));
	  v := flatten entries interiorVector intersection(id_(QQ^m),HP);
	  M := {};
	  if (all(v, e -> (e > 0))) then (
	       i = 1;
	       p := map(QQ^n,QQ^1,0);
	       M = {p};
	       Lyes := {};
	       Lno := {};
	       vlist := keys edgeTCTable;
	       edgerecursion := (i,p,vertexlist,Mvertices) -> (
		    vLyes := {};
		    vLno := {};
		    scan(vertexlist, w -> (
			      if (member(i,w)) then (vLyes = append(vLyes,w))
			      else (vLno = append(vLno,w))));
		    scan(vLyes, w -> (
			      j := edgeTCNoTable#w;
			      if (w#0 == i) then (
				   (vLno,Mvertices) = edgerecursion(w#1,p+(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p+(j#1)*(v#((j#0)-1)))))
			      else (
				   (vLno,Mvertices) = edgerecursion(w#0,p-(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p-(j#1)*(v#((j#0)-1)))))));
		    (vLno,Mvertices));
	       M = unique ((edgerecursion(i,p,vlist,M))#1);
	       verts := M#0;
	       scan(1..(#M-1), j -> (verts = verts | M#j));
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
      smooth := false;
      R := rays C;
      C = posHull R;
      if (dim(C) == ambdim(C)) then (
	   if ((numgens source R) == (numgens target R)) then ( smooth = (abs(det(R)) == 1)))
      else (
	   if ((numgens source R) == dim(C)) then (
		A := smithNormalForm(R, ChangeMatrix=>{false,false});
		smooth = (det(A^{0..(numgens source A)-1}) == 1)));
      smooth);
	   

--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isSmooth Fan := F -> (
     smooth := true;
     i := 0;
     while ((i < F#"noGenCones") and (smooth)) do (
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
     -- checking for input errors
     if ((k < 0) or (k > (P#"polyhedronDimension"))) then (
	  error ("invalid dimension"));
     -- defining recursive face builder, that memorizes which halfspaces have already been choosen ('p')
     -- how many halfspaces have already been choosen ('c')
     -- chooses in each step one of the remaining halfspaces (each of them for a new recursion) and
     -- and adds the equation to the hyperplane equations
     -- if the counter 'c' reaches 0 then it checks if the polyhedron has the correct dimension ('d')
     -- and adds it to L
     facerecursion := (p,c,HS,v,HP,w,L,d) -> (
	  if (c == 0) then (
	       test := false;
	       Q := intersection(HS,v,HP,w);
	       if ((Q#"polyhedronDimension") == d) then (
		    scan(L, F -> ( test = test or (equals(F,Q))));
		    if (not test) then L=join(L,{Q})))
	  else ( c = c-1;
	       scan(p..(numgens target HS)-1, i -> (
			 HPnew := HP||(HS^{i});
			 wnew := w||(v^{i});
			 L = facerecursion(i+1,c,HS,v,HPnew,wnew,L,d))));
	  L);
     -- Saving the dimension of the faces
     d :=  (P#"polyhedronDimension")-k;
     counter := k;
     pos := 0;
     (HS,v) := halfspaces P;
     (HP,w) := hyperplanes P;
     L := {};
     if (d == 0) then ( V := vertices P;
	  scan(numgens source V, i -> (L = join(L,{convexHull(V_{i})}))))
     else ( L = facerecursion(pos,counter,HS,v,HP,w,L,d));
     L)



--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a polyhedron
--  OUTPUT : a List, containing the faces as cones
faces(ZZ,Cone) := (k,C) -> (
     -- checking for input errors
     if ((k < 0) or (k > (C#"coneDimension"))) then (
	  error ("invalid dimension"));
     -- defining recursive face builder, that memorizes which halfspaces have already been choosen ('p')
     -- how many halfspaces have already been choosen ('c')
     -- chooses in each step one of the remaining halfspaces (each of them for a new recursion) and
     -- and adds the equation to the hyperplane equations
     -- if the counter 'c' reaches 0 then it checks if the resulting cone has the correct dimension ('d')
     -- and adds it to L
     facerecursion := (p,c,HS,HP,L,d) -> (
	  if (c == 0) then (
	       test := false;
	       Q := intersection(HS,HP);
	       if ((Q#"coneDimension") == d) then (
		    scan(L, F -> ( test = test or (equals(F,Q))));
		    if (not test) then L = join(L,{Q})))
	  else ( c = c-1;
	       scan(p..(numgens target HS)-1, i -> (
			 HPnew := HP||(HS^{i});
			 L = facerecursion(i+1,c,HS,HPnew,L,d))));
	  L);
     -- Saving the dimension of the faces
     d :=  (C#"coneDimension")-k;
     counter := k;
     pos := 0;
     HS := halfspaces C;
     HP := hyperplanes C;
     L := {};
     if (d == 0) then ( M := map(QQ^(C#"ambientDimension"),QQ^1,0);
	  L = {posHull(M)})
     else ( L = facerecursion(pos,counter,HS,HP,L,d));
     L)



-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)
fVector(Polyhedron) := P -> (
     L := {};
     scan((P#"polyhedronDimension")+1, d -> (L = join({#faces(d,P)},L)));
     L)



--   INPUT : 'C'  a Cone
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector(Cone) := C -> (
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
    A = substitute(A,ZZ);
    H := {A^{0}_{0}};
    s := numgens target A;
    n := numgens source A;
    scan(n-1, i -> (
	      F := {};
	      B := transpose A_{0..(i+1)};
	      if (i < (s-1)) then (
		   scan(H, h -> (
			     j := 0;
			     while ((numgens target h) == (i+1)) do (
				  if (isSubset(image(h || matrix{{j}}), image B)) then (h = (h || matrix{{j}}));
				  j=j+1);
			     F = append(F,h)));
		   F = join(F,{B_{i+1}^{0..(i+1)},(-1)*B_{i+1}^{0..(i+1)}}))
	      else (
		   nullmap := map(ZZ^1,ZZ^s,0);
		   nullvec := map(ZZ^1,ZZ^1,0);
		   scan(H, h -> (
			     h = B*substitute(vertices intersection(nullmap,nullvec,B^{0..i},h),ZZ);
			     F = append(F,h))));
	       C={};
	       scan(#F-1, k -> (
			 scan(#F-k-1, l -> (
				   f := F#k;
				   g := F#(k+l+1);
				   if ((((entries f)#(i+1)#0)*((entries g)#(i+1)#0) < 0) and ((f+g) != 0*(f+g))) then (C=append(C,f+g))))));
	       Cnew := {};
	       G := F;
	       j := 0;
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
	  m := min(n,s);
	  scan(m, i -> (
		    j := i;
		    stop := false;
		    while ((not stop) and (j < s)) do (
			 if (M_i_j != 0) then (stop = true)
			 else (j = j+1));
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
			      M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1}));
		    if (M_i_i < 0) then (M = M^{0..i-1} || (-1)*M^{i} || M^{i+1..s-1})));
	  M);
     -- Function to compute the/one preimage of h under A
     preim := (h,A) -> (
	  N := gens(ker(-h|A));
	  N = transpose(ref(transpose(N)));
	  N_{0}^{1..(numgens target N)-1});
     A := C#"halfspaces";
     if (C#"hyperplanes" != 0) then ( A = A || (C#"hyperplanes") || ((-1)*(C#"hyperplanes")));
     A = substitute(A,ZZ);
     H := constructHilbertBasis ref transpose A;
     apply(H,h -> preim(h,A)))


inInterior = method(TypicalValue => Boolean)
inInterior (Matrix,Polyhedron) := (p,P) -> (
     smallestFace(p,P) == P)

inInterior (Matrix,Cone) := (p,C) -> (
     smallestFace(p,C) == C)


interiorPoint = method(TypicalValue => Matrix)
interiorPoint Polyhedron := P -> (
     Vm := vertices P;
     n := numgens source Vm;
     ones := matrix toList(n:{1/n});
     Vm * ones)


interiorVector = method(TypicalValue => Matrix)
interiorVector Cone := C -> (
     Rm := rays C;
     ones := matrix toList(numgens source Rm:{1_QQ});
     Rm * ones)


latticePoints = method(TypicalValue => List)
latticePoints Polyhedron := P -> (
     -- Checking for input errors
     if (not isCompact(P)) then (
	  error ("The polyhedron must be compact"));
     V := (vertices(P)) || (matrix{toList((numgens source vertices(P)):1_QQ)});
     L := hilbertBasis posHull V;
     L = apply(select(L, l -> (last(flatten(entries(l))) == 1)), v -> (v^{0..(ambdim(P)-1)}));
     L)


-- PURPOSE : Computing the Cone of the Minkowskisummands of a Polyhedron 'P', the minimal 
--           Minkowskisummands, and minimal decompositions
--   INPUT : 'P',  a polyhedron
--  OUTPUT : '(C,L,M)'  where 'C' is the Cone of the Minkowskisummands, 'L' is a list of 
--                      Polyhedra corresponding to the generators of 'C', and 'M' is a 
--                      matrix where the columns give the minimal decompositions of 'P'.
minkSummandCone = method(TypicalValue => ix)
minkSummandCone Polyhedron := P -> (
     d := P#"ambientdimension";
     -- Saving the compact edges of P in the list edges and the "normed" pair of vertices in the list Kanten
     edges := {};
     scan(faces(d-1,P), e -> (
	       if (isCompact(e)) then (edges=append(edges,e))));
     -- Subfunction to save the two vertices of a compact edge in a matrix where the vertex with the smaller entries comes first
     -- by comparing the two vertices entry-wise
     normvert := M -> ( v:= M_{0}-M_{1};
         normrec := w -> ( local i; if ((entries(w))#0#0 > 0) then (i=0) else if ((entries(w))#0#0 < 0) then (i=1) else (w=w^{1..(numgens target w)-1}; i=normrec(w)); i);
          i=normrec(v);
        if (i==1) then (M=M_{1}|M_{0});
        M);
     Kanten = apply(edges, k -> (normvert(vertices(k))));
     -- Saving the compact two dimensional faces of P in the list twofaces
     twofaces = {};
     scan(faces(d-2,P), f -> (
	       if (isCompact(f)) then (twofaces=append(twofaces,f))));
     m:=0;
     -- Generating a hashTable that enumerates the edges in Kanten
     KantenNr:={};
     scan(Kanten, k -> (KantenNr = append(KantenNr, k => m); m=m+1));
     KantenNr = hashTable KantenNr;
     n := #Kanten;
     zeromap := map(QQ^d,QQ^1,0);
     Nfertig := map(QQ^0,QQ^n,0);
     -- Determining the equations on the variables for the edges given by the two 
     -- dimensional faces
     scan(twofaces, f -> (
	       j := 0;
	       ordering := {};
               -- Saving the edges of the active twoface
	       edgesf := faces(1,f);
               -- Selecting the first edge and removing it from the list
	       k := edgesf#0;
               edgesf = edgesf_{1..(#edgesf)-1};
               -- Saving the the number of the selected edge from the hashTable KantenNr in a
	       -- list to order the edges of the active twoface
	       ordering = append(ordering, KantenNr#(normvert(vertices(k))) => j);
	       j = j+1;
	       -- Saving the two vertices of the edge
	       M := (vertices(k))_{0};
	       v := (vertices(k))_{1};
	       -- Scanning through the remaining edges to find the order in which they appear in the active twoface
	       scan(#edgesf, i -> (nedges := {};
			 scan(edgesf, e -> (
				   if contains(e,convexHull(v)) then (
					k = e;
					ordering = append(ordering, KantenNr#(normvert(vertices(k))) => j);
					j = j+1)
				   else (nedges =append(nedges,e))));
			 edgesf = nedges;
			 M = M|v;
			 vnew := v;
			 scan(numgens(source(vertices(k))), l -> ( c := (vertices(k))_{l}; 
				   if (c =!= v) then (vnew = c)));
			 v = vnew));
	       -- The matrix M contains all vertices of the active twoface ordered in a "circle"
	       -- now we take the differences from one colummn with the previous to get edges of 
	       -- the active two face ordered in a "circle" which gives the equations for the 
	       -- variables corresponding to these edges
	       M = (M_{1..(numgens source M)-1} | M_{0}) - M;
	       -- Turn ordering into a hashTable to find out to which edges the columns in M
	       -- correspond 
	       ordering = hashTable ordering;
	       -- Generating a new set of rows for the final equationmatrix for the cone 
	       -- by adding zerocolumns into M for those edges not appearing in the 
	       -- active twoface
	       L := {};
	       scan(#Kanten, i -> (
			 column := zeromap;
			 scan(keys ordering, ko -> (
				   if (ko == i) then (column = M_{ordering#ko})));
			 L = append(L, i => column)));
	       L = hashTable L;
	       N := map(QQ^d,QQ^0,0);
	       scan(#Kanten, l -> ( N = N |(L#l)));
	       Nfertig = Nfertig || N));
     -- constructing the Cone by intersecting the equations with the positive orthant
     Id := map(QQ^n,QQ^n,1);
     C := intersection(Id,Nfertig);
     -- Selecting a vertex of P
     v := (vertices(P))_{0};
     -- Saving the generators of the cone
     M := rays C;
     -- Saving the generators of the tailcone in a matrix
     TC := (map(QQ^(P#"ambientdimension"),QQ^1,0)) | (P#"rays") | (P#"linealityspace") | ((-1)*(P#"linealityspace"));
     summList := {};
     -- Computing for each generator of the cone the corresponding summand polyhedron, where the first vertex 
     -- is the origin
     scan(numgens source M, i -> (
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
			      j := KantenNr#(normvert(vertices(e)));
			      if (contains(e,v)) then (
				   ve := vertices(e);
				   edir := (ve*(matrix{{1_QQ},{1_QQ}})) - 2*v;
				   vnew := v0 + (((entries(mi))#j#0)*edir);
				   L = L | vnew;
				   Lnew = append(Lnew,(v+edir,vnew)))
			      else (nedges = append(nedges,e))));
		     remedges = nedges;
		     scan(Lnew, (u,w) -> (
			       if (remedges =!= {}) then (
				    L = edgesearch(L,u,w,mi))));
		     L);
		mi := M_{i};
		v0 := map(QQ^d,QQ^1,0);
		L := v0;
		-- Calling the edgesearch function to get the vertices of the summand
		L = edgesearch(L,v,v0,mi);
		summList = append(summList, i => convexHull(L,TC))));
      summList = hashTable summList;
      onevec := matrix toList(numgens target M: {1_QQ});
      negId := map(QQ^(numgens source M),QQ^(numgens source M),-1);
      zerovec := matrix toList(numgens source M: {0_QQ});
      Q := intersection(negId,zerovec,M,onevec);
      (C,summList,vertices(Q)))



-- PURPOSE : Computing the 'n'-skeleton of a fan
--   INPUT : (n,F),  where 'n' is a positive integer and
--                   'F' is a Fan
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
smallestFace = method(TypicalValue => Polyhedron)
smallestFace(Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if ((numgens source p) =!= 1) or ((numgens target p) =!= (P#"ambientdimension")) then (
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
	  "p is not contained in P")
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
	  "p is not contained in P")
     )



-- PURPOSE : Computing the subfan of all smooth cones of the Fan
--   INPUT : 'F',  a Fan
--  OUTPUT : The Fan of smooth cones
smoothSubfan = method(TypicalValue => Fan)
smoothSubfan Fan := F -> (
     facerecursion := (L,F) -> (
	  scan(L, C -> (
		    if (isSmooth(C)) then (
			 F = addCone(C,F))
		    else (
			 L' := faces(1,C);
			 F = facerecursion(L',F))));
	  F);
     L := F#"generatingCones";
     F = {};
     facerecursion(L,F))			 




-- PURPOSE : Computing the tail cone of a given Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : The Cone generated by the rays and the lineality space of 'P'
tailCone = method(TypicalValue => Cone)
tailCone Polyhedron := P -> (
     posHull(P#"rays",P#"linealityspace"))



-- PURPOSE : 
--   INPUT : 
--  OUTPUT : 
vertexEdgeMatrix = method(TypicalValue => Matrix)
vertexEdgeMatrix Polyhedron := P -> (
     eP := apply(faces(dim(P)-1,P),vertices);
     vp := vertices(P);
     vList := hashTable apply(numgens source vp, i -> ( vp_{i} => i+1));
     d := #vList;
     n := #eP;
     M := (transpose matrix {toList(0..d)}) | ( (matrix {toList(1..n)}) || (matrix toList(d:toList(n:0))));
     scan(#eP, i -> (
	       j := vList#((eP#i)_{0});
	       M = M_{0..i} | (M_{i+1}^{0..j-1} || matrix {{1}} || M_{i+1}^{j+1..d}) | M_{i+2..n};
	       j = vList#((eP#i)_{1});
	       M = M_{0..i} | (M_{i+1}^{0..j-1} || matrix {{1}} || M_{i+1}^{j+1..d}) | M_{i+2..n}));
     M);



-- PURPOSE : 
--   INPUT : 
--  OUTPUT : 
vertexFacetMatrix = method(TypicalValue => Matrix)
vertexFacetMatrix Polyhedron := P -> (
     fP := apply(faces(1,P),vertices);
     vp := vertices(P);
     vList := hashTable apply(numgens source vp, i -> ( vp_{i} => i+1));
     d := #vList;
     n := #fP;
     M := (transpose matrix {toList(0..d)}) | ( (matrix {toList(1..n)}) || (matrix toList(d:toList(n:0))));
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
     if (( P#"ambientdimension") =!= (numgens source A)) then (
	  error ("Matrix source must be ambientspace"));
     if ((numgens target A) =!= (numgens target v)) then (
	  error ("Vector must lie in target space of matrix"));
     if ((numgens source v) =!= 1) then (
	  error ("Second argument must be a vector"));
     -- Generating nr of vertices many copies of v
     v = v * (matrix {toList(P#"verticesnr":1_QQ)});
     Mv := (A*(vertices P)) + v;
     Mr := A*(rays P);
     if ((numgens source Mr) == 0) then (Mr = matrix toList(numgens target Mv:{0_QQ}));
     convexHull(Mv,Mr))


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space
affineImage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     v := map(QQ^(numgens target A),QQ^1,0);
     affineImage(A,P,v))


--   INPUT : '(P,v)',  where 'v' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : a Polyhedron, which is the translation of 'P' by 'v', i.e. {p+v | p in P} 
affineImage(Polyhedron,Matrix) := (P,v) -> (
     -- Generating the identity matrix
     A := map(QQ^(P#"ambientdimension"),QQ^(P#"ambientdimension"),1);
     affineImage(A,P,v))



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



-- PURPOSE : Converts the Cone 'C' into itself as a Polyhedron 'P'
--   INPUT : 'C'  a Cone
--  OUTPUT : 'P' the cone saved as a polyhedron
coneToPolyhedron = method(TypicalValue => Polyhedron)
coneToPolyhedron(Cone) := C -> (
     M := map(QQ^(C#"ambientDimension"),QQ^1,0);
     N := rays(C);
     convexHull(M,N))



-- PURPOSE : Computing the direct product of two polyhedra in the direct product of their ambient spaces
directProduct = method(TypicalValue => Polyhedron)

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



Polyhedron * Polyhedron := (P1,P2) -> directProduct(P1,P2)
Polyhedron * Cone := (P,C) -> directProduct(P,C)
Cone * Polyhedron := (C,P) -> directProduct(C,P)
Cone * Cone := (C1,C2) -> directProduct(C1,C2)


-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
	genrays := (transpose(C#"halfspaces"),transpose(C#"hyperplanes"));
	dualgens := ((-1)*(C#"rays"),C#"linealityspace");
	coneBuilder(genrays,dualgens))



-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambientspace
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     -- Checking for input errors
     if (P1#"ambientdimension" =!= P2#"ambientdimension") then (
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
     LS := (C1#"linealityspace")|(C2#"linealityspace");
     posHull(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
     -- Checking for input errors
     if (C#"ambientDimension" =!= P#"ambientdimension") then (
	  error ("Cone and polyhedron must lie in the same space"));
     -- Saving the vertices and rays
     V := P#"vertices";
     R := (P#"rays")|(C#"rays")|(C#"linealityspace")|((-1)*(C#"linealityspace"));
     convexHull(V,R))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
     -- Checking for input errors
     if (C#"ambientDimension" =!= P#"ambientdimension") then (
	  error ("Cone and polyhedron must lie in the same space"));
     -- Saving the vertices and rays
     V := P#"vertices";
     R := (P#"rays")|(C#"rays")|(C#"linealityspace")|((-1)*(C#"linealityspace"));
     convexHull(V,R))



Polyhedron + Polyhedron := (P1,P2) -> minkowskiSum(P1,P2)
Polyhedron + Cone := (P,C) -> minkowskiSum(P,C)
Cone + Polyhedron := (C,P) -> minkowskiSum(P,C)
Cone + Cone := (C1,C2) -> minkowskiSum(C1,C2)


-- PURPOSE : Computing the inner normalfan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, which is the outer normalfan of 'P'
normalfan = method(TypicalValue => Fan)
normalfan Polyhedron := P -> (
     vm := vertices P;
     L := {};
     scan(numgens source vm, i -> (
	       Q := affineImage(P,(-1)*(vm_{i}));
	       L = append(L,dualCone(posHull(Q)))));
     makeFan(L))



-- PURPOSE : Computing the polar of a given polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : a Polyhedron, which is the set { v | v*p<=1 forall p in P}
polar = method(TypicalValue => Polyhedron)
polar Polyhedron := P -> (
     d := P#"ambientdimension";
     M := map(QQ^d,QQ^d,1);
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
crosspolytope = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a strictly positive rational number which is the distance of the vertices to the
--     	    	       origin
--  OUTPUT : The 'd'-dimensional crosspolytope with vertex-origin distance 's'
crosspolytope(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if (d < 1) then (
	  error ("dimension must at least be 1"));
     if (s <= 0) then (
	  error ("size of the crosspolytope must be positive"));
     M := map(QQ^d,QQ^d,s) | map(QQ^d,QQ^d,-s);
     convexHull M)


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer which gives the dimension of the polytope and
--     	    	       's' is a strictly positive integer which is the distance of the vertices to the origin
crosspolytope(ZZ,ZZ) := (d,s) -> (
     s = substitute(s,QQ);
     crosspolytope(d,s))


--   INPUT :  'd',  where 'd' is a strictly positive integer which gives the dimension of the polytope
crosspolytope ZZ := d -> (
     s := 1_QQ;
     crosspolytope(d,s))



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


-- PURPOSE : Computing the cone of the Hirzebruch surface H_r
--   INPUT : 'r'  a positive integer
--  OUTPUT : The Hirzebruch surface H_r
hirzebruch = method(TypicalValue => Fan)
hirzebruch ZZ := (r) -> (
     -- Checking for input errors
     if (r < 0) then (
	  error ("Input must be a positive integer"));
     normalfan convexHull matrix {{0,1,0,1+r},{0,0,1,1}})



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



-- PURPOSE : Computing the state polytope of the ideal 'I'
--   INPUT : 'I',  a homogeneous ideal with resect to some strictly psoitive grading
--  OUTPUT : The state polytope as a polyhedron
statePolytope = method(TypicalValue => Polyhedron)
statePolytope Ideal := I -> (
     intVec := C -> (
	  local v;
	  if ((numgens source rays C) == 0) then (v = map(QQ^(ambdim(C)),QQ^1,0))
	  else (
	       v=rays(C)*(matrix toList(numgens source rays C:{1})));
	  v);
     homogeneityCheck := I -> (
	  L := flatten entries gens I;
	  lt := apply(L, leadTerm);
	  M := {};
	  scan(#L, i -> ( scan(exponents(L#i), e -> (M = append(M,flatten(exponents(lt#i))-e)))));
	  M = matrix M;
	  C := intersection(map(QQ^(numgens source M),QQ^(numgens source M),1),M);
	  v := intVec(C);
	  homog := true;
	  scan(flatten entries v, e -> (homog = (homog and (e>0))));
	  (homog,v));
     gCone := (g,lt) -> (
	  reduceGB := g -> (
	       gl = flatten entries gens g;
	       L := {};
	       scan(gl, l -> (L=append(L,((l-leadTerm(l))% g)+leadTerm(l))));
	       L);
	  L := {};
	  g = reduceGB g;
	  lt = flatten entries lt;
	  scan(#g, i -> ( scan(exponents(g#i), e -> (L = append(L,flatten(exponents(lt#i))-e)))));
	  intersection matrix L);
     wLeadTerm := (w,I) -> (
	  w = flatten entries w;
	  R := ring I;
	  --L := apply(gens R, degree);
	  --posVec := (transpose L)#0;
	  --m := min w;
	  --if (m < 0) then (
	  --     p := minPosition w;
	  --     m = m/(posVec#p);
	  --     w = w-(m*posVec));
	  lcm := 1;
	  scan(w, e -> (if (e != 0) then lcm = ((denominator e)/(gcd((denominator e),lcm)))*lcm));
	  w = lcm*w;
	  w = apply(w, e -> (substitute(e,ZZ)));
	  S := (coefficientRing R)[(gens R), MonomialOrder => {Weights => w}, Global => false];
	  f := map(S,R);
	  --f' := map(R,S);
	  I' := f I;
	  g := gb I';
	  lt := leadTerm I';
	  gbRemove I';
	  --g = f' g;
	  --lt = f' lt;
	  (g,lt));
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
     g := gb I;
     lt := leadTerm I;
     C := gCone(g,lt);
     gbRemove I;
     facets := faces(1,C);
     facets = apply(facets, f -> (intVec(f),f,C));
     verts := {lt};
     while (facets != {}) do (
	  local omega',f;
	  (omega',f,C) = facets#0;
	  omega := intVec C;
	  eps := 1/10;
	  stopper := false;
	  while (not stopper) do (
	       omega1 := omega'-(eps*omega);
	       (g,lt) = wLeadTerm(omega1,I);
	       C' := gCone(g,lt);
	       if (equals(intersection(C,C'),f)) then (
		    C = C';
		    stopper = true)
	       else (eps = eps * 1/10));
	  verts = append(verts,lt);
	  newfacets := faces(1,C);
	  newfacets = apply(newfacets, f -> (intVec(f),f,C));
	  facets = sortIn(facets,newfacets));
     posv = substitute(posv,ZZ);
     R := ring I;
     S := QQ[(gens R), Degrees => (entries posv)];
     verts = apply(verts, el -> (T:=ring el; f := map(S,T); f el));
     L := flatten apply(verts, l -> (flatten entries l));
     d := (max apply(flatten L, degree))#0;
     vertmatrix := {};
     scan(verts, v -> (
	       VI := ideal flatten entries v;
	       SI := S/VI;
	       v = {};
	       scan(d, i -> ( v = join(v,flatten entries basis(i+1,SI))));
	       vertmatrix = append(vertmatrix,flatten sum apply(v,exponents))));
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


beginDocumentation()

document {
     Key => Polyhedra,
     "The documentation for this package is contained in the following file: ",
     HREF {replace("PKG","Polyhedra",currentLayout#"package") | "Manual.pdf", "Manual.pdf"}, "."
     }

undocumented {
     fVector,
     (fVector,Cone),
     (fVector,Polyhedron),
     hilbertBasis,
     (hilbertBasis,Cone),
     inInterior,
     (inInterior,Matrix,Cone),
     (inInterior,Matrix,Polyhedron),
     interiorPoint,
     (interiorPoint,Polyhedron),
     interiorVector,
     (interiorVector,Cone),
     latticePoints,
     (latticePoints,Polyhedron),
     minkSummandCone,
     (minkSummandCone,Polyhedron),
     skeleton,
     (skeleton,ZZ,Fan),
     smallestFace,
     (smallestFace,Matrix,Cone),
     (smallestFace,Matrix,Polyhedron),
     smoothSubfan,
     (smoothSubfan,Fan),
     tailCone,
     (tailCone,Polyhedron),
     vertexEdgeMatrix,
     (vertexEdgeMatrix,Polyhedron),
     vertexFacetMatrix,
     (vertexFacetMatrix,Polyhedron),
     affineHull,
     (affineHull,Polyhedron),
     affineImage,
     (affineImage,Matrix,Polyhedron),
     (affineImage,Matrix,Polyhedron,Matrix),
     (affineImage,Polyhedron,Matrix),
     bipyramid,
     (bipyramid,Polyhedron),
     coneToPolyhedron,
     (coneToPolyhedron,Cone),
     directProduct,
     (directProduct,Cone,Cone),
     (directProduct,Cone,Polyhedron),
     (directProduct,Polyhedron,Cone),
     (directProduct,Polyhedron,Polyhedron),
     dualCone,
     (dualCone,Cone),
     minkowskiSum,
     (minkowskiSum,Cone,Cone),
     (minkowskiSum,Cone,Polyhedron),
     (minkowskiSum,Polyhedron,Cone),
     (minkowskiSum,Polyhedron,Polyhedron),
     normalfan,
     (normalfan,Polyhedron),
     polar,
     (polar,Polyhedron),
     pyramid,
     (pyramid,Polyhedron),
     crosspolytope,
     (crosspolytope,ZZ),
     (crosspolytope,ZZ,QQ),
     (crosspolytope,ZZ,ZZ),
     cyclicPolytope,
     (cyclicPolytope,ZZ,ZZ),
     hirzebruch,
     (hirzebruch,ZZ),
     hypercube,
     (hypercube,ZZ),
     (hypercube,ZZ,QQ),
     (hypercube,ZZ,ZZ),
     newtonPolytope,
     (newtonPolytope,RingElement),
     statePolytope,
     (statePolytope,Ideal),
     stdSimplex,
     (stdSimplex,ZZ),
     Polyhedron,
     Cone,
     Fan,
     convexHull,
     (convexHull,Matrix),
     (convexHull,Matrix,Matrix),
     (convexHull,Polyhedron,Polyhedron),
     posHull,
     (posHull,Cone,Cone),
     (posHull,Matrix),
     (posHull,Matrix,Matrix),
     (posHull,Polyhedron),
     intersection,
     (intersection,Cone,Cone),
     (intersection,List),
     (intersection,Matrix),
     (intersection,Matrix,Matrix),
     (intersection,Matrix,Matrix,Matrix,Matrix),
     (intersection,Polyhedron,Polyhedron),
     makeFan,
     (makeFan,Cone),
     (makeFan,List),
     addCone,
     (addCone,Cone,Fan),
     (addCone,Cone,List),
     (addCone,List,Fan),
     symmDiff,
     (symmDiff,List,List),
     union,
     (union,List,List),
     ambdim,
     (ambdim,Cone),
     (ambdim,Fan),
     (ambdim,Polyhedron),
     cones,
     (cones,ZZ,Fan),
     genCones,
     (genCones,Fan),
     halfspaces,
     (halfspaces,Cone),
     (halfspaces,Polyhedron),
     hyperplanes,
     (hyperplanes,Cone),
     (hyperplanes,Polyhedron),
     linspace,
     (linspace,Cone),
     (linspace,Polyhedron),
     rays,
     (rays,Cone),
     (rays,Fan),
     (rays,Polyhedron),
     vertices,
     (vertices,Polyhedron),
     areCompatible,
     (areCompatible,Cone,Cone),
     commonFace,
     (commonFace,Cone,Cone),
     (commonFace,Polyhedron,Polyhedron),
     contains,
     (contains,Cone,Cone),
     (contains,Cone,Matrix),
     (contains,Cone,Polyhedron),
     (contains,Fan,Cone),
     (contains,List,Cone),
     (contains,Polyhedron,Cone),
     (contains,Polyhedron,Matrix),
     (contains,Polyhedron,Polyhedron),
     equals,
     (equals,Cone,Cone),
     (equals,Fan,Fan),
     (equals,Polyhedron,Polyhedron),
     isCompact,
     (isCompact,Polyhedron),
     isComplete,
     (isComplete,Fan),
     isEmpty,
     (isEmpty,Polyhedron),
     isFace,
     (isFace,Cone,Cone),
     (isFace,Polyhedron,Polyhedron),
     isPointed,
     (isPointed,Cone),
     (isPointed,Fan),
     isProjective,
     (isProjective,Fan),
     isPure,
     (isPure,Fan),
     isSmooth,
     (isSmooth,Cone),
     (isSmooth,Fan),
     faces,
     (faces,ZZ,Cone),
     (faces,ZZ,Polyhedron),
     (symbol *,Cone,Cone),
     (symbol *,Cone,Polyhedron),
     (symbol +,Cone,Cone),
     (symbol +,Cone,Polyhedron),
     (symbol ==,Cone,Cone),
     (symbol ==,Fan,Fan),
     (symbol *,Polyhedron,Cone),
     (symbol *,Polyhedron,Polyhedron),
     (symbol +,Polyhedron,Cone),
     (symbol +,Polyhedron,Polyhedron),
     (symbol ==,Polyhedron,Polyhedron),
     (dim,Cone),
     (net,Cone),
     (dim,Fan),
     (net,Fan),
     (dim,Polyhedron),
     (net,Polyhedron)
     }

TEST ///
M = matrix{{1/2,1,0},{1,1/2,0}};
P = convexHull M;
assert(M == (P#"vertices"))

N = matrix{{1,1,0,1},{0,2,1,1}};
P1 = convexHull N;
assert(3 == (P1#"verticesnr"))

P2 = convexHull(P,P1);
TM = matrix{{1,1,0,0},{0,2,1,0}};
assert(image(substitute(TM, QQ)) == image(P2#"vertices"))

assert(equals(hypercube 2, polar crosspolytope 2))
    
///
