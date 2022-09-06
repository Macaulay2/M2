-- Warning: This package is deprecated. Please consider using Polyhedra.m2 instead.

--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : René Birkner 
-- UPDATE HISTORY : April 2008, December 2008, March 2009, Juli 2009,
--     	    	    September 2009, October 2009, January 2010
---------------------------------------------------------------------------
newPackage("OldPolyhedra",
    Headline => "convex polyhedra",
    Version => "1.3",
    Date => "August 21, 2014",
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Polyhedra: a package for computations with convex polyhedral objects",
	 "acceptance date" => "2009-09-07",
	 "published article URI" => "http://j-sag.org/Volume1/jsag-3-2009.pdf",
	 "published code URI" => "http://j-sag.org/Volume1/Polyhedra.m2",
	 "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Polyhedra.m2",
	 "release at publication" => "c065ec7651789907627333018dc7d675968639e4", -- git commit number in hex
	 "version at publication" => "1.0.5",
	 "volume number" => "1",
	 "volume URI" => "http://j-sag.org/Volume1/"
	 },
    Keywords => {"Convex Geometry"},
    Authors => {
         {Name => "René Birkner",
	  HomePage => "http://page.mi.fu-berlin.de/rbirkner/index.htm",
	  Email => "rbirkner@mi.fu-berlin.de"}},
    DebuggingMode => false,
    PackageImports=>{"IntegralClosure", "ReesAlgebra", "LLLBases", "FourierMotzkin" }
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2010 René Birkner
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


export {"PolyhedralObject", 
        "Polyhedron", 
	"Cone", 
	"Fan", 
	"PolyhedralComplex", 
	"convexHull", 
	"posHull", 
	"intersection", 
	"fan", 
	"addCone", 
	"polyhedralComplex", 
	"addPolyhedron", 
        "ambDim", 
	"cones", 
	"maxCones", 
	"maxPolyhedra", 
	"halfspaces", 
	"hyperplanes", 
	"linSpace", 
	"polyhedra", 
	"rays", 
	"vertices",
        "areCompatible", 
	"commonFace", 
	"contains", 
	"isCompact", 
	"isComplete", 
	"isEmpty", 
	"isFace", 
	"isLatticePolytope", 
	"isPointed", 
	"isPolytopal", 
	"isPure",
	"isReflexive",
	"isSimplicial", 
	"isSmooth", 
	"isVeryAmple",
	"boundaryMap", 
	"dualFaceLattice", 
	"faceLattice",
	"faceOf", 
	"faces", 
	"fVector", 
	"hilbertBasis", 
	"incompCones",
	"incompPolyhedra",
	"inInterior", 
	"interiorPoint", 
	"interiorVector",
	"interiorLatticePoints", 
	"latticePoints", 
	"maxFace", 
	"minFace", 
	"objectiveVector",
	"minkSummandCone", 
	"mixedVolume", 
	"polytope", 
	"proximum", 
	"skeleton", 
	"smallestFace", 
	"smoothSubfan", 
	"stellarSubdivision", 
	"tailCone", 
	"triangulate", 
	"volume", 
	"vertexEdgeMatrix", 
	"vertexFacetMatrix", 
	"affineHull", 
	"affineImage", 
	"affinePreimage", 
	"bipyramid", 
	"ccRefinement", 
	"coneToPolyhedron", 
	"directProduct",
	"dualCayley",
	"dualCayleyFace",
	"dualCone", 
	"faceFan", 
	"imageFan", 
	"minkowskiSum", 
	"normalFan", 
	"polar",
	"polarFace", 
	"pyramid", 
	"sublatticeBasis", 
	"toSublattice",
	"crossPolytope", 
	"cellDecompose", 
	"cyclicPolytope", 
	"ehrhart", 
	"emptyPolyhedron", 
	"hirzebruch", 
	"hypercube", 
	"newtonPolytope", 
	"posOrthant", 
	"secondaryPolytope", 
	"statePolytope", 
	"stdSimplex",
	"saveSession"}


---------------------------------------------------------------
-- Sorting rays
---------------------------------------------------------------

-- A ray is a matrix ZZ^n <-- ZZ^1, so rays can be sorted by assembling them
-- into a matrix and calling "sortColumns".  We sort the rays, so that changes to 
-- the algorithm for computing the hash code of matrices doesn't affect what we do.

raySort = rays -> rays _ (reverse sortColumns (- matrix {rays}))

---------------------------------------------------------------

-- WISHLIST
--  -Symmetry group for polytopes


-- Definind the new type PolyhedralObject
PolyhedralObject = new Type of HashTable
globalAssignment PolyhedralObject

-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron

-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone

-- Defining the new type Fan
Fan = new Type of PolyhedralObject
globalAssignment Fan

-- Defining the new type PolyhedralComplex
PolyhedralComplex = new Type of PolyhedralObject
globalAssignment PolyhedralObject


-- Modifying the standard output for a polyhedron to give an overview of its characteristica
net Polyhedron := P -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "dimension of polyhedron",
					      "dimension of lineality space",
					      "number of rays",
					      "number of vertices", 
					      "number of facets"}, key -> (net key, " => ", net P#key))),
	  "}" ))
				

-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "dimension of the cone",
					      "dimension of lineality space",
					      "number of rays",
					      "number of facets"}, key -> (net key, " => ", net C#key))),
	  "}" ))


-- Modifying the standard output for a Fan to give an overview of its characteristica
net Fan := F -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "top dimension of the cones",
					      "number of generating cones",
					      "number of rays"}, key -> (net key, " => ", net F#key))),
	  "}" ))


-- Modifying the standard output for a Polyhedral Complex to give an overview of its characteristica
net PolyhedralComplex := F -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "top dimension of the polyhedra",
					      "number of generating polyhedra"}, key -> (net key, " => ", net F#key))),
	  "}" ))


-- PURPOSE : Computing the Convex Hull of a given set of points and rays
convexHull = method(TypicalValue => Polyhedron)

--   INPUT : 'Mvert'  a Matrix containing the generating points as column vectors
--		 'Mrays'  a Matrix containing the generating rays as column vectors
--  OUTPUT : 'P'  a Polyhedron
-- COMMENT : The description by vertices and rays is stored in P as well as the 
--           description by defining half-spaces and hyperplanes.
convexHull(Matrix,Matrix) := (Mvert,Mrays) -> (
	-- checking for input errors
     	if numgens target Mvert =!= numgens target Mrays then error ("points and rays must lie in the same space");
	Mvert = chkZZQQ(Mvert,"points");
	Mrays = chkZZQQ(Mrays,"rays");
	if numRows Mvert == 0 then Mvert = matrix{{0}};
	if numColumns Mvert == 0 then Mvert = map(target Mvert,QQ^1,0);
	if numRows Mrays == 0 then Mrays = matrix{{0}};
	if numColumns Mrays == 0 then Mrays = map(target Mrays,QQ^1,0);
	-- homogenization of M
	Mvert = map(QQ^1,source Mvert,(i,j)->1) || Mvert;
	Mrays = map(QQ^1,source Mrays,0) || Mrays;
	M := Mvert | Mrays;
	-- Computing generators of the cone M and its dual cone
	hyperA := fourierMotzkin M;
--	verticesA := fourierMotzkin hyperA;
     	local verticesA;
	(verticesA,hyperA) = fMReplacement(M,hyperA#0,hyperA#1);
	polyhedronBuilder(hyperA,verticesA))


--   INPUT : 'M'  a Matrix containing the generating points as column vectors
convexHull Matrix := M -> (
	-- Checking for input errors
	M = chkZZQQ(M,"points");
	if numRows M == 0 then M = matrix{{0}};
	if numColumns M == 0 then M = map(target M,QQ^1,0);
	-- Generating the zero ray R
	R := map(target M,QQ^1,0);
	convexHull(M,R))


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if P1#"ambient dimension" =!= P2#"ambient dimension" then error("Polyhedra must lie in the same ambient space");
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
	  n = P#"ambient dimension";
	  V = map(QQ^n,QQ^1,0);
	  R = rays P | linSpace P | -(linSpace P))
     else if instance(P,Polyhedron) then (
	  n = P#"ambient dimension";
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



-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
posHull = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'LS'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining half-spaces and hyperplanes.
posHull(Matrix,Matrix) := (Mrays,LS) -> (
     -- checking for input errors
     if numRows Mrays =!= numRows LS then error("rays and linSpace generators must lie in the same space");
     Mrays = chkZZQQ(Mrays,"rays");
     LS = chkZZQQ(LS,"lineality space");
     -- Computing generators of the cone and its dual cone
     dualgens := fourierMotzkin(Mrays,LS);
     local genrays;
     (genrays,dualgens) = fMReplacement(Mrays,dualgens#0,dualgens#1);
--     genrays := fourierMotzkin dualgens;
     coneBuilder(genrays,dualgens))


--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
posHull Matrix := R -> (
     R = chkZZQQ(R,"rays");
     -- Generating the zero lineality space LS
     LS := map(target R,QQ^1,0);
     posHull(R,LS))


--   INPUT : '(C1,C2)'  two cones
posHull(Cone,Cone) := (C1,C2) -> (
	-- Checking for input errors
	if C1#"ambient dimension" =!= C2#"ambient dimension" then error("Cones must lie in the same ambient space");
	-- Combining the rays and the lineality spaces into one matrix each
	R := C1#"rays" | C2#"rays";
	LS := C1#"linealitySpace" | C2#"linealitySpace";
	dualgens := fourierMotzkin(R,LS);
	local genrays;
	(genrays,dualgens) = fMReplacement(R,dualgens#0,dualgens#1);
--	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))


--   INPUT : 'P'  a Polyhedron
posHull Polyhedron := P -> (
     Mrays := makePrimitiveMatrix P#"vertices" | P#"rays";
     Mlinspace := P#"linealitySpace";
     posHull(Mrays,Mlinspace))


--   INPUT : 'L',   a list of Cones, Polyhedra, rays given by R, 
--     	    	    and (rays,linSpace) given by '(R,LS)'
posHull List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that gives valid rays and linSpace
     isValidPair := S -> #S == 2 and if S#1 == 0 then instance(S#0,Matrix) else instance(S#1,Matrix) and numRows S#0 == numRows S#1;
     -- Checking for input errors  
     if L == {} then error("List of convex objects must not be empty");
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local R;
     local LS;
     if (not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,Matrix)) then 
	  error ("The input must be cones, polyhedra, rays, or (rays,linSpace).");
     -- Adding the vertices and rays to 'R,LS', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = C#"ambient dimension";
	  R = rays C;
	  LS = linSpace C)
     else if instance(C,Polyhedron) then (
	  n = C#"ambient dimension";
	  R = makePrimitiveMatrix vertices C | rays C;
	  LS = linSpace C)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if not isValidPair C then error("Rays and lineality space must be given as a sequence of two matrices with the same number of rows");
	  R = chkQQZZ(C#0,"rays");
	  n = numRows R;
	  LS = if C#1 == 0 then map(ZZ^n,ZZ^1,0) else chkQQZZ(C#1,"lineality space"))
     else (
	  R = chkQQZZ(C,"rays");
	  n = numRows R;
	  LS = map(ZZ^n,ZZ^1,0));
     --  Adding the rays and lineality spaces to 'R,LS' for each remaining element in 'L', depending on the type of 'C'
     L = apply(drop(L,1), C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone)) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,Matrix)) then 
		    error ("The input must be cones, polyhedra, rays, or (rays,lineality space)");
	       if instance(C1,Cone) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (rays C1,linSpace C1))
	       else if instance(C1,Polyhedron) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (makePrimitiveMatrix vertices C1 | rays C1,linSpace C1))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if not isValidPair C1 then error("(Rays,lineality space) must be given as a sequence of two matrices with the same number of rows");
		    if numRows C1#0 != n then error("(Rays,lineality space) must be of the correct dimension.");
		    if C1#1 != 0 then (chkQQZZ(C1#0,"rays"),chkQQZZ(C1#1,"lineality space"))
		    else (chkQQZZ(C1#0,"rays"),{}))
	       else (
		    -- Checking for input errors
		    if numRows C1 != n then error("Rays must be of the correct dimension.");
		    (chkQQZZ(C1,"rays"),{}))));
     LR := flatten apply(L, l -> l#0);
     if LR != {} then R = R | matrix {LR};
     L = flatten apply(L, l -> l#1);
     if L != {} then LS = LS | matrix {L};
     if LS == 0 then posHull R else posHull(R,LS))


-- PURPOSE : Computing a polyhedron as the intersection of affine half-spaces and hyperplanes
intersection = method()

--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix,Matrix,Matrix) := (M,v,N,w) -> (
	-- checking for input errors
	if numColumns M =!= numColumns N then error("equations of half-spaces and hyperplanes must have the same dimension");
	if numRows M =!= numRows v or numColumns v =!= 1 then error("invalid condition vector for half-spaces");
	if numRows N =!= numRows w or numColumns w =!= 1 then error("invalid condition vector for hyperplanes");
	M = -chkZZQQ(v,"condition vector for half-spaces") | chkZZQQ(M,"half-spaces");
	N = -chkZZQQ(w,"condition vector for hyperplanes") | chkZZQQ(N,"hyperplanes");
	-- Computing generators of the cone and its dual cone
	M = transpose M | map(source M,QQ^1,(i,j) -> if i == 0 then -1 else 0);
	N = transpose N;
	verticesA := fourierMotzkin(M,N);
	local hyperA;
	(hyperA,verticesA) = fMReplacement(M,verticesA#0,verticesA#1);
--	hyperA := fourierMotzkin verticesA;
	polyhedronBuilder(hyperA,verticesA))


--   INPUT : '(M,N)',  two matrices where either 'P' is the Cone {x | Mx<=0, Nx=0} if 'M' and 'N' have the same source space 
--     	    	       or, if 'N' is only a Column vector the Polyhedron {x | Mx<=v} 
--  OUTPUT : 'P', the Cone or Polyhedron
intersection(Matrix,Matrix) := (M,N) -> (
	-- Checking for input errors
	if ((numColumns M =!= numColumns N and numColumns N =!= 1) or (numColumns N == 1 and numRows M =!= numRows N)) and N != 0*N then 
		error("invalid condition vector for half-spaces");
	local genrays;
	local dualgens;
	M = chkZZQQ(M,"half-spaces");
	N = chkZZQQ(N,"condition vector for half-spaces");
	-- Decide whether 'M,N' gives the Cone C={p | M*p >= 0, N*p = 0}
	if numColumns M == numColumns N and numColumns N != 1 then (
		genrays = fourierMotzkin(-transpose M,transpose N);
		--dualgens = fourierMotzkin genrays;
		local dualgens;
		(dualgens,genrays) = fMReplacement(-transpose M,genrays#0,genrays#1);
		coneBuilder(genrays, dualgens))
	-- or the Cone C={p | M*p >= N=0}
	else if numRows N == 0 then (
		genrays = fourierMotzkin (-transpose M);
--		dualgens = fourierMotzkin genrays;
                local dualgens;
		(dualgens,genrays) = fMReplacement(-transpose M,genrays#0,genrays#1);
		coneBuilder(genrays,dualgens))
	-- or the Polyhedron P={p | M*p >= N != 0}
	else (	-- Computing generators of the Polyhedron and its dual cone
		M = -N | M;
		M = transpose M |  map(source M,QQ^1,(i,j) -> if i == 0 then -1 else 0);
		verticesA := fourierMotzkin M;
		--hyperA := fourierMotzkin verticesA;
		local hyperA;
		(hyperA,verticesA) = fMReplacement(M,verticesA#0,verticesA#1);
		polyhedronBuilder(hyperA,verticesA)))
   



--   INPUT : '(P1,P2)',  two polyhedra 
--  OUTPUT : 'P', the polyhedron that is the intersection of both
intersection(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking if P1 and P2 lie in the same space
	if P1#"ambient dimension" =!= P2#"ambient dimension" then error("Polyhedra must lie in the same ambient space");
	-- Combining the Half-spaces and the Hyperplanes
	M := (halfspaces P1)#0 || (halfspaces P2)#0;
	v := (halfspaces P1)#1 || (halfspaces P2)#1;
	N := (hyperplanes P1)#0 || (hyperplanes P2)#0;
	w := (hyperplanes P1)#1 || (hyperplanes P2)#1;
	intersection(M,v,N,w))


--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
intersection Matrix := M -> (
	-- Checking for input errors
	M = chkZZQQ(M,"half-spaces");
	-- Computing generators of the cone and its dual cone
	genrays := fourierMotzkin (-transpose M);
	--dualgens := fourierMotzkin genrays;
	local dualgens;
	(dualgens,genrays) = fMReplacement(-transpose M,genrays#0,genrays#1);
	coneBuilder(genrays,dualgens))



--   INPUT : '(C1,C2)',  two Cones
--  OUTPUT : 'C', the Cone that is the intersection of both
intersection(Cone,Cone) := (C1,C2) -> (
	-- Checking if C1 and C2 lie in the same space
	if C1#"ambient dimension" =!= C2#"ambient dimension" then error("Cones must lie in the same ambient space");
	M := halfspaces C1 || halfspaces C2;
	N := hyperplanes C1 || hyperplanes C2;
	intersection(M,N))
   
   
   
--   INPUT : '(C,P)',  a Cone and a Polyhedron
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Cone,Polyhedron) := (C,P) -> intersection {C,P}



--   INPUT : '(P,C)',  a Polyhedron and a Cone
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Polyhedron,Cone) := (P,C) -> intersection {P,C}



--   INPUT : 'L',   a list of Cones, Polyhedra, inequalities given by (M,v), 
--     	    	    and hyperplanes given by '{N,w}'
intersection List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that gives valid in/equalities
     isValidPair := S -> #S == 2 and if S#1 == 0 then instance(S#0,Matrix) else instance(S#1,Matrix) and numRows S#0 == numRows S#1 and numColumns S#1 == 1;
     -- Checking for input errors  
     if L == {} then error("List of cones must not be empty");   
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local Ml;
     local vl;
     local Nl;
     local wl;
     if (not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,List)) then 
	  error ("The input must be cones, polyhedra, inequalities, equalities.");
     -- Adding the inequalities and equalities to 'M,v,N,w', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = C#"ambient dimension";
	  Ml = -(halfspaces C);
	  vl = map(target halfspaces C,ZZ^1,0);
	  Nl = hyperplanes C;
	  wl = map(target hyperplanes C,ZZ^1,0))
     else if instance(C,Polyhedron) then (
	  n = C#"ambient dimension";
	  Ml = (halfspaces C)#0;
	  vl = (halfspaces C)#1;
	  Nl = (hyperplanes C)#0;
	  wl = (hyperplanes C)#1)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if not isValidPair C then error("Inequalities must be given as a sequence of a matrix and a column vector");
	  --Ml = chkQQZZ(C#0,"half-spaces");
	  n = numColumns C#0;
	  Ml = if C#1 == 0 then ((transpose chkQQZZ(transpose C#0,"half-spaces"))|map(ZZ^(numRows C#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C#0|C#1),"halfspaces or condition vector");
	  vl = Ml_{n};
	  Ml = submatrix'(Ml,{n});
     	  --vl = if C#1 == 0 then map(target Ml,ZZ^1,0) else chkQQZZ(C#1,"condition vector for half-spaces");
	  Nl = map(ZZ^1,source Ml,0);
	  wl = map(ZZ^1,ZZ^1,0))
     else (
	  -- Checking for input errors
	  if not isValidPair C then error("Equalities must be given as a list of a matrix and a column vector");
	  --Nl = chkQQZZ(C#0,"hyperplanes");
	  n = numColumns C#0;
	  Nl = if C#1 == 0 then ((transpose chkQQZZ(transpose C#0,"hyperplanes"))|map(ZZ^(numRows C#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C#0|C#1),"hyperplanes or condition vector");
	  wl = Nl_{n};print wl;
	  Nl = submatrix'(Nl,{n});
	  Ml = map(ZZ^1,source Nl,0);
	  vl = map(ZZ^1,ZZ^1,0));
	  --wl = if C#1 == 0 then map(target Nl,ZZ^1,0) else chkQQZZ(C#1,"condition vector for half-spaces"));
     --  Adding the inequalities and equalities to 'M,v,N,w', for each remaining element in 'L', depending on the type of 'C'
     L = apply(drop(L,1), C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone)) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and (not instance(C1,List)) then 
		    error("The input must be cones, polyhedra, inequalities, equalities.");
	       if instance(C1,Cone) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (-(halfspaces C1),map(target halfspaces C1,ZZ^1,0),hyperplanes C1,map(target hyperplanes C1,ZZ^1,0)))
	       else if instance(C1,Polyhedron) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    ((halfspaces C1)#0,(halfspaces C1)#1,(hyperplanes C1)#0,(hyperplanes C1)#1))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if not isValidPair C1 then error("Inequalities must be given as a sequence of a matrix and a column vector");
		    if numColumns C1#0 != n then error("Inequalities must be for the same ambient space.");
		    C1 = if C1#1 == 0 then ((transpose chkQQZZ(transpose C1#0,"half-spaces"))|map(ZZ^(numRows C1#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C1#0|C1#1),"halfspaces or condition vector");
		    (submatrix'(C1,{n}),C1_{n},map(ZZ^1,ZZ^n,0),map(ZZ^1,ZZ^1,0)))		      	   
--		    C1 = (chkQQZZ(C1#0,"half-spaces"),chkQQZZ(C1#1,"condition vector for half-spaces"));
--		    if C1#1 == 0 then (C1#0,map(target C1#0,ZZ^1,0),map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0))
--		    else (C1#0,C1#1,map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0)))
	       else (
		    -- Checking for input errors
		    if not isValidPair C1 then error("Equalities must be given as a list of a matrix and a column vector");
		    if numColumns C1#0 != n then error ("Inequalities must be for the same ambient space.");
		    C1 = if C1#1 == 0 then ((transpose chkQQZZ(transpose C1#0,"hyperplanes"))|map(ZZ^(numRows C1#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C1#0|C1#1),"hyperplanes or condition vector");
		    (map(ZZ^1,ZZ^n,0),map(ZZ^1,ZZ^1,0),submatrix'(C1,{n}),C1_{n}))));
--		    C1 = (chkQQZZ(C1#0,"hyperplanes"),chkQQZZ(C1#1,"condition vector for hyperplanes"));
--		    if C1#1 == 0 then (map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0),C1#0,map(target C1#0,ZZ^1,0))
--		    else (map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0),C1#0,C1#1))));
     LM := flatten apply(L, l -> entries(l#0));
     if LM != {} then Ml = Ml || matrix LM;
     LM = flatten apply(L, l -> entries(l#1));
     if LM != {} then vl = vl || matrix LM;
     LM = flatten apply(L, l -> entries(l#2));
     if LM != {} then Nl = Nl || matrix LM;
     LM = flatten apply(L, l -> entries(l#3));
     if LM != {} then wl = wl || matrix LM;
     if vl == 0*vl and wl == 0*wl then intersection(-Ml,Nl) else intersection(Ml,vl,Nl,wl));
     


-- PURPOSE : Building the Fan 'F'
--   INPUT : 'L',  a list of cones and fans in the same ambient space
--  OUTPUT : The fan of all Cones in 'L' and all Cones in of the fans in 'L' and all their faces
fan = method(TypicalValue => Fan)
fan List := L -> (
     -- Checking for input errors
     if L == {} then error("List of cones and fans must not be empty");
     if (not instance(L#0,Cone)) and (not instance(L#0,Fan)) then error("Input must be a list of cones and fans");
     -- Starting with the first Cone in the list and extracting its information
     C := L#0;
     L = drop(L,1);
     ad := C#"ambient dimension";
     local F;
     if instance(C,Fan) then F = C
     else (
	  rayList := rays C;
	  -- Collecting the rays
	  rayList = apply(numColumns rayList, i-> rayList_{i});
	  -- Generating the new fan
	  F = new Fan from {
	  "generatingCones" => set {C},
	  "ambient dimension" => ad,
	  "top dimension of the cones" => C#"dimension of the cone",
	  "number of generating cones" => 1,
	  "rays" => set rayList,
	  "number of rays" => #rayList,
	  "isPure" => true,
	  symbol cache => new CacheTable});
     -- Checking the remaining list for input errors and reducing fans in the list
     -- to their list of generating cones
     L = flatten apply(L, C -> if instance(C,Cone) then C else if instance(C,Fan) then toList(C#"generatingCones") else 
	  error ("Input must be a list of cones and fans"));       
     -- Adding the remaining cones of the list with 'addCone'
     scan(L, C -> F = addCone(C,F));
     F);


--   INPUT : 'C',  a Cone
--  OUTPUT : The Fan given by 'C' and all of its faces
fan Cone := C -> fan {C};


-- PURPOSE : Building the PolyhedralComplex 'PC'
--   INPUT : 'L',  a list of polyhedra in the same ambient space
--  OUTPUT : The polyhedral complex of all Polyhedra in 'L' and all their faces
polyhedralComplex = method(TypicalValue => PolyhedralComplex)
polyhedralComplex List := L -> (
     -- Checking for input errors
     if L == {} then error("List of polyhedra must not be empty");
     if (not instance(L#0,Polyhedron)) and (not instance(L#0,PolyhedralComplex)) then error("Input must be a list of polyhedra and polyhedral complexes");
     -- Starting with the first Polyhedron in the list and extracting its information
     P := L#0;
     L = drop(L,1);
     ad := P#"ambient dimension";
     local PC;
     if instance(P,PolyhedralComplex) then PC = P
     else (
	  verticesList := vertices P;
	  -- Collecting the vertices
	  verticesList = apply(numColumns verticesList, i-> verticesList_{i});
	  -- Generating the new fan
	  PC = new PolyhedralComplex from {
	       "generatingPolyhedra" => set {P},
	       "ambient dimension" => ad,
	       "top dimension of the polyhedra" => P#"dimension of polyhedron",
	       "number of generating polyhedra" => 1,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => true,
	       symbol cache => new CacheTable});
     -- Checking the remaining list for input errors and reducing polyhedral complexes in the list
     -- to their list of generating polyhedra
     L = flatten apply(L, e -> if instance(e,Polyhedron) then e else if instance(e,PolyhedralComplex) then toList(e#"generatingPolyhedra") else 
	  error ("Input must be a list of polyhedra and polyhedral complexes"));       
     -- Adding the remaining polyhedra of the list with 'addPolyhedron'
     scan(L, e -> PC = addPolyhedron(e,PC));
     PC);

polyhedralComplex Polyhedron := P -> polyhedralComplex {P}


addPolyhedron = method(TypicalValue => PolyhedralComplex)
addPolyhedron (Polyhedron,PolyhedralComplex) := (P,PC) -> (
     -- Checking for input errors
     if P#"ambient dimension" != PC#"ambient dimension" then error("The polyhedra must lie in the same ambient space.");
     -- Extracting data
     GP := toList PC#"generatingPolyhedra";
     d := P#"dimension of polyhedron";
     inserted := false;
     -- Polyhedra in the list 'GP' are ordered by decreasing dimension so we start compatibility checks with 
     -- the cones of higher or equal dimension. For this we divide GP into two seperate lists
     GP = partition(Pf -> (dim Pf) >= d,GP);
     GP = {if GP#?true then GP#true else {},if GP#?false then GP#false else {}};
     if all(GP#0, Pf ->  (
	       (a,b) := areCompatible(Pf,P);
	       -- if 'Pf' and 'P' are not compatible then there is an error
	       if not a then error("The polyhedra are not compatible");
	       -- if they are compatible and 'P' is a face of 'Pf' then 'C' does not 
	       -- need to be added to 'GP'
	       b != P)) then (
	  -- otherwise 'Pf' is still a generating Polyhedron and has to be kept and the remaining polyhedra
	  -- have to be checked
	  GP = GP#0 | {P} | select(GP#1, Pf -> (
		    (a,b) := areCompatible(Pf,P);
		    if not a then error("The polyhedra are not compatible");
		    -- if one of the remaining polyhedra is a face of 'P' this Polyhedron can be dropped
		    b != Pf));
	  inserted = true)     
     -- Otherwise 'P' was already a face of one of the original polyhedra and does not need to be added
     else GP = flatten GP;
     -- If 'P' was added to the Polyhedron as a generating polyhedron then the codim 1 faces on the boundary have to changed to check for 
     -- completeness
     verticesList := toList PC#"vertices";
     if inserted then (
	  -- The vertices of 'P' have to be added
	  Vm := vertices P;
	  Vm = apply(numColumns Vm, i -> Vm_{i});
	  verticesList = unique(verticesList|Vm));
     -- Saving the polyhedral complex
     new PolyhedralComplex from {
	       "generatingPolyhedra" => set GP,
	       "ambient dimension" => P#"ambient dimension",
	       "top dimension of the polyhedra" => (GP#0)#"dimension of polyhedron",
	       "number of generating polyhedra" => #GP,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => dim first GP == dim last GP,
	       symbol cache => new CacheTable})
     
     
--   INPUT : '(L,PC)',  where 'L' is a list of Polyhedra in the same ambient space as the PolyhedralComplex 'PC'
--  OUTPUT : The original PolyhedralComplex 'PC' together with polyhedra in the list 'L'
addPolyhedron (List,PolyhedralComplex) := (L,PC) -> (     
    -- Checking for input errors
    if L == {} then error("The list must not be empty");
    if (not instance(L#0,Polyhedron)) and (not instance(L#0,PolyhedralComplex)) then error("The list may only contain polyhedra and polyhedral complexes");
    if #L == 1 then addPolyhedron(L#0,PC) else addPolyhedron(drop(L,1),addPolyhedron(L#0,PC)))


--   INPUT : '(PC1,PC2)',  where 'PC1' is a PolyhedralComplex in the same ambient space as the PolyhedralComplex 'PC2'
--  OUTPUT : The original fan 'PC2' together with cones of the fan 'PC1'
addPolyhedron (PolyhedralComplex,PolyhedralComplex) := (PC1,PC2) -> (
     -- Checking for input errors
     if ambDim PC2 != ambDim PC1 then error("The polyhedral complexes must be in the same ambient space");
     L := toList PC1#"generatingCones";
     addCone(L,PC2))
     



-- PURPOSE : Adding a Cone to an existing fan 
--   INPUT : '(C,F)',  where 'C' is a Cone in the same ambient space as 'F'
--  OUTPUT : The original fan 'F' together with 'C' if it is compatible with the already existing cones, 
--     	     if not there is an error
addCone = method(TypicalValue => Fan)
addCone (Cone,Fan) := (C,F) -> (
     -- Checking for input errors
     if C#"ambient dimension" != F#"ambient dimension" then error("Cones must lie in the same ambient space");
     -- Extracting data
     GC := toList F#"generatingCones";
     d := C#"dimension of the cone";
     -- We need to memorize for later if 'C' has been inserted
     inserted := false;
     -- Cones in the list 'GC' are ordered by decreasing dimension so we start compatibility checks with 
     -- the cones of higher or equal dimension. For this we divide GC into two seperate lists
     GC = partition(Cf -> (dim Cf) >= d,GC);
     GC = {if GC#?true then GC#true else {},if GC#?false then GC#false else {}};
     if all(GC#0, Cf ->  (
	       (a,b) := areCompatible(Cf,C);
	       -- if 'Cf' and 'C' are not compatible then there is an error
	       if not a then error("The cones are not compatible");
	       -- if they are compatible and 'C' is a face of 'Cf' then 'C' does not 
	       -- need to be added to 'F'
	       b != C)) then (
	  -- otherwise 'Cf' is still a generating Cone and has to be kept and the remaining cones
	  -- have to be checked
	  GC = GC#0 | {C} | select(GC#1, Cf -> (
		    (a,b) := areCompatible(Cf,C);
		    if not a then error("The cones are not compatible");
		    -- if one of the remaining cones is a face of 'C' this Cone can be dropped
		    b != Cf));
	  inserted = true)     
     -- Otherwise 'C' was already a face of one of the original cones and does not need to be added
     else GC = flatten GC;
     -- If 'C' was added to the Fan as a generating cone then the codim 1 faces on the boundary have to changed to check for 
     -- completeness
     rayList := raySort toList F#"rays";
     if inserted then (
	  -- The rays of 'C' have to be added
	  rm := rays C;
	  rm = apply(numColumns rm, i -> rm_{i});
	  rayList = unique(rayList|rm));
     -- Saving the fan
     new Fan from {
	  "generatingCones" => set GC,
	  "ambient dimension" => F#"ambient dimension",
	  "top dimension of the cones" => dim GC#0,
	  "number of generating cones" => #GC,
	  "rays" => set rayList,
	  "number of rays" => #rayList,
	  "isPure" => dim first GC == dim last GC,
	  symbol cache => new CacheTable})


--   INPUT : '(L,F)',  where 'L' is a list of Cones in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones in the list 'L'
addCone (List,Fan) := (L,F) -> (     
    -- Checking for input errors
    if L == {} then error("The list must not be empty");
    if (not instance(L#0,Cone)) and (not instance(L#0,Fan)) then error("The list may only contain cones and fans");
    if #L == 1 then addCone(L#0,F) else addCone(drop(L,1),addCone(L#0,F)))


--   INPUT : '(F1,F)',  where 'F1' is a fan in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones of the fan 'F1'
addCone (Fan,Fan) := (F1,F) -> (
     -- Checking for input errors
     if ambDim F != ambDim F1 then error("The fans must be in the same ambient space");
     L := toList F1#"generatingCones";
     addCone(L,F))


Cone ? Cone := (C1,C2) -> (
     if C1 == C2 then symbol == else (
	  if ambDim C1 != ambDim C2 then ambDim C1 ? ambDim C2 else (
	       if dim C1 != dim C2 then dim C1 ? dim C2 else (
		    R1 := rays C1;
		    R2 := rays C2;
		    if R1 != R2 then (
			 R1 = apply(numColumns R1, i -> R1_{i});
			 R2 = apply(numColumns R2, i -> R2_{i});
			 (a,b) := (set R1,set R2); 
			 r := (sort matrix {join(select(R1,i->not b#?i),select(R2,i->not a#?i))})_{0};
			 if a#?r then symbol > else symbol <)
		    else (
			 R1 = linSpace C1;
			 R2 = linSpace C2;
			 R1 = apply(numColumns R1, i -> R1_{i});
			 R2 = apply(numColumns R2, i -> R2_{i});
			 (c,d) := (set R1,set R2);
			 l := (sort matrix {join(select(R1,i->not d#?i),select(R2,i->not c#?i))})_{0};
			 if c#?l then symbol > else symbol <)))))


-- PURPOSE : Giving the defining affine hyperplanes
ambDim = method(TypicalValue => ZZ)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim PolyhedralObject := X -> X#"ambient dimension"

--   INPUT : 'C'  a Cone 
--  OUTPUT : an integer, the dimension of the ambient space
--ambDim Cone := C -> C#"ambient dimension"

--   INPUT : 'F'  a Fan 
--  OUTPUT : an integer, the dimension of the ambient space
--ambDim Fan := F -> F#"ambient dimension"



-- PURPOSE : Giving the k dimensional Cones of the Fan
--   INPUT : (k,F)  where 'k' is a positive integer and F is a Fan 
--  OUTPUT : a List of Cones
cones = method(TypicalValue => List)
cones(ZZ,Fan) := (k,F) -> (
	-- Checking for input errors
	if k < 0 or dim F < k then error("k must be between 0 and the dimension of the fan.");
	L := select(toList F#"generatingCones", C -> dim C >= k);
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	unique flatten apply(L, C -> faces(dim(C)-k,C)))


-- PURPOSE : Giving the k dimensional Polyhedra of the Polyhedral Complex
--   INPUT : (k,PC)  where 'k' is a positive integer and PC is a PolyhedralComplex 
--  OUTPUT : a List of Polyhedra
polyhedra = method(TypicalValue => List)
polyhedra(ZZ,PolyhedralComplex) := (k,PC) -> (
	-- Checking for input errors
	if k < 0 or dim PC < k then error("k must be between 0 and the dimension of the fan.");
	L := select(toList PC#"generatingPolyhedra", P -> dim P >= k);
	-- Collecting the 'k'-dim faces of all generating polyhedra of dimension greater than 'k'
	unique flatten apply(L, P -> faces(dim(P)-k,P)))

	     
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the polyhedron
dim Polyhedron := P -> P#"dimension of polyhedron"


--   INPUT : 'C'  a Cone 
--  OUTPUT : an integer, the dimension of the Cone
dim Cone := C -> C#"dimension of the cone"


--   INPUT : 'F'  a Fan 
--  OUTPUT : an integer, the highest dimension of Cones in 'F'
dim Fan := F -> F#"top dimension of the cones"


--   INPUT : 'PC'  a PolyhedralComplex
--  OUTPUT : an integer, the highest dimension of polyhedra in 'PC'
dim PolyhedralComplex := PC -> PC#"top dimension of the polyhedra"


-- PURPOSE : Giving the generating Cones of the Fan
--   INPUT : 'F'  a Fan
--  OUTPUT : a List of Cones
maxCones = method(TypicalValue => List)
maxCones Fan := F -> toList F#"generatingCones"


-- PURPOSE : Giving the generating Polyhedra of the PolyhedralComplex
--   INPUT : 'PC'  a PolyhedralComplex
--  OUTPUT : a List of Cones
maxPolyhedra = method(TypicalValue => List)
maxPolyhedra PolyhedralComplex := PC -> toList PC#"generatingPolyhedra"


-- PURPOSE : Giving the defining affine half-spaces
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
halfspaces = method()
halfspaces Polyhedron := P -> P#"halfspaces"


--   INPUT : 'C'  a Cone
--  OUTPUT : 'M', where M is a matrix and C={x in H | Mx>=0}, where 
--		 H is the intersection of the defining hyperplanes
halfspaces Cone := C -> C#"halfspaces"



-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
hyperplanes = method()
hyperplanes Polyhedron := P -> P#"hyperplanes"


--   INPUT : 'C'  a Cone
hyperplanes Cone := C -> C#"hyperplanes"



-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Polyhedron := P -> P#"linealitySpace"


--   INPUT : 'C'  a Cone
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Cone := C -> C#"linealitySpace"


--   INPUT : 'F'  a Fan
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Fan := F -> ((toList F#"generatingCones")#0)#"linealitySpace"


-- PURPOSE : Giving the rays
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the rays of P as column vectors
rays = method()
rays Polyhedron := P -> P#"rays"


--   INPUT : 'C'  a Cone
rays Cone := C -> C#"rays"


--   INPUT : 'F'  a Fan
rays Fan := F -> raySort toList F#"rays"


   
-- PURPOSE : Giving the vertices
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the vertices of P as column vectors
vertices = method()
vertices Polyhedron := P -> P#"vertices"

vertices PolyhedralComplex := PC -> matrix {toList PC#"vertices"}



-- PURPOSE : Tests whether the intersection of two Cones is a face of both
--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : '(Boolean,Cone)'   (true,the intersection),if their intersection is a face of each and 
--     	                        (false,the intersection) otherwise. If the two cones do not lie in 
--     	    	      	   	the same ambient space it returns the empty polyhedron instead of 
--     	    	      	   	the intersection
areCompatible = method()
areCompatible(Cone,Cone) := (C1,C2) -> (
     if C1#"ambient dimension" == C2#"ambient dimension" then (
	  I := intersection(C1,C2);
	  (isFace(I,C1) and isFace(I,C2),I))
     else (false,emptyPolyhedron(C1#"ambient dimension")))


areCompatible(Polyhedron,Polyhedron) := (P1,P2) -> (
     if P1#"ambient dimension" == P2#"ambient dimension" then (
	  I := intersection(P1,P2);
	  (isFace(I,P1) and isFace(I,P2),I))
     else (false,emptyPolyhedron(P1#"ambient dimension")))


-- PURPOSE : Tests whether the intersection of two Polyhedra/Cones is a face of both
commonFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
commonFace(Polyhedron,Polyhedron) := (P,Q) -> (
	if P#"ambient dimension" == Q#"ambient dimension" then (
	     I := intersection(P,Q);
	     isFace(I,P) and isFace(I,Q))
	else false)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
commonFace(Cone,Cone) := (C1,C2) -> (
     if C1#"ambient dimension" == C2#"ambient dimension" then (
	  I := intersection(C1,C2);
	  isFace(I,C1) and isFace(I,C2))
     else false)


--   INPUT : '(C,F)'  a Cone and a Fan
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if the cone has a common face with every generating cone of the fan
commonFace(Cone,Fan) := (C,F) -> if C#"ambient dimension" == F#"ambient dimension" then all(maxCones F, C1 -> commonFace(C,C1)) else false


--   INPUT : '(F,C)'  a Fan and a Cone
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if the cone has a common face with every generating cone of the fan
commonFace(Fan,Cone) := (F,C) -> commonFace(C,F)


--   INPUT : '(F1,F2)'  two Fans
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if all generating cones of 'F1' have a common face with every generating cone of 'F2'
commonFace(Fan,Fan) := (F1,F2) -> all(maxCones F1, C -> commonFace(C,F2))


--   INPUT : 'L'  a List
--  OUTPUT : 'true' or 'false'
commonFace List := L -> all(#L-1, i -> all(i+1..#L-1, j -> commonFace(L#i,L#j)))



-- PURPOSE : Check if 'P' contains 'Q'
--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
contains = method(TypicalValue => Boolean)
contains(Polyhedron,Polyhedron) := (P,Q) -> (
      -- checking for input errors
      if P#"ambient dimension" =!= Q#"ambient dimension" then error("Polyhedra must lie in the same ambient space");
      -- Saving the equations of P and vertices/rays of Q
      (A,B) := P#"homogenizedHalfspaces";
      (C,D) := Q#"homogenizedVertices";
      A = transpose A;
      B = transpose B;
      E := A*C;
      -- Checking if vertices/rays of Q satisfy the equations of P
      all(flatten entries E, e -> e <= 0) and A*D == 0*A*D and B*C == 0*B*C and B*D == 0*B*D)


-- PURPOSE : Check if 'C1' contains 'C2'
--   INPUT : '(C1,C2)'  two Cones
contains(Cone,Cone) := (C1,C2) -> (
      -- checking for input errors
      if C1#"ambient dimension" =!= C2#"ambient dimension" then error("Cones must lie in the same ambient space");
      -- Saving the equations of C1 and rays of C2
      (A,B) := C1#"dualgens";
      (C,D) := C2#"genrays";
      A = transpose A;
      B = transpose B;
      E := A*C;
      -- Checking if the rays of C2 satisfy the equations of C1
      all(flatten entries E, e -> e <= 0) and A*D == 0*A*D and B*C == 0*B*C and B*D == 0*B*D)
 

 
-- PURPOSE : Check if 'C' contains 'P'
--   INPUT : '(C,P)'  a Cone and a Polyhedron
contains(Cone,Polyhedron) := (C,P) -> (
      -- checking for input errors
      if C#"ambient dimension" =!= P#"ambient dimension" then error("Cone and Polyhedron must lie in the same ambient space");
      -- Saving the equations of C and vertices/rays of P
      M := makePrimitiveMatrix P#"vertices" | P#"rays";
      LS := P#"linealitySpace";
      C1 := posHull(M,LS);
      contains(C,C1))



-- PURPOSE : Check if 'P' contains 'C'
--   INPUT : '(P,C)'  a Polyhedron and a Cone
contains(Polyhedron,Cone) := (P,C) -> (
      -- checking for input errors
      if C#"ambient dimension" =!= P#"ambient dimension" then error("Polyhedron and Cone must lie in the same ambient space");
      -- Saving the cone 'C' as a polyhedron and using the function on two polyhedra
      Q := coneToPolyhedron C;
      contains(P,Q))



-- PURPOSE : Check if 'P' contains 'p'
--   INPUT : '(P,p)'  a Polyhedron 'P' and a point 'p' given as a matrix
contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if P#"ambient dimension" =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(P,convexHull p))



-- PURPOSE : Check if 'C' contains 'p'
--   INPUT : '(C,p)'  a Cone 'C' and a point 'p' given as a matrix
contains(Cone,Matrix) := (C,p) -> (
      -- checking for input errors
      if C#"ambient dimension" =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(C,convexHull p))



-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Cone) := (L,C) -> any(L, C1 -> C1 == C)
 
 
-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Polyhedron) := (L,P) -> any(L, Q -> Q == P)
 
 
-- PURPOSE : Check if 'F' contains 'C'
--   INPUT : '(F,C)'  a Fan 'F' and a Cone 'C'
contains(Fan,Cone) := (F,C) -> (
      -- Checking for input errors
      if ambDim F != ambDim C then error("Fan and Cone must lie in the same ambient space");
      -- Making the list of cones of same dimension as 'C'
      L := cones(dim C,F);
      contains(L,C))
      


Polyhedron == Polyhedron := (P,Q) -> P === Q

Cone == Cone := (C1,C2) -> C1 === C2

Fan == Fan := (F1,F2) -> F1 === F2




-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact Polyhedron := P -> P#"linealitySpace" == 0 and P#"rays" == 0


-- PURPOSE : Tests if a Fan is complete
--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isComplete = method(TypicalValue => Boolean)
isComplete Fan := F -> (
     if not F.cache.?isComplete then (
	  n := F#"top dimension of the cones";
	  F.cache.isComplete = if n == ambDim F then (
	       symmDiff := (x,y) -> ((x,y) = (set x,set y); toList ((x-y)+(y-x)));
	       Lfaces := {};
	       scan(maxCones F, C -> if dim C == n then Lfaces = symmDiff(Lfaces,faces(1,C)));
	       Lfaces == {})
	  else false);
     F.cache.isComplete)

isComplete PolyhedralComplex := PC -> (
     if not PC.cache.?isComplete then (
	  n := PC#"top dimension of the polyhedra";
	  PC.cache.isComplete = if n == ambDim PC then (
	       symmDiff := (x,y) -> ((x,y) = (set x,set y); toList ((x-y)+(y-x)));
	       Lfaces := {};
	       scan(maxPolyhedra PC, P -> if dim P == n then Lfaces = symmDiff(Lfaces,faces(1,P)));
	       Lfaces == {})
	  else false);
     PC.cache.isComplete)


-- PURPOSE : Tests if a Polyhedron is empty
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isEmpty = method(TypicalValue => Boolean)
isEmpty Polyhedron := P -> P#"dimension of polyhedron" == -1


     
-- PURPOSE : Tests if the first Polyhedron/Cone is a face of the second Polyhedron/Cone
isFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
isFace(Polyhedron,Polyhedron) := (P,Q) -> (
     -- Checking if the two polyhedra lie in the same space and computing the dimension difference
     c := Q#"dimension of polyhedron" - P#"dimension of polyhedron";
     if P#"ambient dimension" == Q#"ambient dimension" and c >= 0 then (
	  -- Checking if P is the empty polyhedron
	  if c > Q#"dimension of polyhedron" then true
	  -- Checking if one of the codim 'c' faces of Q is P
	  else any(faces(c,Q), f -> f === P))
     else false)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
isFace(Cone,Cone) := (C1,C2) -> (
     c := C2#"dimension of the cone" - C1#"dimension of the cone";
     -- Checking if the two cones lie in the same space and the dimension difference is positive
     if C1#"ambient dimension" == C2#"ambient dimension" and c >= 0 then (
	  -- Checking if one of the codim 'c' faces of C2 is C1
	  any(faces(c,C2), f -> f === C1))
     else false)


-- PURPOSE : Checks if the polyhedron is a lattice polytope
--   INPUT : 'P'  a Polyhedron, which must be compact
--  OUTPUT : 'true' or 'false'
-- COMMENT : Tests if the vertices are in ZZ
isLatticePolytope = method()
isLatticePolytope Polyhedron := Boolean => P -> isCompact P and liftable(vertices P,ZZ)

-- PURPOSE : Checks if the polytope is normal
--   INPUT : 'P'  a Polyhedron, which must be compact
--  OUTPUT : 'true' or 'false'
-- COMMENT : The polytope is normal if the lattice of the cone over the polytope embedded on height 1 
--     	     is generated by the lattice points on height 1
isNormal Polyhedron := (cacheValue symbol isNormal)(P -> (
     	  -- Checking for input errors
     	  if not isCompact P then error ("The polyhedron must be compact");
     	  -- Computing the Hilbert basis of the cone over 'P' on height 1
	  V := vertices P || map(QQ^1,source vertices P,(i,j) -> 1);
	  L := hilbertBasis posHull V;
	  n := ambDim P;
	  -- Do all lattice points lie in height one?
	  all(L,v -> v_(n,0) == 1)))
     

-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed = method(TypicalValue => Boolean)
isPointed Cone := C -> rank C#"linealitySpace" == 0


--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     if not F.cache.?isPointed then F.cache.isPointed = isPointed((toList F#"generatingCones")#0);
     F.cache.isPointed)



-- PURPOSE : Tests if a Fan is projective
--   INPUT : 'F'  a Fan
--  OUTPUT : a Polyhedron, which has 'F' as normal fan, if 'F' is projective or the empty polyhedron
isPolytopal = method(TypicalValue => Boolean)
isPolytopal Fan := F -> (
     if not F.cache.?isPolytopal then (
	  F.cache.isPolytopal = false;
	  -- First of all the fan must be complete
     	  if isComplete F then (
	       -- Extracting the generating cones, the ambient dimension, the codim 1 
	       -- cones (corresponding to the edges of the polytope if it exists)
	       i := 0;
	       L := hashTable apply(toList F#"generatingCones", l -> (i=i+1; i=>l));
	       n := F#"ambient dimension";
	       edges := cones(n-1,F);
	       -- Making a table that indicates in which generating cones each 'edge' is contained
	       edgeTCTable := hashTable apply(edges, e -> select(1..#L, j -> contains(L#j,e)) => e);
	       i = 0;
	       -- Making a table of all the edges where each entry consists of the pair of top cones corr. to
	       -- this edge, the codim 1 cone, an index number i, and the edge direction from the first to the
	       -- second top Cone
	       edgeTable := apply(pairs edgeTCTable, e -> (i=i+1; 
		    	 v := transpose hyperplanes e#1;
		    	 if not contains(dualCone L#((e#0)#0),v) then v = -v;
		    	 (e#0, e#1, i, v)));
	       edgeTCNoTable := hashTable apply(edgeTable, e -> e#0 => (e#2,e#3));
	       edgeTable = hashTable apply(edgeTable, e -> e#1 => (e#2,e#3));
	       -- Computing the list of correspondencies, i.e. for each codim 2 cone ( corresponding to 2dim-faces of the polytope) save 
	       -- the indeces of the top cones containing it
	       corrList := hashTable {};
	       scan(keys L, j -> (corrList = merge(corrList,hashTable apply(faces(2,L#j), C -> C => {j}),join)));
	       corrList = pairs corrList;
          << corrList << endl;
	       --  Generating the 0 matrix for collecting the conditions on the edges
	       m := #(keys edgeTable);
	       -- for each entry of corrlist another matrix is added to HP
	       HP := flatten apply(#corrList, j -> (
		    	 v := corrList#j#1;
		    	 HPnew := map(ZZ^n,ZZ^m,0);
		    	 -- Scanning trough the top cones containing the active codim2 cone and order them in a circle by their 
		    	 -- connecting edges
		    	 v = apply(v, e -> L#e);
		    	 C := v#0;
		    	 v = drop(v,1);
		    	 C1 := C;
		    	 nv := #v;
             << "v: " << v << endl;
		    	 scan(nv, i -> (
			      	   i = position(v, e -> dim intersection(C1,e) == n-1);
			      	   C2 := v#i;
			      	   v = drop(v,{i,i});
			      	   (a,b) := edgeTable#(intersection(C1,C2));
			      	   if not contains(dualCone C2,b) then b = -b;
			      	   -- 'b' is the edge direction inserted in column 'a', the index of this edge
			      	   HPnew = HPnew_{0..a-2} | b | HPnew_{a..m-1};
			      	   C1 = C2));
		    	 C3 := intersection(C,C1);
		    	 (a,b) := edgeTable#C3;
		    	 if not contains(dualCone C,b) then b = -b;
		    	 -- 'b' is the edge direction inserted in column 'a', the index of this edge
		    	 -- the new restriction is that the edges ''around'' this codim2 Cone must add up to 0
		    	 entries(HPnew_{0..a-2} | b | HPnew_{a..m-1})));
	       if HP != {} then HP = matrix HP
	       else HP = map(ZZ^0,ZZ^m,0);
	       -- Find an interior vector in the cone of all positive vectors satisfying the restrictions
	       v := flatten entries interiorVector intersection(id_(ZZ^m),HP);
	       M := {};
	       -- If the vector is strictly positive then there is a polytope with 'F' as normalFan
	       if all(v, e -> e > 0) then (
	       	    -- Construct the polytope
	       	    i = 1;
	       	    -- Start with the origin
	       	    p := map(ZZ^n,ZZ^1,0);
	       	    M = {p};
	       	    Lyes := {};
	       	    Lno := {};
	       	    vlist := apply(keys edgeTCTable,toList);
	       	    -- Walk along all edges recursively
	       	    edgerecursion := (i,p,vertexlist,Mvertices) -> (
		    	 vLyes := {};
		    	 vLno := {};
		    	 -- Sorting those edges into 'vLyes' who emerge from vertex 'i' and the rest in 'vLno'
		    	 vertexlist = partition(w -> member(i,w),vertexlist);
		    	 if vertexlist#?true then vLyes = vertexlist#true;
		    	 if vertexlist#?false then vLno = vertexlist#false;
		    	 -- Going along the edges in 'vLyes' with the length given in 'v' and calling edgerecursion again with the new index of the new 
		    	 -- top Cone, the new computed vertex, the remaining edges in 'vLno' and the extended matrix of vertices
		    	 scan(vLyes, w -> (
			      	   w = toSequence w;
			      	   j := edgeTCNoTable#w;
			      	   if w#0 == i then (
				   	(vLno,Mvertices) = edgerecursion(w#1,p+(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p+(j#1)*(v#((j#0)-1)))))
			      	   else (
				   	(vLno,Mvertices) = edgerecursion(w#0,p-(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p-(j#1)*(v#((j#0)-1)))))));
		    	 (vLno,Mvertices));
	       	    -- Start the recursion with vertex '1', the origin, all edges and the vertexmatrix containing already the origin
	       	    M = unique ((edgerecursion(i,p,vlist,M))#1);
	       	    M = matrix transpose apply(M, m -> flatten entries m);
	       	    -- Computing the convex hull
	       	    F.cache.polytope = convexHull M;
	       	    F.cache.isPolytopal = true)));
     F.cache.isPolytopal)



-- PURPOSE : Checks if the Fan is of pure dimension
--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isPure = method(TypicalValue => Boolean)
isPure Fan := F -> F#"isPure"


isPure PolyhedralComplex := PC -> PC#"isPure"

-- PURPOSE : Checks if a lattice polytope is reflexive
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isReflexive = method(TypicalValue => Boolean)
isReflexive Polyhedron := (cacheValue symbol isReflexive)(P -> isLatticePolytope P and inInterior(matrix toList(ambDim P:{0}),P) and isLatticePolytope polar P)


isSimplicial = method(TypicalValue => Boolean)

isSimplicial PolyhedralObject := (cacheValue symbol isSimplicial)(X -> (
	if instance(X,Cone) then (isPointed X and numColumns rays X == dim X)
	else if instance(X,Fan) then all(maxCones X,isSimplicial)
	else if instance(X,Polyhedron) then (isCompact X and numColumns vertices X == dim X + 1)
	else all(maxPolyhedra X,isSimplicial)))
--isSimplicial Cone := (cacheValue symbol isSimplicial)(C -> isPointed C and numColumns rays C == dim C)
--isSimplicial Fan := (cacheValue symbol isSimplicial)(F -> all(maxCones F,isSimplicial))
--isSimplicial Polyhedron := (cacheValue symbol isSimplicial)(P -> isCompact P and numColumns vertices P == dim P +1)
--isSimplicial PolyhedralComplex := (cacheValue symbol isSimplicial)(PC -> all(maxPolyhedra PC,isSimplicial))


-- PURPOSE : Checks if the input is smooth
isSmooth = method(TypicalValue => Boolean)

--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isSmooth Cone := C -> (
     -- generating the non-linealityspace cone of C
     R := lift(transpose rays C,ZZ);
     n := dim C - C#"dimension of lineality space";
     -- if the cone is full dimensional then it is smooth iff its rays form a basis over ZZ
     numRows R == n and (M := (smithNormalForm R)#0; product apply(n, i -> M_(i,i)) == 1))
     
	   

--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isSmooth Fan := F -> (
     if not F.cache.?isSmooth then F.cache.isSmooth = all(toList F#"generatingCones",isSmooth);
     F.cache.isSmooth)


-- PURPOSE : Checks if a polytope is very ample
--   INPUT : 'P'  a Polyhedron, which must be compact
--  OUTPUT : 'true' or 'false'
isVeryAmple = method()
isVeryAmple Polyhedron := P -> (
     if not isCompact P then error("The polyhedron must be compact");
     if not dim P == ambDim P then error("The polyhedron must be full dimensional");
     if not isLatticePolytope P then error("The polyhedron must be a lattice polytope");
     if not P.cache.?isVeryAmple then (
	  E := apply(faces(dim P -1, P), e -> (e = vertices e; {e_{0},e_{1}}));
     	  V := vertices P;
     	  V = apply(numColumns V, i -> V_{i});
     	  HS := -(halfspaces P)#0;
     	  HS = apply(numRows HS, i -> HS^{i});
     	  P.cache.isVeryAmple = all(V, v -> (
	       	    Ev := select(E, e -> member(v,e));
	       	    Ev = apply(Ev, e -> makePrimitiveMatrix(if e#0 == v then e#1-e#0 else e#0-e#1));
	       	    ind := (smithNormalForm matrix {Ev})_0;
	       	    ind = product toList apply(rank ind, i-> ind_(i,i));
	       	    ind == 1 or (
		    	 EvSums := apply(subsets Ev, s -> sum(s|{v}));
	       	    	 all(EvSums, e -> contains(P,e)) or (
		    	      Ev = matrix{Ev};
		    	      HSV := matrix for h in HS list if all(flatten entries(h*Ev), e -> e >= 0) then {h} else continue;
		    	      C := new Cone from {
	   		      	   "ambient dimension" => numRows Ev,
	   		      	   "dimension of the cone" => numRows Ev,
	   		      	   "dimension of lineality space" => 0,
	   		      	   "linealitySpace" => map(ZZ^(numRows Ev),ZZ^0,0),
	   		      	   "number of rays" => numColumns Ev,
	   		      	   "rays" => Ev,
	   		      	   "number of facets" => numColumns HSV,
	   		      	   "halfspaces" => HSV,
	   		      	   "hyperplanes" => map(ZZ^0,ZZ^(numRows Ev),0),
	   		      	   "genrays" => (Ev,map(ZZ^(numRows Ev),ZZ^0,0)),
	   		      	   "dualgens" => (-(transpose HSV),map(ZZ^(numRows Ev),ZZ^0,0)),
	   		      	   symbol cache => new CacheTable};
		    	      HB := hilbertBasis C;
		    	      all(HB, e -> contains(P,e+v)))))));
     P.cache.isVeryAmple);


boundaryMap = method(TypicalValue => Matrix)
boundaryMap (ZZ,Polyhedron) := (i,P) -> (
     L1 := faces(dim P - i,P);
     L2 := faces(dim P - i + 1,P);
     L1 = apply(L1, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     L2 = apply(L2, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     transpose matrix apply(L1, l1 -> (
	       apply(L2, l2 -> (
			 if isSubset(set l2,set l1) then (
			      l3 := toList(set l1 - set l2);
			      l3 = apply(l3, e -> position(l1, e1 -> e1 == e));
			      l := #l3; 
			      k := #l2; 
			      (-1)^(k*l + sum l3 - substitute((l^2-l)/2,ZZ))) else 0)))))

boundaryMap (ZZ,PolyhedralComplex) := (i,PC) -> (
     L1 := polyhedra(i,PC);
     L2 := polyhedra(i-1,PC);
     L1 = apply(L1, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     L2 = apply(L2, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     transpose matrix apply(L1, l1 -> (
	       apply(L2, l2 -> (
			 if isSubset(set l2,set l1) then (
			      l3 := toList(set l1 - set l2);
			      l3 = apply(l3, e -> position(l1, e1 -> e1 == e));
			      l := #l3; 
			      k := #l2; 
			      (-1)^(k*l + sum l3 - substitute((l^2-l)/2,ZZ))) else 0)))))


-- PURPOSE : Compute the dual face lattice
dualFaceLattice = method(TypicalValue => List)

--   INPUT : '(k,P)',  where 'k' is an integer between 0 and dim 'P' where P is a Polyhedron
--  OUTPUT :  a list, where each entry gives a face of 'P' of dim 'k'. Each entry is a list
-- 	      of the positions of the defining halfspaces
dualFaceLattice(ZZ,Cone) := (k,C) -> (
     L := faceBuilderCone(dim C - k,C);
     HS := halfspaces C;
     HS = apply(numRows HS, i -> HS^{i});
     apply(L, l -> positions(HS, hs -> all(toList l, v -> hs*v == 0))))


dualFaceLattice(ZZ,Polyhedron) := (k,P) -> (
     L := faceBuilder(dim P - k,P);
     HS := halfspaces P;
     HS = apply(numRows HS#0, i -> ((HS#0)^{i},(HS#1)^{i}));
     apply(L, l -> (
	       l = (toList l#0,toList l#1);
	       positions(HS, hs -> (all(l#0, v -> (hs#0)*v - hs#1 == 0) and all(l#1, r -> (hs#0)*r == 0))))))

--   INPUT : 'P',  a Polyhedron
--  OUTPUT :  a list, where each entry is dual face lattice of a certain dimension going from 0 to dim 'P'
dualFaceLattice Polyhedron := P -> apply(dim P + 1, k -> dualFaceLattice(dim P - k,P))

dualFaceLattice Cone := C -> apply(dim C + 1, k -> dualFaceLattice(dim C - k,C))

faceLattice = method(TypicalValue => List)
faceLattice(ZZ,Polyhedron) := (k,P) -> (
     L := faceBuilder(k,P);
     V := vertices P;
     R := rays P;
     V = apply(numColumns V, i -> V_{i});
     R = apply(numColumns R, i -> R_{i});
     apply(L, l -> (
	       l = (toList l#0,toList l#1);
	       (sort apply(l#0, e -> position(V, v -> v == e)),sort apply(l#1, e -> position(R, r -> r == e))))))

faceLattice(ZZ,Cone) := (k,C) -> (
     L := faceBuilderCone(k,C);
     R := rays C;
     R = apply(numColumns R, i -> R_{i});
     apply(L, l -> sort apply(toList l, e -> position(R, r -> r == e))))


faceLattice Polyhedron := P -> apply(dim P + 1, k -> faceLattice(dim P - k,P))


faceLattice Cone := C -> apply(dim C + 1, k -> faceLattice(dim C - k,C))


faceOf = method(TypicalValue => PolyhedralObject)
faceOf Polyhedron := (cacheValue symbol faceOf)( P -> P)
	  
     	  


-- PURPOSE : Computing the faces of codimension 'k' of 'P'
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'P'  plus one a polyhedron
--  OUTPUT : a List, containing the faces as polyhedra
faces = method(TypicalValue => List)
faces(ZZ,Polyhedron) := (k,P) -> (
     --local faceOf;
     if k == dim P +1 then (
	  Pn := emptyPolyhedron ambDim P;
	  (cacheValue symbol faceOf)(Pn -> P);
	  --Pn.cache.faceOf := P;
	  {Pn})
     else (
     	  L := faceBuilder(k,P);
     	  LS := linSpace P;
     	  -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
     	  apply(L, l -> (
	       	    l = (toList l#0,toList l#1);
	       	    V := matrix transpose apply(l#0, e -> flatten entries e);
	       	    R := if l#1 != {} then matrix transpose apply(l#1, e -> flatten entries e) else map(target V,QQ^1,0);
	       	    if LS != 0 then R = R | LS | -LS;
	       	    Pnew := convexHull(V,R);
		    (cacheValue symbol faceOf)(Pnew -> P);
	       	    --Pnew.cache.faceOf := P;
	       	    Pnew))))


--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a cone
--  OUTPUT : a List, containing the faces as cones
faces(ZZ,Cone) := (k,C) -> (
     L := faceBuilderCone(k,C);
     LS := linSpace C;
     --local faceOf;
     -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
     apply(L, l -> (
	       Cnew := posHull(matrix transpose apply(toList l, e -> flatten entries e),LS);
	       (cacheValue symbol faceOf)(Cnew -> C);
	       --Cnew.cache.faceOf = C;
	       Cnew)))


     
-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)
fVector Polyhedron := P -> apply(P#"dimension of polyhedron" + 1, d -> #faces(dim P - d,P))


--   INPUT : 'C'  a Cone
--  OUTPUT : a List of integers, starting with the number of vertices and going up in dimension
fVector Cone := C -> apply(C#"dimension of the cone" + 1, d -> #faces(dim C - d,C))


-- PURPOSE : Computing the Hilbert basis of a Cone 
--   INPUT : 'C',  a Cone
--  OUTPUT : 'L',  a list containing the Hilbert basis as one column matrices 
hilbertBasis = method(TypicalValue => List)
hilbertBasis Cone := C -> (
     -- Computing the row echolon form of the matrix M
     ref := M -> (
	  n := numColumns M;
	  s := numRows M;
	  BC := map(ZZ^n,ZZ^n,1);
	  m := min(n,s);
	  -- Scan through the first square part of 'M'
	  i := 0;
	  stopper := 0;
	  while i < m and stopper < n do (
		    -- Selecting the first non-zero entry after the i-th row in the i-th column
		    j := select(1,toList(i..s-1),k -> M_i_k != 0);
		    -- if there is a non-zero entry, scan the remaining entries and compute the reduced form for this column
		    if j != {} then (
			 j = j#0;
			 scan((j+1)..(s-1), k -> (
				   if M_i_k != 0 then (
					a := M_i_j;
					b := M_i_k;
					L := gcdCoefficients(a,b);
					a = substitute(a/(L#0),ZZ);
					b = substitute(b/(L#0),ZZ);
					M = M^{0..j-1} || (L#1)*M^{j} + (L#2)*M^{k} || M^{j+1..k-1} || (-b)*M^{j} + a*M^{k} || M^{k+1..s-1})));
			 if i != j then (
			      M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1});
			 if M_i_i < 0 then M = M^{0..i-1} || -M^{i} || M^{i+1..s-1})
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
	  N := gens ker(-h|A);
	  N = transpose (ref transpose N)#0;
	  N_{0}^{1..(numRows N)-1});
     A := C#"halfspaces";
     if C#"hyperplanes" != 0 then A = A || C#"hyperplanes" || -(C#"hyperplanes");
     A = substitute(A,ZZ);
     -- Use the project and lift algorithm to compute a basis of the space of vectors positive on 'A' whose preimages are the HilbertBasis
     (B,BC) := ref transpose A; 
     H := constructHilbertBasis B;
     BC = inverse transpose BC;
     apply(H,h -> preim(BC*h,A)))


-- PURPOSE : Get the pairs of incompatible cones in a list of cones
--   INPUT : 'L',  a list of cones and fans
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of cones/fans that are  
--                 	not compatible
incompCones = method(TypicalValue => List)
incompCones List := L -> (
     if any(L, l -> (not instance(l,Cone)) and (not instance(l,Fan))) then error("The list may only contain cones and fans");
     select(apply(subsets(L,2),toSequence), p -> not commonFace p))


--   INPUT : '(C,F)',  a cone and a fan
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of 'C' with the cones of 
--                 	'F' that are not compatible
incompCones(Cone,Fan) := (C,F) -> select(apply(maxCones F, f -> (C,f)), p -> not commonFace p)


--   INPUT : '(F,C)',  a fan and a cone
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of 'C' with the cones of 
--                 	'F' that are not compatible
incompCones(Fan,Cone) := (F,C) -> select(apply(maxCones F, f -> (f,C)), p -> not commonFace p)


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of cones of 'F1' and cones of 
--                 	'F2' that are not compatible
incompCones(Fan,Fan) := (F1,F2) -> flatten apply(maxCones F1, C1 -> flatten apply(maxCones F2, C2 -> if not commonFace(C1,C2) then (C1,C2) else {}))


-- PURPOSE : Get the pairs of incompatible polyhedra in a list of polyhedra
--   INPUT : 'L',  a list of polyhedra and polyhedral complexes
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of polyhedra/polyhedral 
--                      complexes that are not compatible
incompPolyhedra = method(TypicalValue => List)
incompPolyhedra List := L -> (
     if any(L, l -> (not instance(l,Polyhedron)) and (not instance(l,PolyhedralComplex))) then error("The list may only contain polyhedra and polyhedral complexes");
     select(apply(subsets(L,2),toSequence), p -> not commonFace p))


--   INPUT : '(P,PC)',  a Polyhedron and a PolyhedralComplex
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of 'P' with the polyhedra of 
--                 	'PC' that are not compatible
incompPolyhedra(Polyhedron,PolyhedralComplex) := (P,PC) -> select(apply(maxPolyhedra PC, p -> (P,p)), e -> not commonFace e)


--   INPUT : '(PC,P)',  a PolyhedralComplex and a Polyhedron
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of 'P' with the polyhedra of 
--                 	'PC' that are not compatible
incompPolyhedra(PolyhedralComplex,Polyhedron) := (PC,P) -> select(apply(maxPolyhedra PC, p -> (p,P)), e -> not commonFace e)


--   INPUT : '(PC1,PC2)',  two PolyhedralComplexes
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of polyhedra of 'PC1' and polyhedra of 
--                 	'PC2' that are not compatible
incompPolyhedra(PolyhedralComplex,PolyhedralComplex) := (PC1,PC2) -> flatten apply(maxPolyhedra PC1, P1 -> flatten apply(maxPolyhedra PC2, P2 -> if not commonFace(P1,P2) then (P1,P2) else {}))
     


-- PURPOSE : Checking if a point is an interior point of a Polyhedron or Cone 
inInterior = method(TypicalValue => Boolean)


--   INPUT : '(p,P)',  where 'p' is a point given by a matrix and 'P' is a Polyhedron
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Polyhedron) := (p,P) -> (
     HP := hyperplanes P;
     HP = (HP#0 * p)-HP#1;
     all(flatten entries HP, e -> e == 0) and (
	  HS := halfspaces P;
	  HS = (HS#0 * p)-HS#1;
	  all(flatten entries HS, e -> e < 0)))


--   INPUT : '(p,C)',  where 'p' is a point given by a matrix and 'C' is a Cone
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Cone) := (p,C) -> (
     HP := hyperplanes C;
     all(flatten entries(HP*p), e -> e == 0) and (
	  HS := halfspaces C;
	  all(flatten entries(HS*p), e -> e > 0)))


-- PURPOSE : Computing a point in the relative interior of a cone or Polyhedron 
interiorPoint = method(TypicalValue => Matrix)


--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'p',  a point given as a matrix
interiorPoint Polyhedron := P -> (
     -- Checking for input errors
     if isEmpty P then error("The polyhedron must not be empty");
     Vm := vertices P | promote(rays P,QQ);
     n := numColumns Vm;
     ones := matrix toList(n:{1/n});
     -- Take the '1/n' weighted sum of the vertices
     Vm * ones)


-- PURPOSE : Computing an interior vector of a cone
--   INPUT : 'C',  a Cone
--  OUTPUT : 'p',  a point given as a matrix 
interiorVector = method(TypicalValue => Matrix)
interiorVector Cone := C -> (
     if dim C == 0 then map(ZZ^(ambDim C),ZZ^1,0)
     else (
	  Rm := rays C;
	  ones := matrix toList(numColumns Rm:{1});
	  -- Take the sum of the rays
	  iv := Rm * ones;
	  transpose matrix apply(entries transpose iv, w -> (g := abs gcd w; apply(w, e -> e//g)))));
--	  if M != 0 then lift(transpose matrix apply(entries transpose M, w -> (g := gcd w; apply(w, e -> e//g))),ZZ) else lift(M,ZZ);
--	  d := abs gcd flatten entries iv;
--	  (1/d)*iv))

-- PURPOSE : Computing the interior lattice points of a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the interior lattice points
interiorLatticePoints = method(TypicalValue => List)
interiorLatticePoints Polyhedron := (cacheValue symbol interiorLatticePoints)(P -> (
     L := latticePoints P;
     select(L,e -> inInterior(e,P))))


-- PURPOSE : Computing the lattice points of a compact Polyhedron 
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the lattice points of 'P'
latticePoints = method(TypicalValue => List)
latticePoints Polyhedron := P -> (
     if not P.cache.?latticePoints then (
	  -- Checking for input errors
	  if  not isCompact P then error("The polyhedron must be compact");
	  -- Recursive function that intersects the polyhedron with parallel hyperplanes in the axis direction
	  -- in which P has its minimum extension
	  latticePointsRec := P -> (
	       -- Finding the direction with minimum extension of P
	       V := entries vertices P;
	       n := ambDim P;
	       minv := apply(V,min);
	       maxv := apply(V,max);
	       minmaxv := maxv-minv;
	       pos := min minmaxv;
	       pos = position(minmaxv,v -> v == pos);
	       -- Determining the lattice heights in this direction
	       L := toList({{ceiling minv_pos}}..{{floor maxv_pos}});
	       -- If the dimension is one, than it is just a line and we take the lattice points
	       if n == 1 then apply(L,matrix)
	       -- Otherwise intersect with the hyperplanes and project into the hyperplane
	       else flatten apply(L,p -> (
			 NP := intersection {P,{map(QQ^1,QQ^n,(i,j) -> if j == pos then 1 else 0),matrix p}};
			 if NP#"number of vertices" == 1 then (
			      v := vertices NP;
			      if promote(substitute(v,ZZ),QQ) == v then substitute(v,ZZ) else {})
			 else (
			      A := matrix drop((entries id_(QQ^n)),{pos,pos});
			      apply(latticePointsRec affineImage(A,NP),v -> v^{0..(pos-1)} || matrix p || v^{pos..(n-2)})))));
	  -- Checking if the polytope is just a point
	  if dim P == 0 then P.cache.latticePoints = if liftable(vertices P,ZZ) then {lift(vertices P,ZZ)} else {}
	  -- Checking if the polytope is full dimensional
	  else if (dim P == ambDim P) then P.cache.latticePoints = latticePointsRec P
	  -- If not checking first if the affine hull of P contains lattice points at all and if so projecting the polytope down
	  -- so that it becomes full dimensional with a map that keeps the lattice
	  else (
	       (M,v) := hyperplanes P;
	       -- Finding a lattice point in the affine hull of P
	       b := if all(entries M, e -> gcd e == 1) then (
		    -- Computing the Smith Normal Form to solve the equation over ZZ
		    (M1,Lmatrix,Rmatrix) := smithNormalForm substitute(M,ZZ);
		    v1 := flatten entries (Lmatrix*v);
		    w := apply(numRows M1, i -> M1_(i,i));
		    -- Checking if the system is at least solvable over QQ
		    if all(#w, i -> w#i != 0 or v1#i == 0) then (
			 -- If it is, then solve over QQ
			 w = apply(#w, i -> (v1#i/w#i,v1#i%w#i));
			 if all(w, e -> e#1 == 0) then (
			      -- If the solution is in fact in ZZ then return it
			      w = transpose matrix{apply(w,first) | toList(numColumns M1 - numRows M1:0)};
			      Rmatrix * w)));
	       -- If there is no lattice point in the affine hull then P has none
	       if b === null then P.cache.latticePoints = {}
	       else (
		    A := gens ker substitute(M,ZZ);
		    -- Project the translated polytope, compute the lattice points and map them back
		    P.cache.latticePoints = apply(latticePoints affinePreimage(A,P,b),e -> substitute(A*e + b,ZZ)))));
     P.cache.latticePoints)



-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its maximum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its maximum
maxFace = method()
maxFace (Matrix,Polyhedron) := (v,P) -> minFace(-v,P)


--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its maximum
maxFace (Matrix,Cone) := (v,C) -> minFace(-v,C)



-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its minimum
minFace = method()
minFace (Matrix,Polyhedron) := (v,P) -> (
     -- Checking for input errors
     if numColumns v =!= 1 or numRows v =!= P#"ambient dimension" then error("The vector must lie in the same space as the polyhedron");
     C := dualCone tailCone P;
     V := vertices P;
     R := rays P;
     LS := linSpace P;
     -- The weight must lie in the dual of the tailcone of the polyhedron, otherwise there is 
     -- no minimum and the result is the empty polyhedron
     if contains(C,v) then (
	  -- Compute the values of 'v' on the vertices of 'V'
	  Vind := flatten entries ((transpose v)*V);
	  -- Take the minimal value(s)
	  Vmin := min Vind;
	  Vind = positions(Vind, e -> e == Vmin);
	  -- If 'v' is in the interior of the dual tailCone then the face is exactly spanned 
	  -- by these vertices
	  if inInterior(v,C) then convexHull(V_Vind,LS | -LS)
	  else (
	       -- Otherwise, one has to add the rays of the tail cone that are orthogonal to 'v'
	       Rind := flatten entries ((transpose v)*R);
	       Rind = positions(Rind, e -> e == 0);
	       convexHull(V_Vind,R_Rind | LS | -LS)))
     else emptyPolyhedron ambDim P)



-- PURPOSE : Computing the face of a Cone where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its minimum
minFace (Matrix,Cone) := (v,C) -> (
     -- Checking for input errors
     if numColumns v =!= 1 or numRows v =!= C#"ambient dimension" then error("The vector must lie in the same space as the polyhedron");
     R := rays C;
     LS := linSpace C;
     C = dualCone C;
     -- The weight must lie in the dual of the cone, otherwise there is 
     -- no minimum and the result is the empty polyhedron
     if contains(C,v) then (
	  -- Take the rays of the cone that are orthogonal to 'v'
	  Rind := flatten entries ((transpose v)*R);
	  Rind = positions(Rind, e -> e == 0);
	  posHull(R_Rind,LS))
     else emptyPolyhedron ambDim C)   



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
	  normrec := w -> if (entries w)#0#0 > 0 then 0 else if (entries w)#0#0 < 0 then 1 else (w = w^{1..(numRows w)-1}; normrec w);
          i := normrec v;
	  if i == 1 then M = {M#1,M#0};
	  M);
     -- If the polyhedron is 0 or 1 dimensional itself is its only summand
     if dim P == 0 or dim P == 1 then (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}})
     else (
	  -- Extracting the data to compute the 2 dimensional faces and the edges
	  d := P#"ambient dimension";
          dP := P#"dimension of polyhedron";
          (HS,v) := halfspaces P;
          (HP,w) := hyperplanes P;
	  F := apply(numRows HS, i -> intersection(HS,v,HP || HS^{i},w || v^{i}));
	  F = apply(F, f -> (
		    V := vertices f;
		    R := rays f;
		    (set apply(numColumns V, i -> V_{i}),set apply(numColumns R, i -> R_{i}))));
	  LS := linSpace P;
	  L := F;
	  i := 1;
	  while i < dP-2 do (
	       L = intersectionWithFacets(L,F);
	       i = i+1);
	  -- Collect the compact edges
	  L1 := select(L, l -> l#1 === set{});
	  -- if the polyhedron is 2 dimensional and not compact then every compact edge with the tailcone is a summand
	  if dim P == 2 and (not isCompact P) then (
	       L1 = intersectionWithFacets(L,F);
	       L1 = select(L, l -> l#1 === set{});
	       if #L1 == 0 or #L1 == 1 then (posHull matrix{{1}},hashTable {0 => P},matrix{{1}})
	       else (
		    TailC := rays P;
		    if linSpace P != 0 then TailC = TailC | linSpace P | -linSpace(P);
		    (posHull map(QQ^(#L1),QQ^(#L1),1),hashTable apply(#L1, i -> i => convexHull((L1#i)#0 | (L1#i)#1,TailC)),matrix toList(#L1:{1_QQ}))))
	  else (
	       -- If the polyhedron is compact and 2 dimensional then there is only one 2 faces
	       if dim P == 2 then L1 = {(set apply(numColumns vertices P, i -> (vertices P)_{i}), set {})};
	       edges := {};
	       edgesTable := edges;
	       condmatrix := map(QQ^0,QQ^0,0);
	       scan(L1, l -> (
			 -- for every 2 face we get a couple of rows in the condition matrix for the edges of this 2 face
			 -- for this the edges if set in a cyclic order must add up to 0. These conditions are added to 
			 -- 'condmatrix' by using the indices in edges
			 ledges := apply(intersectionWithFacets({l},F), e -> normvert e#0);
			 -- adding e to edges if not yet a member
			 newedges := select(ledges, e -> not member(e,edges));
			 -- extending the condmatrix by a column of zeros for the new edge
			 condmatrix = condmatrix | map(target condmatrix,QQ^(#newedges),0);
			 edges = edges | newedges;
			 -- Bring the edges into cyclic order
			 oedges := {(ledges#0,1)};
			 v := ledges#0#1;
			 ledges = drop(ledges,1);
			 nledges := #ledges;
			 oedges = oedges | apply(nledges, i -> (
				   i = position(ledges, e -> e#0 == v or e#1 == v);
				   e := ledges#i;
				   ledges = drop(ledges,{i,i});
				   if e#0 == v then (
					v = e#1;
					(e,1))
				   else (
					v = e#0;
					(e,-1))));
			 M := map(QQ^d,source condmatrix,0);
			 -- for the cyclic order in oedges add the corresponding edgedirections to condmatrix
			 scan(oedges, e -> (
				   ve := (e#0#1 - e#0#0)*(e#1);
				   j := position(edges, edge -> edge == e#0);
				   M = M_{0..j-1} | ve | M_{j+1..(numColumns M)-1}));
			 condmatrix = condmatrix || M));
	       -- if there are no conditions then the polyhedron has no compact 2 faces
	       if condmatrix == map(QQ^0,QQ^0,0) then (
		    -- collect the compact edges
		    LL := select(faces(dim P - 1,P), fLL -> isCompact fLL);
		    -- if there is only none or one compact edge then the only summand is the polyhedron itself
		    if #LL == 0 or #LL == 1 then (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}})
		    -- otherwise we get a summand for each compact edge
		    else (
			 TailCLL := rays P;
			 if linSpace P != 0 then TailCLL = TailCLL | linSpace P | -linSpace(P);
			 (posHull map(QQ^(#LL),QQ^(#LL),1),hashTable apply(#LL, i -> i => convexHull(vertices LL#i,TailCLL)),matrix toList(#LL:{1_QQ}))))
	       -- Otherwise we can compute the Minkowski summand cone
	       else (
		    Id := map(source condmatrix,source condmatrix,1);
		    C := intersection(Id,condmatrix);
		    R := rays C;
		    TC := map(ZZ^(P#"ambient dimension"),ZZ^1,0) | P#"rays" | P#"linealitySpace" | -(P#"linealitySpace");
		    v = (vertices P)_{0};
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
			      edgesearch := (v,v0,mi) -> (
				   remedges = partition(e -> member(v,e),remedges);
				   Lnew := {};
				   if remedges#?true then Lnew = apply(remedges#true, e -> (
					     j := position(edges, edge -> edge == e);
					     edir := e#0 + e#1 - 2*v;
					     vnew := v0 + (mi_(j,0))*edir;
					     (v+edir,vnew,vnew != v0)));
				   if remedges#?false then remedges = remedges#false else remedges = {};
				   L := apply(select(Lnew, e -> e#2),e -> e#1);
				   Lnew = apply(Lnew, e -> (e#0,e#1));
				   L = L | apply(Lnew, (u,w) -> if remedges =!= {} then edgesearch(u,w,mi) else {});
				   flatten L);
			      mi := R_{i};
			      v0 := map(QQ^d,QQ^1,0);
			      -- Calling the edgesearch function to get the vertices of the summand
			      L := {v0} | edgesearch(v,v0,mi);
			      L = matrix transpose apply(L, e -> flatten entries e);
			      i => convexHull(L,TC)));
		    -- computing the inclusion minimal decompositions
		     onevec := matrix toList(numRows R: {1_QQ});
		     negId := map(source R,source R,-1);
		     zerovec :=  map(source R,ZZ^1,0);
		     Q := intersection(negId,zerovec,R,onevec);
		     (C,summList,vertices(Q))))))
 

-- PURPOSE : Computes the mixed volume of n polytopes in n-space
--   INPUT : 'L'  a list of n polytopes in n-space
--  OUTPUT : the mixed volume
-- COMMENT : Note that at the moment the input is NOT checked!
mixedVolume = method()
mixedVolume List := L -> (
     n := #L;
     Elist := apply(L, P -> apply(faces(dim P -1,P),vertices));
     liftings := apply(n, i -> map(ZZ^n,ZZ^n,1)||matrix{apply(n, j -> random 25)});
     Qlist := apply(n, i -> affineImage(liftings#i,L#i));
     local Qsum;
     Qsums := apply(n, i -> if i == 0 then Qsum = Qlist#0 else Qsum = Qsum + Qlist#i);
     mV := 0;
     Elist = apply(n, i -> apply(Elist#i, e -> (e,(liftings#i)*e)));
     E1 := Elist#0;
     Elist = drop(Elist,1);
     center := matrix{{1/2},{1/2}};
     edgeTuple := {};
     k := 0;
     selectRecursion := (E1,edgeTuple,Elist,mV,Qsums,Qlist,k) -> (
	  for e1 in E1 do (
	       Elocal := Elist;
	       if Elocal == {} then mV = mV + (volume sum apply(edgeTuple|{e1}, et -> convexHull first et))
	       else (
		    Elocal = for i from 0 to #Elocal-1 list (
			 HP := halfspaces(Qsums#k + Qlist#(k+i+1));
			 HP = for j from 0 to numRows(HP#0)-1 list if (HP#0)_(j,n) < 0 then ((HP#0)^{j},(HP#1)^{j}) else continue;
			 returnE := select(Elocal#i, e -> (
				   p := (sum apply(edgeTuple|{e1}, et -> et#1 * center)) + (e#1 * center);
				   any(HP, pair -> (pair#0)*p - pair#1 == 0)));
			 --if returnE == {} then break{};
			 returnE);
		    mV = selectRecursion(Elocal#0,edgeTuple|{e1},drop(Elocal,1),mV,Qsums,Qlist,k+1)));
	  mV);
     selectRecursion(E1,edgeTuple,Elist,mV,Qsums,Qlist,k))
 
 
objectiveVector = method()
objectiveVector (Polyhedron,Polyhedron) := (P,Q) -> (
     -- Checking for input errors
     if not isFace(Q,P) then error("The second polyhedron must be a face of the first one");
     (HS,w) := halfspaces P;
     V := vertices Q;
     R := rays Q;
     V = apply(numColumns V, i -> V_{i});
     v := select(toList (0..(numRows HS)-1), i -> all(V, v -> HS^{i} * v - w^{i} == 0) and HS^{i} * R == 0);
     sum apply(v, i -> transpose HS^{i}))



-- PURPOSE : Returning a polytope of which the fan is the normal if the fan is polytopal
--   INPUT : 'F',  a Fan
--  OUTPUT : A Polytope of which 'F' is the normal fan
polytope = method(TypicalValue => Polyhedron)
polytope Fan := F -> (
     if not F.cache.?isPolytopal then isPolytopal F;
     if not F.cache.isPolytopal then error("The fan must be polytopal");
     F.cache.polytope)



-- PURPOSE : Computing the closest point of a polyhedron to a given point
--   INPUT : (p,P),  where 'p' is a point given by a one column matrix over ZZ or QQ and
--                   'P' is a Polyhedron
--  OUTPUT : the point in 'P' with the minimal euclidian distance to 'p'
proximum = method(TypicalValue => Matrix)
proximum (Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= P#"ambient dimension" then error("The point must lie in the same space");
     if isEmpty P then error("The polyhedron must not be empty");
     -- Defining local variables
     local Flist;
     d := ambDim P;
     c := 0;
     prox := {};
     -- Checking if 'p' is contained in 'P'
     if contains(P,p) then p
     else (
	  V := vertices P;
	  R := promote(rays P,QQ);
	  -- Distinguish between full dimensional polyhedra and not full dimensional ones
	  if dim P == d then (
	       -- Continue as long as the proximum has not been found
	       while instance(prox,List) do (
		    -- Take the faces of next lower dimension of P
		    c = c+1;
		    if c == dim P then (
			 Vdist := apply(numColumns V, j -> ((transpose(V_{j}-p))*(V_{j}-p))_(0,0));
			 pos := min Vdist;
			 pos = position(Vdist, j -> j == pos);
			 prox = V_{pos})
		    else (
			 Flist = faces(c,P);
			 -- Search through the faces
			 any(Flist, F -> (
				   -- Take the inward pointing normal cone with respect to P
				   (vL,bL) := hyperplanes F;
				   -- Check for each ray if it is pointing inward
				   vL = matrix apply(numRows vL, i -> (
					     v := vL^{i};
					     b := first flatten entries bL^{i};
					     if all(flatten entries (v*(V | R)), e -> e >= b) then flatten entries v
					     else flatten entries(-v)));
				   -- Take the polyhedron spanned by the inward pointing normal cone 
				   -- and 'p' and intersect it with the face
				   Q := intersection(F,convexHull(p,transpose vL));
				   -- If this intersection is not empty, it contains exactly one point, 
				   -- the proximum
				   if not isEmpty Q then (
					prox = vertices Q;
					true)
				   else false))));
	       prox)
	  else (
	       -- For not full dimensional polyhedra the hyperplanes of 'P' have to be considered also
	       while instance(prox,List) do (
		    if c == dim P then (
			 Vdist1 := apply(numColumns V, j -> ((transpose(V_{j}-p))*(V_{j}-p))_(0,0));
			 pos1 := min Vdist1;
			 pos1 = position(Vdist1, j -> j == pos1);
			 prox = V_{pos1})
		    else (
			 Flist = faces(c,P);
			 -- Search through the faces
			 any(Flist, F -> (
				   -- Take the inward pointing normal cone with respect to P
				   (vL,bL) := hyperplanes F;
				   vL = matrix apply(numRows vL, i -> (
					     v := vL^{i};
					     b := first flatten entries bL^{i};
					     entryList := flatten entries (v*(V | R));
					     -- the first two ifs find the vectors not in the hyperspace
					     -- of 'P'
					     if any(entryList, e -> e > b) then flatten entries v
					     else if any(entryList, e -> e < b) then flatten entries(-v)
					     -- If it is an original hyperplane than take the direction from 
					     -- 'p' to the polyhedron
					     else (
						  bCheck := first flatten entries (v*p);
						  if bCheck < b then flatten entries v
						  else flatten entries(-v))));
				   Q := intersection(F,convexHull(p,transpose vL));
				   if not isEmpty Q then (
					prox = vertices Q;
					true)
				   else false)));
		    c = c+1);
	       prox)))


--   INPUT : (p,C),  where 'p' is a point given by a one column matrix over ZZ or QQ and
--                   'C' is a Cone
--  OUTPUT : the point in 'C' with the minimal euclidian distance to 'p'
proximum (Matrix,Cone) := (p,C) -> proximum(p,coneToPolyhedron C)


-- PURPOSE : Computing the 'n'-skeleton of a fan
--   INPUT : (n,F),  where 'n' is a positive integer and
--                   'F' is a Fan
--  OUTPUT : the Fan consisting of the 'n' dimensional cones in 'F'
skeleton = method(TypicalValue => Fan)
skeleton(ZZ,Fan) := (n,F) -> (
     -- Checking for input errors
     if n < 0 or dim F < n then error("The integer must be between 0 and dim F");
     fan cones(n,F))

skeleton(ZZ,PolyhedralComplex) := (n,PC) -> (
     -- Checking for input errors
     if n < 0 or dim PC < n then error("The integer must be between 0 and dim F");
     GP := polyhedra(n,PC);
     verticesList := unique flatten apply(GP, P -> (Vm := vertices P; apply(numColumns Vm, i -> Vm_{i})));
     new PolyhedralComplex from {
	       "generatingPolyhedra" => set GP,
	       "ambient dimension" => ambDim PC,
	       "top dimension of the polyhedra" => n,
	       "number of generating polyhedra" => #GP,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => true,
	       symbol cache => new CacheTable});
     


-- PURPOSE : Computing the smallest face of 'P' containing 'p'
--   INPUT : '(p,P)',  where 'p' is a point given as a matrix and
--     	    	       'P' is a polyhedron
--  OUTPUT : The smallest face containing 'p' as a polyhedron
smallestFace = method()
smallestFace(Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= P#"ambient dimension" then error("The point must lie in the same space");
     p = chkZZQQ(p,"point");
     -- Checking if 'P' contains 'p' at all
     if contains(P,convexHull p) then (
	  (M,v) := halfspaces P;
     	  (N,w) := hyperplanes P;
     	  -- Selecting the half-spaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  v = promote(v,QQ);
	  pos := select(toList(0..(numRows M)-1), i -> (M^{i})*p == v^{i});
	  N = N || M^pos;
	  w = w || lift(v^pos,ZZ);
	  intersection(M,v,N,w))
     else emptyPolyhedron P#"ambient dimension")



--   INPUT : '(p,C)',  where 'p' is point given as a matrix and
--     	    	       'C' is a Cone
--  OUTPUT : The smallest face containing 'p' as a cone
smallestFace(Matrix,Cone) := (p,C) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= C#"ambient dimension" then error("The point must lie in the same space");
     p = chkZZQQ(p,"point");
     -- Checking if 'C' contains 'p' at all
     if contains(C,posHull p) then (
	  M := halfspaces C;
     	  N := hyperplanes C;
     	  -- Selecting the half-spaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  pos := select(toList(0..(numRows M)-1), i -> (M^{i})*p == 0);
	  N = N || M^pos;
	  intersection(M,N))
     else emptyPolyhedron C#"ambient dimension")



-- PURPOSE : Computing the subfan of all smooth cones of the Fan
--   INPUT : 'F',  a Fan
--  OUTPUT : The Fan of smooth cones
smoothSubfan = method(TypicalValue => Fan)
smoothSubfan Fan := F -> (
     -- recursive function that adds the cones of the list 'L' to 'F' if they are smooth
     -- and calls itself with the faces of the cone if the cone is not smooth
     facerecursion := L -> flatten apply(L, C -> if isSmooth C then C else facerecursion faces(1,C));
     L := toList F#"generatingCones";
     fan facerecursion L)


-- PURPOSE : Computing the stellar subdivision
--   INPUT : '(F,r)', where 'F' is a Fan and 'r' is a ray
--  OUTPUT : A fan, which is the stellar subdivision
stellarSubdivision = method()
stellarSubdivision (Fan,Matrix) := Fan => (F,r) -> (
     -- Checking for input errors
     if numColumns r != 1 or numRows r != ambDim F then error("The ray must be given by a one column matrix in the ambient dimension of the fan");
     divider := (C,r) -> if dim C != 1 then flatten apply(faces(1,C), f -> if not contains(f,r) then posHull {f,r} else divider(f,r)) else {C};
     L := flatten apply(maxCones F, C -> if contains(C,r) then divider(C,r) else {C});
     L = sort select(L, l -> all(L, e -> not contains(e,l) or e == l));
     n := dim L#0;
     R := unique(rays F|{promote(r,QQ)});
     new Fan from {
	  "generatingCones" => set L,
	  "ambient dimension" => ambDim L#0,
	  "top dimension of the cones" => n,
	  "number of generating cones" => #L,
	  "rays" => set R,
	  "number of rays" => #R,
	  "isPure" => dim L#0 == dim last L,
	  symbol cache => new CacheTable})


-- PURPOSE : Computing the tail cone of a given Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : The Cone generated by the rays and the lineality space of 'P'
tailCone = method(TypicalValue => Cone)
tailCone Polyhedron := P -> posHull(P#"rays",P#"linealitySpace")



-- PURPOSE : Triangulating a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A list of the simplices of the triangulation. Each simplex is given by a list 
--    	     of its vertices.
--COMMENTS : The triangulation is build recursively, for each face that is not a simplex it takes 
--     	     the weighted centre of the face. for each codim 1 face of this face it either takes the 
--     	     convex hull with the centre if it is a simplex or triangulates this in the same way.
triangulate = method()
triangulate Polyhedron := P -> (
     -- Defining the recursive face triangulation
     -- This takes a polytope and computes all facets. For each facet that is not a simplex, it calls itself
     -- again to replace this facet by a triangulation of it. then it has a list of simplices triangulating 
     -- the facets. Then it computes for each of these simplices the convex hull with the weighted centre of 
     -- the input polytope. The weighted centre is the sum of the vertices divided by the number of vertices.
     -- It returns the resulting list of simplices in a list, where each simplex is given by a list of its 
     -- vertices.
     -- The function also needs the dimension of the Polyhedron 'd', the list of facets of the original 
     -- polytope, the list 'L' of triangulations computed so far and the dimension of the original Polytope.
     -- 'L' contains a hash table for each dimension of faces of the original Polytope (i.e. from 0 to 'n').
     -- If a face has been triangulated than the list of simplices is saved in the hash table of the 
     -- corresponding dimension with the weighted centre of the original face as key.
     recursiveFaceTriangulation := (P,d,originalFacets,L,n) -> (
	  -- Computing the facets of P, given as lists of their vertices
	  F := intersectionWithFacets({(set P,set{})},originalFacets);
	  F = apply(F, f -> toList(f#0));
	  d = d-1;
	  -- if the facets are at least 2 dimensional, then check if they are simplices, if not call this 
	  -- function again
	  if d > 1 then (
	       F = flatten apply(F, f -> (
			 -- Check if the face is a simplex
			 if #f != d+1 then (
			      -- Computing the weighted centre
			      p := (sum f)*(1/#f);
			      -- Taking the hash table of the corresponding dimension
			      -- Checking if the triangulation has been computed already
			      if L#d#?p then L#d#p
			      else (
				   -- if not, call this function again for 'f' and then save this in 'L'
				   (f,L) = recursiveFaceTriangulation(f,d,originalFacets,L,n);
				   L = merge(L,hashTable {d => hashTable{p => f}},(x,y) -> merge(x,y,));
				   f))
			 else {f})));
	  -- Adding the weighted centre to each face simplex
	  q := (sum P)*(1/#P);
	  P = apply(F, f -> f | {q});
	  (P,L));
     -- Checking for input errors
     if not isCompact P then error("The polytope must be compact!");
     n := dim P;
     -- Computing the facets of P as lists of their vertices
     (HS,v) := halfspaces P;
     (HP,w) := hyperplanes P;
     originalFacets := apply(numRows HS, i -> intersection(HS,v, HP || HS^{i}, w || v^{i}));
     originalFacets = apply(originalFacets, f -> (
	       V := vertices f;
	       (set apply(numColumns V, i -> V_{i}),set {})));
     -- Making a list of the vertices of P
     P = vertices P;
     P = apply(numColumns P, i -> P_{i});
     if #P == n+1 then {P} else (
	  d := n;
	  -- Initiating the list of already computed triangulations
	  L := hashTable apply(n+1, i -> i => hashTable {});
	  (P,L) = recursiveFaceTriangulation(P,d,originalFacets,L,n);
	  P))



-- PURPOSE : Computing the volume of a full dimensional polytope
--   INPUT : 'P',  a compact polyhedron
--  OUTPUT : QQ, giving the volume of the polytope
volume = method(TypicalValue => QQ)
volume Polyhedron := P -> (
     d := dim P;
     -- Checking for input errors
     if  not isCompact P then error("The polyhedron must be compact, i.e. a polytope.");
     -- If P is not full dimensional then project it down
     if d != ambDim P then (
	  A := substitute((hyperplanes P)#0,ZZ);
	  A = inverse (smithNormalForm A)#2;
	  n := ambDim P;
	  A = A^{n-d..n-1};
	  P = affineImage(A,P));
     -- Computing the triangulation of P
     P = triangulate P;
     -- Computing the volume of each simplex without the dimension factor, by 
     -- taking the absolute of the determinant of |v_1-v_0..v_d-v_0|
     P = apply(P, p -> abs det matrix transpose apply(toList(1..d), i -> flatten entries(p#i - p#0)));
     -- Summing up the volumes and dividing out the dimension factor
     (sum P)/(d!))
	       



-- PURPOSE : Computing the vertex-edge-matrix of a polyhedron
--   INPUT : 'P',  a polyhedron
--  OUTPUT : a matrix, where the columns are indexed by the edges and the rows indexed by the vertices and has 1 as entry
--           if the corresponding edge contains this vertex
vertexEdgeMatrix = method(TypicalValue => Matrix)
vertexEdgeMatrix Polyhedron := P -> (
     -- list the edges and the vertices
     eP := apply(faces(dim P -1,P),f -> (
	       f = vertices f;
	       set apply(numColumns f, i -> f_{i})));
     vp := vertices P;
     vp = apply(numColumns vp, i -> vp_{i});
     d := #vp;
     n := #eP;
     -- Generate the matrix with indeces in the first row and column and for every edge add two 1's in the corresponding column
     transpose matrix {toList(0..d)} | ( matrix {toList(1..n)} || matrix apply(vp,v -> apply(eP,e -> if e#?v then 1 else 0))))



-- PURPOSE : Computing the vertex-facet-matrix of a polyhedron
--   INPUT : 'P',  a polyhedron
--  OUTPUT : a matrix, where the columns are indexed by the facets and the rows are indexed by the vertices and has 1 as entry
--           if the corresponding facet contains this vertex
vertexFacetMatrix = method(TypicalValue => Matrix)
vertexFacetMatrix Polyhedron := P -> (
     -- list the facets and the vertices
     fP := apply(faces(1,P),f -> (
	       f = vertices f; 
	       set apply(numColumns f, i -> f_{i})));
     vp := vertices P;
     vp = apply(numColumns vp, i -> vp_{i});
     d := #vp;
     n := #fP;
     -- Generate the matrix with indeces in the first row and column and for every facet add 1's in the corresponding column
     transpose matrix {toList(0..d)} | ( matrix {toList(1..n)} || matrix apply(vp, v -> apply(fP,f -> if f#?v then 1 else 0))))




-- PURPOSE : Computing the affine hull
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : the affine hull of 'P' as a Polyhedron
affineHull = method(TypicalValue => Polyhedron)
affineHull Polyhedron := P -> (
     M := vertices P;
     R := promote(rays P,QQ);
     -- subtracting the first vertex from all other vertices
     N := (M+M_{0}*(matrix {toList(numColumns M:-1)}))_{1..(numColumns M)-1};
     convexHull(M_{0},N | -N | R | -R));


-- PURPOSE : Computing the affine image of a polyhedron
affineImage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,v)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space and 'v' is a matrix
--     	    	      	 defining a vector in the target space of 'A'
--  OUTPUT : a polyhedron, the affine image of 'P':
--                       A*P+v={A*p+v | p in P}
affineImage(Matrix,Polyhedron,Matrix) := (A,P,v) -> (
     -- Checking for input errors
     A = chkZZQQ(A,"linear map");
     v = chkZZQQ(v,"translation vector");
     if P#"ambient dimension" =!= numColumns A then error("Matrix source must be ambient space");
     if numRows A =!= numRows v then error("Vector must lie in target space of matrix");
     if numColumns v =!= 1 then error("Second argument must be a vector");
     -- Generating nr of vertices many copies of v
     v = v * (matrix {toList(P#"number of vertices":1_QQ)});
     Mv := A*(vertices P) + v;
     Mr := A*(rays P);
     if numColumns Mr == 0 then Mr = matrix toList(numRows Mv:{0_QQ});
     convexHull(Mv,Mr))


--   INPUT : '(A,P)',  where 'A' is a ZZ or QQ matrix from the ambient space of the 
--     	    	      	 polyhedron 'P' to some other target space
--  OUTPUT : A Polyhedron, the image of 'P' under 'A'
affineImage(Matrix,Polyhedron) := (A,P) -> (
     -- Generating the zero translation vector
     A = chkZZQQ(A,"map");
     v := map(target A,QQ^1,0);
     affineImage(A,P,v))


--   INPUT : '(P,v)',  where 'v' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : A Polyhedron, the translation of 'P' by 'v', i.e. {p+v | p in P} 
affineImage(Polyhedron,Matrix) := (P,v) -> (
     -- Generating the identity matrix
     A := map(QQ^(P#"ambient dimension"),QQ^(P#"ambient dimension"),1);
     affineImage(A,P,v))


--   INPUT : '(M,C,v)',  where 'M' is a ZZ or QQ matrix from the ambient space of 
--     	    	      	 the cone 'C' to some target space and 'v' is a matrix
--     	    	      	 defining a vector in that target space
--  OUTPUT : A polyhedron, the affine image of 'C':
--                       (M*C)+v={(M*c)+v | c in C}
affineImage(Matrix,Cone,Matrix) := (M,C,v) -> if v == 0 then affineImage(M,C) else affineImage(M,coneToPolyhedron C,v)


--   INPUT : '(M,C)',  where 'M' is a ZZ or QQ matrix from the 
--     	    	      	 ambient space of the cone 'C' to some target space
--  OUTPUT : A cone, the affine image of 'C':
--                       M*C={M*c | c in C}
affineImage(Matrix,Cone) := (M,C) -> posHull affineImage(M,coneToPolyhedron C)


--   INPUT : '(C,v)',  where 'C' is a cone and 'v' is a matrix
--     	    	      	 defining a vector in the ambient space of 'C'
--  OUTPUT : A polyhedron, the affine image of 'C':
--                       C+v={c+v | c in C}
affineImage(Cone,Matrix) := (C,v) -> affineImage(coneToPolyhedron C,v)


-- PURPOSE : Computing the affine preimage of a cone or polyhedron
affinePreimage = method(TypicalValue => Polyhedron)

--   INPUT : '(A,P,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the polyhedron 'P' and 'b' is a matrix
--     	    	      	 defining a vector in the ambient space of 'P'
--  OUTPUT : A polyhedron, the affine preimage of 'P':
--                       {q | (A*q)+b in P}
affinePreimage(Matrix,Polyhedron,Matrix) := (A,P,b) -> (
     -- Checking for input errors
     A = chkZZQQ(A,"linear map");
     b = chkZZQQ(b,"translation vector");
     if P#"ambient dimension" =!= numRows A then error("Matrix source must be ambient space");
     if numRows A =!= numRows b then error("Vector must lie in target space of matrix");
     if numColumns b =!= 1 then error("Second argument must be a vector");
     -- Constructing the new half-spaces and hyperplanes
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
     A = chkZZQQ(A,"map");
     affinePreimage(A,P,map(target A,QQ^1,0)))


--   INPUT : '(P,b)',  where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the polyhedron 'P'
--  OUTPUT : A Polyhedron, the negative translation of 'P' by 'b', i.e. {q | q+b in P} 
affinePreimage(Polyhedron,Matrix) := (P,b) -> affinePreimage(map(QQ^(P#"ambient dimension"),QQ^(P#"ambient dimension"),1),P,b)


--   INPUT : '(A,C,b)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C' and 'b' is a matrix
--     	    	      	 defining a vector in the ambient space of 'C'
--  OUTPUT : A polyhedron, the affine preimage of 'C':
--                       {q | (A*q)+b in C}
--     	     or a cone, the affine preimage of 'C' if 'b' is 0:
--     	    	         {q | (A*q) in C}
affinePreimage(Matrix,Cone,Matrix) := (A,C,b) -> if b == 0 then affinePreimage(A,C) else affinePreimage(A,coneToPolyhedron C,b)


--   INPUT : '(A,C)',  where 'A' is a ZZ or QQ matrix from some source space to the 
--     	    	      	 ambient space of the cone 'C'
--  OUTPUT : A cone, the affine preimage of 'C':
--                       {q | (A*q) in C}
affinePreimage(Matrix,Cone) := (A,C) -> posHull affinePreimage(A,coneToPolyhedron C)


--   INPUT : '(C,b)',   where 'b' is a ZZ or QQ one-column matrix describing a point in
--                     the ambient space of the cone 'C'
--  OUTPUT : A polyhedron, the affine preimage of 'C':
--                       {q | q+b in C}
affinePreimage(Cone,Matrix) := (C,b) -> affinePreimage(coneToPolyhedron C,b)


-- PURPOSE : Computing the bipyramid over the polyhedron 'P'
--   INPUT : 'P',  a polyhedron 
--  OUTPUT : A polyhedron, the convex hull of 'P', embedded into ambientdim+1 space and the 
--     	         points (barycenter of 'P',+-1)
bipyramid = method(TypicalValue => Polyhedron)
bipyramid Polyhedron := P -> (
     -- Saving the vertices
     V := vertices P;
     n := numColumns V;
     if n == 0 then error("P must not be empty");
     -- Computing the barycenter of P
     v := matrix toList(n:{1_QQ,1_QQ});
     v = (1/n)*V*v;
     (M,LS) := P#"homogenizedVertices";
     -- Embedding into n+1 space and adding the two new vertices
     zerorow := map(ZZ^1,source M,0);
     newvertices := makePrimitiveMatrix(matrix {{1_QQ,1_QQ}} || v || matrix {{1_QQ,-(1_QQ)}});
     M = (M || zerorow) | newvertices;
     LS = LS || map(ZZ^1,source LS,0);
     hyperA := fourierMotzkin(M,LS);
     --verticesA := fourierMotzkin hyperA;
     local verticesA;
     (verticesA,hyperA) = fMReplacement(M,hyperA#0,hyperA#1);
     polyhedronBuilder(hyperA,verticesA))


-- PURPOSE : Computes the coarsest common refinement of a given set of rays
--   INPUT : 'M'  a Matrix
--  OUTPUT : 'F'  a Fan, the coarsest common refinement of the rays in 'M'
ccRefinement = method(TypicalValue => Fan)
ccRefinement Matrix := M -> (
     -- Checking for input errors
     M = chkZZQQ(M,"rays");
     -- Extracting data
     n := numRows M;
     m := numColumns M;
     -- Generating all cones generated by 'n' rays in 'M'
     nCones := apply(subsets(m,n), e -> posHull M_e);
     -- Selecting those cones that are 'n' dimensional and do not contain any 
     -- of the others
     nConesfd := select(nCones, C -> dim C == n);
     nConesfd = inclMinCones nConesfd;
     refCones := {};
     while nConesfd != {} do (
	  newCones := {};
	  -- scan through the 'n' dimensional cones and check for each of the cones generated by
	  -- 'n' rays if their intersection is 'n' dimensional and if the first one is not contained 
	  -- in the latter. If true, then their intersection will be saved in the list 'newCones'.
	  -- If false for every cone generated by 'n' rays, then the 'n' dimensional cone will be 
	  -- appended to the list 'refCones'
	  refCones = refCones | (flatten apply(nConesfd, C1 -> (
			 toBeAdded := flatten apply(nCones, C2 -> (
				   C := intersection(C2,C1);
				   if dim C == n and (not contains(C2,C1)) then C
				   else {}));
			 if toBeAdded == {} then C1
			 else (
			      newCones = newCones | toBeAdded;
			      {}))));
	  -- now, the new intersections will be the 'n' dimensional cones and the same procedure 
	  -- starts over again if this list is not empty
	  nConesfd = unique newCones);
     -- Compute the fan generated by the 'refCones'
     fan refCones);


-- PURPOSE : Converts the Cone 'C' into itself as a Polyhedron 'P'
--   INPUT : 'C'  a Cone
--  OUTPUT : 'P' the Cone saved as a polyhedron
coneToPolyhedron = method(TypicalValue => Polyhedron)
coneToPolyhedron Cone := C -> (
     M := map(QQ^(C#"ambient dimension"),QQ^1,0);
     N := rays C;
     convexHull(M,N))


-- PURPOSE : Computing the direct product of two polyhedra in the direct product of their ambient spaces
directProduct = method()

--   INPUT : '(P,Q)',  two polyhedra
--  OUTPUT : A polyhedron, the direct product
directProduct (Polyhedron,Polyhedron) := (P,Q) -> (
     -- Extracting half-spaces and hyperplanes of P and Q
     (Mp,vp) := halfspaces P;
     (Np,wp) := hyperplanes P;
     (Mq,vq) := halfspaces Q;
     (Nq,wq) := hyperplanes Q;
     -- Constructing the new half-spaces matrix |Mp 0 | and vector |vp|
     --                                        |0  Mq|            |vq|
     M := Mp ++ Mq;
     v := vp || vq;
     -- Constructing the new hyperplanes matrix |Np 0 | and vector |wp|
     --                                         |0  Nq|            |wq|
     N := Np ++ Nq;
     w := wp || wq;
     intersection(M,v,N,w))


--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : A cone, the direct product
directProduct (Cone,Cone) := (C1,C2) -> (
     -- Extracting half-spaces and hyperplanes of P and Q
     Mp := halfspaces C1;
     Np := hyperplanes C1;
     Mq := halfspaces C2;
     Nq := hyperplanes C2;
     -- Constructing the new half-spaces matrix |Mp 0 |
     --                                        |0  Mq|
     M := Mp ++Mq;
     -- Constructing the new hyperplanes matrix |Np 0 |
     --                                         |0  Nq|
     N := Np ++ Nq;
     intersection(M,N))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : A polyhedron, the direct product
directProduct (Cone,Polyhedron) := (C,P) -> directProduct(coneToPolyhedron C,P)


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : A polyhedron, the direct product
directProduct (Polyhedron,Cone) := (P,C) -> directProduct(P,coneToPolyhedron C)


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : A fan, the direct product
directProduct (Fan,Fan) := (F1,F2) -> (
     -- computing the direct products of all pairs of generating cones
     fan flatten apply(toList F1#"generatingCones", C1 -> apply(toList F2#"generatingCones", C2 -> directProduct(C1,C2))))


Polyhedron * Polyhedron := directProduct
Polyhedron * Cone := directProduct
Cone * Polyhedron := directProduct
Cone * Cone := directProduct
Fan * Fan := directProduct


dualCayley = method(TypicalValue => Polyhedron)
dualCayley Polyhedron := P -> (
     V := vertices P;
     (M,N) := fourierMotzkin V;
     M = sort(map(QQ^1,source M,(i,j) -> 1)|| -M);
     R := map(target M,QQ^0,0);
     HS := map(QQ^1,source V,0) || -V;
     (hyperA,verticesA) := fMReplacement(HS,M,R);
     polyhedronBuilder(hyperA,verticesA)) 


dualCayleyFace = method(TypicalValue => Polyhedron)
dualCayleyFace Polyhedron := (cacheValue symbol dualCayleyFace)(P -> (
	  local Pd;
	  --local faceOf;
	  if P.cache.?faceOf then (
	       V := transpose vertices P;
	       R := transpose rays P;
	       P0 := P.cache.faceOf;
	       P0d := dualCayley P0;
	       codimensionPd := dim P - P0#"dimension of lineality space" + 1;
	       L := faces(codimensionPd,P0d);
	       Pd = first select(1,L, l -> (V || R)*(vertices l | rays l) == 0);
	       Pd.cache.dualCayleyFace = P;
	       Pd)
	  else (
	       Pdual := dualCayley P;
	       Pd = first faces(dim P + 1,P);
	       Pd.cache.dualCayleyFace = P;
	       Pd))) 
     

-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
	genrays := (sort transpose C#"halfspaces",sort transpose C#"hyperplanes");
	dualgens := (sort (-(C#"rays")),sort C#"linealitySpace");
	coneBuilder(genrays,dualgens))
   

-- PURPOSE : Computing the face fan of a polytope
--   INPUT : 'P',  a Polyhedron, containing the origin in its interior
--  OUTPUT : The Fan generated by the cones over all facets of the polyhedron
faceFan = method(TypicalValue => Fan)
faceFan Polyhedron := P -> (
     -- Checking for input errors
     if not inInterior(map(QQ^(ambDim P),QQ^1,0),P) then  error("The origin must be an interior point.");
     F := fan apply(faces(1,P), posHull);
     F.cache.isPolytopal = true;
     F.cache.polytope = polar P;
     F)
   
   
-- PURPOSE : Computing the image fan of a cone
--   INPUT : '(M,C)',  a Matrix 'M' and a Cone 'C'
--  OUTPUT : A Fan, the common refinement of the images of all faces of
--     	     'C' under 'M'
imageFan = method(TypicalValue => Fan)
imageFan (Matrix,Cone) := (M,C) -> (
     M = chkZZQQ(M,"map");
     if numColumns M != ambDim C then error("The source space of the matrix must be the ambient space of the cone");
     -- Extracting data
     m := numRows M;
     n := dim C;
     -- Compute the images of all 'm' dimensional faces and select those that are again 
     -- 'm' dimensional
     L := apply(faces(n-m,C), e -> affineImage(M,e));
     L = select(L, e -> dim e == m);
     -- Compute their common refinement
     refineCones L)


-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambient space
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     -- Checking for input errors
     if P1#"ambient dimension" =!= P2#"ambient dimension" then error("Polyhedra must lie in the same space");
     if isEmpty P1 or isEmpty P2 then emptyPolyhedron ambDim P1 else if P1 == P2 then 2 * P1 else if ambDim P1 <= 3 then oldMinkSum(P1,P2) else newMinkSum(P1,P2))


oldMinkSum = (P1,P2) -> (
     -- Saving the vertices and rays
     V1 := vertices P1;
     V2 := vertices P2;
     R := promote(rays P1 | rays P2,QQ) | map(target V1,QQ^1,0);
     Vnew := map(target V1,QQ^0,0);
     -- Collecting all sums of vertices of P1 with vertices of P2
     Vnew = matrix {unique flatten apply(numColumns V1, i -> apply(numColumns V2, j -> V1_{i}+V2_{j}))};
     convexHull(Vnew,R))


newMinkSum = (P,Q) -> (
     facePairBuilder := (k,P) -> (
	  L := faceBuilder(k,P);
	  HS := halfspaces P;
	  HS = apply(numRows HS#0, i -> ((HS#0)^{i},(HS#1)^{i}));
	  apply(L, l -> (
		    l = (toList l#0,toList l#1);
		    (l,select(HS, hs -> all(l#0, v -> (hs#0)*v - hs#1 == 0) and all(l#1, r -> (hs#0)*r == 0))))));
     uniqueColumns := M -> (
	  if M!=0 then matrix{(unique apply(numColumns M, i -> M_{i}))} else map(ZZ^(numRows M),ZZ^0,0)
	  );
     n := ambDim P;
     HPP := hyperplanes P;
     HPQ := hyperplanes Q;
     HP := if HPP == (0,0) or HPQ == (0,0) then (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
	  k := transpose mingens ker transpose(HPP#0|| -HPQ#0);
	  if k == 0 then (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
	       dHPP := numRows HPP#0;
	       (k_{0..dHPP-1} * HPP#0,k*(HPP#1||HPQ#1))));
     d := n - numRows(HP#0);
     if d != n then (
	  if numRows HPP#0 == numRows HP#0 then HPP = (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
	       kPP := (transpose mingens ker(HP#0 * transpose HPP#0))_{0..(numRows HPP#0)-1};
	       HPP = (kPP * HPP#0,kPP * HPP#1));
	  if numRows HPQ#0 == numRows HP#0 then HPQ = (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
	       kPQ := (transpose mingens ker(HP#0 * transpose HPQ#0))_{0..(numRows HPQ#0)-1};
	       HPQ = (kPQ * HPQ#0,kPQ * HPQ#1)));
     LP := reverse apply(dim P + 1, k -> facePairBuilder(k,P));
     LP = LP | toList(max(0,d-#LP):{});
     LQ := reverse apply(dim Q + 1, k -> facePairBuilder(k,Q));
     LQ = LQ | toList(max(0,d-#LQ):{});
     HS := unique flatten apply(d, i -> (
	       if i == 0 then flatten for f in LQ#(d-1) list (
		    if f#1 == {} then (
			 entP := flatten entries((HPQ#0)*(rays P));
			 maxP := flatten entries((HPQ#0)*(vertices P));
			 if all(entP, e -> e == 0) then {(HPQ#0,matrix{{max maxP}} + HPQ#1),(-HPQ#0,-(matrix{{min maxP}} + HPQ#1))}
			 else if all(entP, e -> e <= 0) then {(HPQ#0,matrix{{max maxP}} + HPQ#1)} 
			 else if all(entP, e -> e >= 0) then {(-HPQ#0,-(matrix{{min maxP}} + HPQ#1))}
			 else continue)
		    else if all(flatten entries((f#1#0#0)*(rays P)), e -> e <= 0) then (
			 mP := max flatten entries((f#1#0#0)*(vertices P));
			 --mP = transpose makePrimitiveMatrix transpose(f#1#0#0|(f#1#0#1 + matrix{{mP}}));
			 {(f#1#0#0,f#1#0#1 + matrix{{mP}})}) else continue)
	       else if i == d-1 then flatten for f in LP#(d-1) list (
		    if f#1 == {} then (
			 entQ := flatten entries((HPP#0)*(rays Q));
			 maxQ := flatten entries((HPP#0)*(vertices Q));
			 if all(entQ, e -> e == 0) then {(HPP#0,matrix{{max maxQ}} + HPP#1),(-HPP#0,-(matrix{{min maxQ}} + HPP#1))}
			 else if all(entQ, e -> e <= 0) then {(HPP#0,matrix{{max maxQ}} + HPP#1)} 
			 else if all(entQ, e -> e >= 0) then {(-HPP#0,-(matrix{{min maxQ}} + HPP#1))}
			 else continue)
		    else if all(flatten entries((f#1#0#0)*(rays Q)), e -> e <= 0) then (
			 mQ := max flatten entries((f#1#0#0)*(vertices Q));
			 --mQ = transpose makePrimitiveMatrix transpose(f#1#0#0|(f#1#0#1 + matrix{{mQ}}));
			 {(f#1#0#0,f#1#0#1 + matrix{{mQ}})}) else continue)
	       else flatten for Pface in LP#i list (
		    for Qface in LQ#(d-i-1) list (
			 -- This fixes the descending vertex number bug. We forgot to add the common hyperplanes.
			 HPPp := hyperplanes P;
			 PfaceHS := if Pface#1 != {} then (matrix apply(Pface#1, f -> {f#0}) || HPPp#0,matrix apply(Pface#1, f -> {f#1}) || HPPp#1) else HPPp;
			 QfaceHS := if Qface#1 != {} then (matrix apply(Qface#1, f -> {f#0}) || HPQ#0,matrix apply(Qface#1, f -> {f#1}) || HPQ#1) else HPQ;
			 dP := rank PfaceHS#0;
			 dQ := rank QfaceHS#0;
			 PfaceHS = ((PfaceHS#0)^{0..dP-1},(PfaceHS#1)^{0..dP-1});
			 QfaceHS = ((QfaceHS#0)^{0..dQ-1},(QfaceHS#1)^{0..dQ-1});
			 kPQ := transpose mingens ker transpose(PfaceHS#0|| -QfaceHS#0); 
			 if numRows kPQ != 1 then continue else (
			      dPfaceHS := numRows PfaceHS#0;
			      newHS := kPQ_{0..dPfaceHS-1} * PfaceHS#0 | kPQ*(PfaceHS#1||QfaceHS#1);
			      --newHS = transpose makePrimitiveMatrix newHS;
			      newHS = (submatrix'(newHS,{n}),newHS_{n});
			      checkValueP := (newHS#0 *(Pface#0#0#0))_(0,0);
			      checkValueQ := (newHS#0 *(Qface#0#0#0))_(0,0);
			      if all(flatten entries(newHS#0 *(vertices P)), e -> e <= checkValueP) and all(flatten entries(newHS#0 *(vertices Q)), e -> e <= checkValueQ) then (
				   if all(Pface#0#1, r -> (newHS#0 *r)_(0,0) <= 0) and all(Qface#0#1, r -> (newHS*r)_(0,0) <= 0) then newHS else continue) 
			      else if all(flatten entries(newHS#0 *(vertices P)), e -> e >= checkValueP) and all(flatten entries(newHS#0 *(vertices Q)), e -> e >= checkValueQ) then (
				   if all(Pface#0#1, r -> (newHS#0 *r)_(0,0) >= 0) and all(Qface#0#1, r -> (newHS*r)_(0,0) >= 0) then (-(newHS#0),-(newHS#1)) else continue) 
			      else continue)))));
     HS = (matrix apply(HS, e -> {first e}),matrix apply(HS, e -> {last e}));
     V := matrix {unique flatten apply(numColumns vertices P, i -> apply(numColumns vertices Q, j -> (vertices P)_{i}+(vertices Q)_{j}))};
     -- Maybe the following line is needed as well:
     -- if V==0 then V = map(ZZ^(ambDim P),ZZ^1,0);
     -- This fixes the wrong ring bug.
     R := promote(rays P | rays Q,QQ) | map(target promote(V,QQ),QQ^1,0);
     V = (map(QQ^1,source promote(V,QQ),(i,j)->1) || promote(V,QQ)) | (map(QQ^1,source R,0) || R);
     HS = sort makePrimitiveMatrix transpose(-(HS#1)|HS#0);
     HS = uniqueColumns HS;
     HP = sort makePrimitiveMatrix transpose(-(HP#1)|HP#0);
     HP = uniqueColumns HP;
     polyhedronBuilder reverse fMReplacement(V,HS,HP))


--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : The Minkowskisum as a cone
minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if C1#"ambient dimension" =!= C2#"ambient dimension" then error("Cones must lie in the same space");
     -- Saving the vertices and rays
     R := C1#"rays" | C2#"rays";
     LS := C1#"linealitySpace" | C2#"linealitySpace";
     posHull(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
     -- Checking for input errors
     if C#"ambient dimension" =!= P#"ambient dimension" then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := P#"vertices";
     R := P#"rays" | C#"rays" | C#"linealitySpace" | -(C#"linealitySpace");
     convexHull(V,R))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
     -- Checking for input errors
     if C#"ambient dimension" =!= P#"ambient dimension" then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := P#"vertices";
     R := P#"rays" | C#"rays" | C#"linealitySpace" | -(C#"linealitySpace");
     convexHull(V,R))


Polyhedron + Polyhedron := minkowskiSum
Polyhedron + Cone := minkowskiSum
Cone + Polyhedron := minkowskiSum
Cone + Cone := minkowskiSum

-- PURPOSE : Scaling respectively the multiple Minkowski sum of a polyhedron
--   INPUT : '(k,P)',  where 'k' is a strictly positive rational or integer number and 
--     	    	             'P' is a Polyhedron
--  OUTPUT : The polyehdron 'P' scaled by 'k'
QQ * Polyhedron := (k,P) -> (
     -- Checking for input errors
     if k <= 0 then error("The factor must be strictly positiv");
     convexHull(k*(vertices P),rays P | linSpace P))

ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P


-- PURPOSE : Computing the normal cone of a face of a polytope
--   INPUT : '(P,Q)',  two polyhedra
--  OUTPUT : 'C',  a Cone, the inner normal cone of P in the face Q
-- COMMENT : 'Q' must be a face of P
normalCone (Polyhedron,Polyhedron) := Cone => opts -> (P,Q) -> (
     if not P.cache.?normalCone then P.cache.normalCone = new MutableHashTable;
     if not P.cache.normalCone#?Q then (
	  -- Checking for input errors
	  if not isFace(Q,P) then error("The second polyhedron must be a face of the first one");
	  p := interiorPoint Q;
	  P.cache.normalCone#Q = dualCone posHull affineImage(P,-p));
     P.cache.normalCone#Q)


-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, the inner normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
     if not P.cache.?normalFan then (
	  -- Saving the vertices
	  vm := vertices P;
	  -- For every vertex translate P by -this vertex and take the dual cone of the positive hull of it
	  L := sort apply(numColumns vm, i -> (dualCone posHull affineImage(P,-vm_{i})));
	  HS := transpose (halfspaces P)#0;
	  HS = apply(numColumns HS, i -> -HS_{i});
	  F := new Fan from {
	       "generatingCones" => set L,
	       "ambient dimension" => ambDim P,
	       "top dimension of the cones" => dim L#0,
	       "number of generating cones" => #L,
	       "rays" => set HS,
	       "number of rays" => #HS,
	       "isPure" => true,
	       symbol cache => new CacheTable};
	  F.cache.isPolytopal = true;
	  F.cache.polytope = P;
	  P.cache.normalFan = F);
     P.cache.normalFan)


-- PURPOSE : Computing the polar of a given polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A Polyhedron, the set { v | v*p<=1 forall p in P}
polar = method(TypicalValue => Polyhedron)
polar Polyhedron := (cacheValue symbol polar)(P -> (
     d := P#"ambient dimension";
     -- Make the 'd'-dimensional identity
     M := map(ZZ^d,ZZ^d,-1);
     -- make the block matrix of -1 and the 'd'identity
     M = (matrix{{-1_ZZ}} | map(ZZ^1,ZZ^d,0))||(map(ZZ^d,ZZ^1,0) | M);
     hyperA := P#"homogenizedVertices";
     hyperA = (sort (M*(hyperA#0)),hyperA#1);
     verticesA := fourierMotzkin hyperA;
     (hyperA,verticesA) = fMReplacement(hyperA#0,verticesA#0,verticesA#1);
     Q := polyhedronBuilder(hyperA,verticesA);
     Q.cache.polar = P;
     Q))


-- PURPOSE : Compute the corresponding face of the polar polytope
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A Polyhedron, if 'P' is the face of some polyhedron 'Q' then the
--     	     result is the dual face on the polar of 'Q'. If 'P' is not a face
--           then it is considered as the face of itself and thus the 
--           polarFace is the empty Polyhedron
polarFace = method(TypicalValue => Polyhedron)
polarFace Polyhedron := (cacheValue symbol polarFace)(P -> (
	  local Pd;
	  --local faceOf;
	  if P.cache.?faceOf then (
	       V := transpose vertices P;
	       R := transpose rays P;
	       P0 := P.cache.faceOf;
	       P0d := polar P0;
	       codimensionPd := dim P - P0#"dimension of lineality space" + 1;
	       L := faces(codimensionPd,P0d);
	       Pd = first select(1,L, l -> all(flatten entries(V*(vertices l)),e -> e == -1) and V*(rays l) == 0 and R*(vertices l | rays l) == 0);
	       Pd.cache.polarFace = P;
	       Pd)
	  else (
	       Pdual := polar P;
	       Pd = first faces(dim P + 1,P);
	       Pd.cache.polarFace = P;
	       Pd)))	       
	       


-- PURPOSE : Computing the pyramid over the polyhedron 'P'
--   INPUT : 'P',  a polyhedron 
--  OUTPUT : A polyhedron, the convex hull of 'P', embedded into ambientdim+1 space, and the 
--     	         point (0,...,0,1)
pyramid = method(TypicalValue => Polyhedron)
pyramid Polyhedron := P -> (
     (M,LS) := P#"homogenizedVertices";
     -- Embedding into n+1 space and adding the new vertex
     zerorow := map(ZZ^1,source M,0);
     newvertex := 1 || map(ZZ^((numRows M)-1),ZZ^1,0) || 1;
     M = (M || zerorow) | newvertex;
     LS = LS || map(ZZ^1,source LS,0);
     hyperA := fourierMotzkin(M,LS);
     --verticesA := fourierMotzkin hyperA;
     local verticesA;
     (verticesA,hyperA) = fMReplacement(M,hyperA#0,hyperA#1);
     polyhedronBuilder(hyperA,verticesA))


-- PURPOSE : Computing the sublattice basis for a given matrix of lattice points or for the lattice points
--     	     of a given polytope
sublatticeBasis = method(TypicalValue => Matrix)

--   INPUT : 'M',  a Matrix
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points in 'M'
sublatticeBasis Matrix := M -> (
     -- Checking for input errors
     M = chkZZQQ(M,"lattice points");
     M = if promote(substitute(M,ZZ),QQ) == M then substitute(M,ZZ) else error("The matrix must contain only lattice points.");
     -- The sublattice is isomorphic to source mod kernel, i.e. A/K
     A := source M; 
     K := ker M;
     -- Taking minimal generators and applying M gives a basis in target M
     M*(mingens (A/K)))


--   INPUT : 'P',  a polyhedron,
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points of 'P'
sublatticeBasis Polyhedron := P -> (
     L := latticePoints P;
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);
     sublatticeBasis(matrix {L}))
   
   
-- PURPOSE : Calculating the preimage of a polytope in the sublattice generated by its lattice points
--   INPUT : 'P',  a polyhedron
--  OUTPUT : A polyhedron, the projected polyhedron, which is now normal
toSublattice = method()
toSublattice Polyhedron := P -> (
     L := latticePoints P;
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     b := L#0;
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);     
     affinePreimage(sublatticeBasis matrix {L},P,b))


-- PURPOSE : Generating the 'd'-dimensional crosspolytope with edge length 2*'s'
crossPolytope = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and 's' is
--     	    	       a strictly positive rational number, the distance of the vertices to the origin
--  OUTPUT : The 'd'-dimensional crosspolytope with vertex-origin distance 's'
crossPolytope(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if d < 1 then error("dimension must at least be 1");
     if s <= 0 then error("size of the crosspolytope must be positive");
     constructMatrix := (d,v) -> (
	  if d != 0 then flatten {constructMatrix(d-1,v|{-1}),constructMatrix(d-1,v|{1})}
	  else {v});
     homHalf := ( sort makePrimitiveMatrix transpose( matrix toList(2^d:{-s}) | promote(matrix constructMatrix(d,{}),QQ)),map(ZZ^(d+1),ZZ^0,0));
     homVert := (sort makePrimitiveMatrix (matrix {toList(2*d:1_QQ)} || (map(QQ^d,QQ^d,s) | map(QQ^d,QQ^d,-s))),map(ZZ^(d+1),ZZ^0,0));
     polyhedronBuilder(homHalf,homVert))


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and 's' is a
--     	    	        strictly positive integer, the distance of the vertices to the origin
crossPolytope(ZZ,ZZ) := (d,s) -> crossPolytope(d,promote(s,QQ))


--   INPUT :  'd',  where 'd' is a strictly positive integer, the dimension of the polytope
crossPolytope ZZ := d -> crossPolytope(d,1_QQ)


-- PURPOSE : Computing the cyclic polytope of n points in QQ^d
--   INPUT : '(d,n)',  two positive integers
--  OUTPUT : A polyhedron, the convex hull of 'n' points on the moment curve in 'd' space 
-- COMMENT : The moment curve is defined by t -> (t,t^2,...,t^d) in QQ^d, if we say we take 'n' points 
--            on the moment curve, we mean the images of 0,...,n-1
cyclicPolytope = method(TypicalValue => Polyhedron)
cyclicPolytope(ZZ,ZZ) := (d,n) -> (
     -- Checking for input errors
     if d < 1 then error("The dimension must be positive");
     if n < 1 then error("There must be a positive number of points");
     convexHull map(ZZ^d, ZZ^n, (i,j) -> j^(i+1)))

-- PURPOSE : Computing the cell decomposition of a compact polyhedron given by a weight vector on the lattice points
--   INPUT : '(P,w)',  where 'P' is a compact polyhedron and 'w' is a one row matrix with with lattice points of 'P' 
--     	    	       many entries
--  OUTPUT : A list of polyhedra that are the corresponding cell decomposition
cellDecompose = method(TypicalValue => List)
cellDecompose (Polyhedron,Matrix) := (P,w) -> (
     n := dim P;
     LP := latticePoints P;
     -- Checking for input errors
     if numColumns w != #LP or numRows w != 1 then error("The weight must be a one row matrix with number of lattice points many entries");
     LP = matrix{LP}||w;
     P = convexHull(LP,matrix (toList(dim P:{0})|{{1}}));
     A := map(QQ^n,QQ^n,1) | map(QQ^n,QQ^1,0);
     flatten apply(faces(1,P), f -> if isCompact f then affineImage(A,f) else {}))


-- PURPOSE : Computing the Ehrhart polynomial of a polytope
--   INPUT : 'P',  a polyhedron which must be compact, i.e. a polytope
--  OUTPUT : A polynomial in QQ[x], the Ehrhart polynomial
-- COMMENT : Compactness is checked within latticePoints
ehrhart = method(TypicalValue => RingElement)
ehrhart Polyhedron := P -> (
	n := dim P;
	v := matrix apply(n,k -> {-1+#latticePoints( (k+1)*P)});
	M := promote(matrix apply(n,i -> reverse apply(n, j -> (i+1)^(j+1))),QQ);
	M = flatten entries ((inverse M)*v);
	R := QQ[getSymbol "x"];
	x := R_"x";
	1+sum apply(n,i -> M#i * x^(n-i)))


-- PURPOSE : Generating the empty polyhedron in n space
--   INPUT : 'n',  a strictly positive integer
--  OUTPUT : The empty polyhedron in 'n'-space
emptyPolyhedron = method(TypicalValue => Polyhedron)
emptyPolyhedron ZZ := n -> (
     -- Checking for input errors
     if n < 1 then error("The ambient dimension must be positive");
     verticesA := 2:map(ZZ^(n+1),ZZ^0,0);
     hyperA := (map(ZZ^(n+1),ZZ^0,0),map(ZZ^(n+1),ZZ^(n+1),1));
     polyhedronBuilder(hyperA,verticesA));



-- PURPOSE : Computing the cone of the Hirzebruch surface H_r
--   INPUT : 'r'  a positive integer
--  OUTPUT : The Hirzebruch surface H_r
hirzebruch = method(TypicalValue => Fan)
hirzebruch ZZ := r -> (
     -- Checking for input errors
     if r < 0 then error ("Input must be a positive integer");
     L := {((matrix{{0,-1},{1,r}},map(ZZ^2,ZZ^0,0)),(matrix{{1,-r},{0,-1}},map(ZZ^2,ZZ^0,0))),
	   ((matrix{{0,-1},{-1,r}},map(ZZ^2,ZZ^0,0)),(matrix{{1,r},{0,1}},map(ZZ^2,ZZ^0,0))),
	   ((matrix{{1,0},{0,1}},map(ZZ^2,ZZ^0,0)),(matrix{{-1,0},{0,-1}},map(ZZ^2,ZZ^0,0))),
	   ((matrix{{1,0},{0,-1}},map(ZZ^2,ZZ^0,0)),(matrix{{-1,0},{0,1}},map(ZZ^2,ZZ^0,0)))};
     L = apply(L,coneBuilder);
     F := new Fan from {
	  "generatingCones" => set L,
	  "ambient dimension" => 2,
	  "top dimension of the cones" => 2,
	  "number of generating cones" => 4,
	  "rays" => set {matrix{{0}, {-1}},matrix{{1}, {0}},matrix{{-1}, {r}},matrix{{0}, {1}}},
	  "number of rays" => 4,
	  "isPure" => true,
	  symbol cache => new CacheTable};
     F.cache.isComplete = true;
     F.cache.isPointed = true;
     F.cache.isPolytopal = true;
     F.cache.isSmooth = true;
     F.cache.polytope = polyhedronBuilder((map(ZZ^3,ZZ^4,{{0, -1, 0, -1}, {-1, 1, 0, 0}, {0, -r, -1, 1}}),map(ZZ^3,0,0)),
			 (map(ZZ^3,ZZ^4,{{1, 1, 1, 1}, {0, 1, 0, 1+r}, {0, 0, 1, 1}}),map(ZZ^3,0,0)));
     F)



-- PURPOSE : Generating the 'd'-dimensional hypercube with edge length 2*'s'
hypercube = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and
--     	    	       's' is a positive rational number, half of the edge length
--  OUTPUT : The 'd'-dimensional hypercube with edge length 2*'s' as a polyhedron
hypercube(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if d < 1 then error("dimension must at least be 1");
     if s <= 0 then error("size of the hypercube must be positive");
     -- Generating half-spaces matrix and vector
     intersection(map(QQ^d,QQ^d,1) || -map(QQ^d,QQ^d,1),matrix toList(2*d:{s})))


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and
--     	    	       's' is a positive integer, half of the edge length
hypercube(ZZ,ZZ) := (d,s) -> hypercube(d,promote(s,QQ))

     
--   INPUT : 'd',  is a strictly positive integer, the dimension of the polytope 
hypercube ZZ := d -> hypercube(d,1_QQ)


-- PURPOSE : Computing the Newton polytope for a given polynomial
--   INPUT : 'p',  a RingElement
--  OUTPUT : The polyhedron that has the exponent vectors of the monomials of 'p' as vertices
newtonPolytope = method(TypicalValue => Polyhedron)
newtonPolytope RingElement := p -> convexHull transpose matrix exponents p


-- PURPOSE : Generating the positive orthant in n-space as a cone
--   INPUT : 'n",  a strictly positive integer
--  OUTPUT : The cone that is the positive orthant in n-space
posOrthant = method(TypicalValue => Cone)
posOrthant ZZ := n -> posHull map(QQ^n,QQ^n,1)


-- PURPOSE : Computing the secondary Polytope of a Polyhedron
--   INPUT : 'P',  a Polyhedron which must be compact
--  OUTPUT : a polytope, the secondary polytope
secondaryPolytope = method(TypicalValue => Polyhedron)
secondaryPolytope Polyhedron := P -> (
     -- Checking for input errors
     if not isCompact P then error("The polyhedron must be compact.");
     -- Extracting necessary data
     V := vertices P;
     n := dim P;
     m := numColumns V;
     -- Computing the cell decomposition of P induced by the projection of the m-1 simplex onto P
     nCells := apply(subsets(m,n+1), e -> convexHull V_e);
     nCellsfd := select(nCells, C -> dim C == n);
     nCellsfd = inclMinCones nCellsfd;
     refCells := {};
     while nCellsfd != {} do (
	  newCells := {};
	  -- scan through the 'n' dimensional cells and check for each of the cells generated by
	  -- 'n+1' vertices if their intersection is 'n' dimensional and if the first one is not contained 
	  -- in the latter. If true, then their intersection will be saved in the list 'newCells'.
	  -- If false for every cone generated by 'n+1' vertices, then the 'n' dimensional cell will be 
	  -- appended to the list 'refCells'
	  refCells = refCells | (flatten apply(nCellsfd, C1 -> (
			 toBeAdded := flatten apply(nCells, C2 -> (
				   C := intersection(C2,C1);
				   if dim C == n and (not contains(C2,C1)) then C
				   else {}));
			 if toBeAdded == {} then C1
			 else (
			      newCells = newCells | toBeAdded;
			      {}))));
	  -- now, the new intersections will be the 'n' dimensional cones and the same procedure 
	  -- starts over again if this list is not empty
	  nCellsfd = unique newCells);
     refCells = if n != ambDim P then (
	  A := substitute((hyperplanes P)#0,ZZ);
	  A = inverse (smithNormalForm A)#2;
	  d := ambDim P;
	  A = A^{d-n..d-1};
	  apply(refCells, P -> (volume affineImage(A,P),interiorPoint P)))
     else apply(refCells, P -> (volume P,interiorPoint P));
     volP := sum apply(refCells,first);
     Id := -map(QQ^m,QQ^m,1);
     v := map(QQ^m,QQ^1,0);
     N := matrix{toList(m:1_QQ)} || V;
     w := matrix {{1_QQ}};
     sum apply(refCells, e -> (e#0/volP) * intersection(Id,v,N,w||e#1)))
     


-- PURPOSE : Computing the state polytope of the ideal 'I'
--   INPUT : 'I',  a homogeneous ideal with resect to some strictly positive grading
--  OUTPUT : The state polytope as a polyhedron
statePolytope = method(TypicalValue => Polyhedron)
statePolytope Ideal := I -> (
     -- Check if there exists a strictly positive grading such that 'I' is homogeneous with
     -- respect to this grading
     homogeneityCheck := I -> (
	  -- Generate the matrix 'M' that spans the space of the differeneces of the 
	  -- exponent vectors of the generators of 'I'
	  L := flatten entries gens I;
	  lt := apply(L, leadTerm);
	  M := matrix flatten apply(#L, i -> apply(exponents L#i, e -> (flatten exponents lt#i)-e));
	  -- intersect the span of 'M' with the positive orthant
	  C := intersection(map(source M,source M,1),M);
	  -- Check if an interior vector is strictly positive
	  v := interiorVector C;
	  (all(flatten entries v, e -> e > 0),v));
     -- Compute the Groebner cone
     gCone := (g,lt) -> (
	  -- for a given groebner basis compute the reduced Groebner basis
	  -- note: might be obsolete, but until now (Jan2009) groebner bases appear to be not reduced
	  g = apply(flatten entries gens g, l -> ((l-leadTerm(l))% g)+leadTerm(l));
	  -- collect the differences of the exponent vectors of the groebner basis
	  lt = flatten entries lt;
	  L := matrix flatten apply(#g, i -> apply(exponents g#i, e -> (flatten exponents lt#i)-e));
	  -- intersect the differences
	  intersection L);
     wLeadTerm := (w,I) -> (
	  -- Compute the Groebner basis and their leading terms of 'I' with respect to the weight 'w'
	  R := ring I;
	  -- Resize w to a primitive vector in ZZ
	  w = flatten entries substitute((1 / abs gcd flatten entries w) * w,ZZ);
	  -- generate the new ring with weight 'w'
	  S := (coefficientRing R)[gens R, MonomialOrder => {Weights => w}, Global => false];
	  f := map(S,R);
	  -- map 'I' into 'S' and compute Groebner basis and leadterm
	  I1 := f I;
	  g := gb I1;
	  lt := leadTerm I1;
	  gbRemove I1;
	  (g,lt));
     makePositive := (w,posv) -> (
	  w = flatten entries w;
	  posv = flatten entries posv;
	  j := min(apply(#w, i -> w#i/posv#i));
	  if j <= 0 then j = 1 - floor j else j = 0;
	  matrix transpose{w + j * posv});
     -- computes the symmetric difference of the two lists
     sortIn := (L1,L2) -> ((a,b) := (set apply(L1,first),set apply(L2,first)); join(select(L1,i->not b#?(i#0)),select(L2,i->not a#?(i#0))));
     --Checking for homogeneity
     (noError,posv) := homogeneityCheck I;
     if not noError then error("The ideal must be homogeneous w.r.t. some strictly positive grading");
     -- Compute a first Groebner basis to start with
     g := gb I;
     lt := leadTerm I;
     -- Compute the Groebner cone
     C := gCone(g,lt);
     gbRemove I;
     -- Generate all facets of 'C'
     -- Save each facet by an interior vector of it, the facet itself and the cone from 
     -- which it has been computed
     facets := apply(faces(1,C), f -> (interiorVector f,f,C));
     --Save the leading terms as the first vertex
     verts := {lt};
     -- Scan the facets
     while facets != {} do (
	  local omega';
	  local f;
	  (omega',f,C) = facets#0;
	  -- compute an interior vector of the big cone 'C' and take a small 'eps'
	  omega := promote(interiorVector C,QQ);
	  eps := 1/10;
	  omega1 := omega'-(eps*omega);
	  (g,lt) = wLeadTerm(makePositive(omega1,posv),I);
	  C' := gCone(g,lt);
	  -- reduce 'eps' until the Groebner cone generated by omega'-(eps*omega) is 
	  -- adjacent to the big cone 'C'
	  while intersection(C,C') != f do (
	       eps = eps * 1/10;
	       omega1 = omega'-(eps*omega);
	       (g,lt) = wLeadTerm(makePositive(omega1,posv),I);
	       C' = gCone(g,lt));
	  C = C';
	  -- save the new leadterms as a new vertex
	  verts = append(verts,lt);
	  -- Compute the facets of the new Groebner cone and save them in the same way as before
	  newfacets := faces(1,C);
	  newfacets = apply(newfacets, f -> (interiorVector f,f,C));
	  -- Save the symmetric difference into 'facets'
	  facets = sortIn(facets,newfacets));
     posv = substitute(posv,ZZ);
     R := ring I;
     -- generate a new ring with the strictly positive grading computed by the homogeneity check
     S := QQ[gens R, Degrees => entries posv];
     -- map the vertices into the new ring 'S'
     verts = apply(verts, el -> (map(S,ring el)) el);
     -- Compute the maximal degree of the vertices
     L := flatten apply(verts, l -> flatten entries l);
     d := (max apply(flatten L, degree))#0;
     -- compute the vertices of the state polytope
     vertmatrix := transpose matrix apply(verts, v -> (
	       VI := ideal flatten entries v;
	       SI := S/VI;
	       v = flatten apply(d, i -> flatten entries basis(i+1,SI));
	       flatten sum apply(v,exponents)));
     -- Compute the state polytope
     P := convexHull vertmatrix;
     (verts,P));
	  
	  
-- PURPOSE : Generating the 'd'-dimensional standard simplex in QQ^(d+1)
--   INPUT : 'd',  a positive integer
--  OUTPUT : The 'd'-dimensional standard simplex as a polyhedron
stdSimplex = method(TypicalValue => Polyhedron)
stdSimplex ZZ := d -> (
     -- Checking for input errors
     if d < 0 then error("dimension must not be negative");
     -- Generating the standard basis
     convexHull map(QQ^(d+1),QQ^(d+1),1))


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
     F = openOut F;
     -- Make sure Polyhedra is loaded when the session is recovered
     F << "needsPackage \"Polyhedra\"" << endl;
     -- Check if PPDivisor has been loaded
     PPDivisorPackageLoaded :=  PackageDictionary#?"PPDivisor";
     if (PPDivisorPackageLoaded) then (
	  -- if so, make sure it will also be loaded when the session is recovered
	  F << "needsPackage \"PPDivisor\"" << endl);
     --Save all Matrices to the file
     scan(userSymbols Matrix, s -> F << s << " = " << toExternalString value s << endl);
     scan(userSymbols PolyhedralObject, s -> F << s << " = " << toExternalString value s << endl);
     -- Save all Lists and Sequences containing only convex polyhedral objects and/or lists of them to the file
     scan(userSymbols List | userSymbols Sequence, s -> (
	       L := value s;
	       while L =!= flatten L do L = flatten L;
	       if all(L, l -> (
			 if instance(l,Sequence) then all(l, e -> instance(l,PolyhedralObject) or instance(l,Matrix)) 
			 else instance(l,PolyhedralObject) or instance(l,Matrix))) then F << s << " = " << toExternalString value s << endl)))
     



---------------------------------------
-- DECLARING AUXILIARY FUNCTIONS
-- >> not public <<
---------------------------------------

liftable (Matrix,Number) := (f,k) -> try (lift(f,k); true) else false; 

makePrimitiveMatrix = M -> if M != 0 then lift(transpose matrix apply(entries transpose M, w -> (g := abs gcd w; apply(w, e -> e//g))),ZZ) else lift(M,ZZ);
     

fMReplacement = (R,HS,HP) -> (
     uniqueColumns := M -> matrix{(unique apply(numColumns M, i -> M_{i}))};
     n := numRows R;
     LS := mingens ker transpose(HS|HP);
     alpha := rank LS;
     if alpha > 0 then (
	  LS = lift(gens gb promote(LS,QQ[]),QQ);
	  CR := mingens ker transpose LS;
	  CR = CR * (inverse(LS|CR))^{alpha..n-1};
	  R = CR * R);
     beta := rank HP;
     if beta > 0 then (
	  HP = lift(gens gb promote(HP,QQ[]),QQ);
	  CHS := mingens ker transpose HP;
	  CHS = CHS * (inverse(HP|CHS))^{beta..n-1};
	  HS = CHS * HS);
     HS = if HS == 0 then map(ZZ^(numRows HS),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix HS;
     R = apply(numColumns R, i -> R_{i});
     R = select(R, r -> (r != 0 and (
		    pos := positions(flatten entries((transpose HS) * r), e -> e == 0);
		    #pos >= n-alpha-beta-1 and (n <= 3 or rank HS_pos >= n-alpha-beta-1))));
     if R == {} then R = map(ZZ^(numRows LS),ZZ^0,0) else R = sort matrix {unique apply(R, makePrimitiveMatrix)};
     LS = if LS == 0 then map(ZZ^(numRows LS),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix LS;
     HP = if HP == 0 then map(ZZ^(numRows HP),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix HP;
     ((R,LS),(HS,HP)))


faceBuilder = (k,P) -> (
     --Checking for input errors
     if k < 0 or k > dim P then error("the codimension must be between 0 and the dimension of the polyhedron");
     if not P.cache.?faces then P.cache.faces = new MutableList;
     i := #(P.cache.faces);
     if k < i then P.cache.faces#k
     else (
	  d := dim P - k;
	  dl := P#"dimension of lineality space";
	  -- Saving the lineality space of 'P', which is the also the lineality space of each face
	  LS := P#"linealitySpace";
	  -- for d = dim P it is the polyhedron itself
	  if d == dim P then (
	       VP := vertices P;
	       RP := rays P;
	       P.cache.faces#k = {(set apply(numColumns VP, i -> VP_{i}),set apply(numColumns RP, i -> RP_{i}))};
	       P.cache.faces#k)
	  -- for k=dim(P) the faces are the vertices
	  else if d == dl then (
	       VP1 := vertices P;
	       -- Generating the list of vertices with each vertex as a polyhedron
	       apply(numColumns VP1, i -> (set {VP1_{i}},set {})))
	  else if d < dl then {}
	  else (
	       if i == 0 then (
		    VP2 := vertices P;
		    RP2 := rays P;
		    P.cache.faces#0 = {(set apply(numColumns VP2, i -> VP2_{i}),set apply(numColumns RP2, i -> RP2_{i}))};
		    i = 1);
	       if i == 1 then (
		    -- Saving the half-spaces and hyperplanes
		    (HS,v) := halfspaces P;
		    (HP,w) := hyperplanes P;
		    -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
		    Fl := apply(numRows HS, i -> intersection(HS,v,HP || HS^{i},w || v^{i}));
		    Fl = apply(Fl, f -> (
			      V := vertices f;
			      R := rays f;
			      (set apply(numColumns V, i -> V_{i}),set apply(numColumns R, i -> R_{i}))));
		    i = 2;
		    P.cache.faces#1 = Fl);
	       F := P.cache.faces#1;
	       i = i - 1;
	       L := P.cache.faces#i;
	       -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	       while i < k do (
		    L = intersectionWithFacets(L,F);
		    i = i+1;
		    P.cache.faces#i = L);
	       P.cache.faces#k)))


faceBuilderCone = (k,C) -> (
     d := dim C - k;
     dl := C#"dimension of lineality space";
     LS := linSpace C;
     --Checking for input errors
     if d < 0 or d > dim C then error("the codimension must be between 0 and the dimension of the cone");
     if not C.cache.?faces then C.cache.faces = new MutableList;
     i := #(C.cache.faces);
     if k < i then C.cache.faces#k
     -- for d = dim C it is the cone itself
     else if d == dim C then (
	  Rd := rays C;
	  C.cache.faces#k = {set apply(numColumns Rd, i -> Rd_{i})};
	  C.cache.faces#k)
     -- for d = dl it is the lineality space
     else if d == dl then {set {map(QQ^(ambDim C),QQ^1,0)}}
     -- for d = dl+1 it is the lineality space plus one of the rays
     else if d == dl+1 then (
	  -- Generating the list of cones given by one ray and the lineality space
	  R1 := rays C;
	  apply(numColumns R1, i -> set {R1_{i}}))
     else if 0 <= d and d < dl then {}
     else (
	  if i == 0 then (
	       R2 := rays C;
	       C.cache.faces#0 = {set apply(numColumns R2, i -> R2_{i})};
	       i = 1);
	  if i == 1 then (
	       -- Saving the half-spaces and hyperplanes
	       HS := halfspaces C;
	       HP := hyperplanes C;
	       -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
	       F1 := apply(numRows HS, i -> intersection(HS,HP || HS^{i}));
	       F1 = apply(F1, f -> (
			 R := rays f;
			 (set apply(numColumns R, i -> R_{i}))));
	       i = 2;
	       C.cache.faces#1 = F1);	       
	  -- Duplicating the list of facets
	  F := C.cache.faces#1;
	  i = i-1;
	  L := C.cache.faces#i;
	  -- Intersecting L k-1 times with F and returning the maximal inclusion sets. These are the faces of codim plus 1
	  while i < k do (
	       L = intersectionWithFacetsCone(L,F);
	       i = i+1;
	       C.cache.faces#i = L);
	  -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
	  C.cache.faces#k))


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
	HP := transpose(hyperA#1);
	HP = (HP_{1..(numgens source HP)-1},-HP_{0});
	-- Defining the Polyhedron
	new Polyhedron from {
	     "ambient dimension" => (numgens target B)-1,
	     "dimension of polyhedron" =>  ((numgens target B)-1)-(rank(hyperA#1)),
	     "dimension of lineality space" => numgens source LS,
	     "linealitySpace" => LS,
	     "number of vertices" => numgens source B,
	     "number of rays" => numgens source C,
	     "vertices" => B^{1..(numgens target B)-1},
	     "rays" => C^{1..(numgens target C)-1},
	     "number of facets" => numgens target H,
	     "halfspaces" => (H_{1..(numgens source H)-1},-H_{0}),
	     "hyperplanes" => HP,
	     "homogenizedVertices" => verticesA,
	     "homogenizedHalfspaces" => hyperA,
	     symbol cache => new CacheTable})
   
   
-- PURPOSE : Building the Cone 'C'
--   INPUT : '(genrays,dualgens)',  a pair of two matrices each describing the cone C
--                                	directly  as generating rays ('genrays') and in the 
--						dual description as intersection of half-spaces through 
--						the origin ('dualgens')
--  OUTPUT : The Cone 'C'
coneBuilder = (genrays,dualgens) -> (
      -- Sorting into rays, lineality space generators, supporting half-spaces, and hyperplanes
      RM := genrays#0;
      LS := genrays#1;
      HS := transpose(-dualgens#0);
      HP := transpose(dualgens#1);
      -- Defining C
      new Cone from {
	   "ambient dimension" => numgens target RM,
	   "dimension of the cone" => (numgens target RM)-(rank HP),
	   "dimension of lineality space" => numgens source LS,
	   "linealitySpace" => LS,
	   "number of rays" => numgens source RM,
	   "rays" => RM,
	   "number of facets" => numgens target HS,
	   "halfspaces" => HS,
	   "hyperplanes" => HP,
	   "genrays" => genrays,
	   "dualgens" => dualgens,
	   symbol cache => new CacheTable})
   
   
-- PURPOSE : check whether a matrix is over ZZ or QQ
--   INPUT : '(M,msg)', a matrix 'M' and a string 'msg'
--  OUTPUT : the matrix 'M' promoted to QQ if it was over ZZ or QQ, otherwise an error
chkZZQQ = (M,msg) -> (
     R := ring M;
     if R =!= ZZ and R =!= QQ then error("expected matrix of ",msg," to be over ZZ or QQ");
     promote(M,QQ));

-- PURPOSE : check whether a matrix is over ZZ or QQ, return it over ZZ
--   INPUT : '(M,msg)', a matrix 'M' and a string 'msg'
--  OUTPUT : the matrix 'M' cleared of denominatorx columnwise and lifted to ZZ if it was over QQ, 
--     	     itself if already over ZZ, otherwise an error
chkQQZZ = (M,msg) -> (
     R := ring M;
     if R === ZZ then M else if R === QQ then makePrimitiveMatrix M else error("expected matrix of ",msg," to be over ZZ or QQ"));


-- PURPOSE : Computing the Hilbert basis of a standardised cone (project and lift algorithm
--   INPUT : 'A' a matrix, the row echolon form of the defining half-spaces of the cone
--  OUTPUT : a list of one column matrices, the generators of the cone over A intersected with 
--     	     the positive orthant
constructHilbertBasis = A -> (
    -- Defining the function to determine if u is lower v
    lowvec := (u,v) -> (
	 n := (numRows u)-1;
	 diffvec := flatten entries(u-v);
	 if all(diffvec, i -> i <= 0) then abs(u_(n,0)) <= abs(v_(n,0)) and (u_(n,0))*(v_(n,0)) >= 0
	 else false);
    -- Collecting data
    A = substitute(A,ZZ);
    H := {A^{0}_{0}};
    s := numRows A;
    n := numColumns A;
    --doing the project and lift algorithm step by step with increasing dimensions
    scan(n-1, i -> (
	      -- the set 'F' will contain the lifted basis vectors, 'B' are the first i+2 columns of 'A' as a rowmatrix,
	      -- the set 'H' contains the vectors from the last loop that are one dimension smaller
	      F := {};
	      B := transpose A_{0..(i+1)};
	      -- Decide between lifting the existing vectors (i > s-1) or also adding the next column of 'B'
	      if i < s-1 then (
		   -- Lifting the existing vectors from 'H'
		   F = apply(H, h -> (
			     j := 0;
			     while numRows h == i+1 do (
				  if isSubset(image(h || matrix{{j}}), image B) then h = (h || matrix{{j}});
				  j = j+1);
			     h));
		   -- Adding +- times the next column of 'B'
		   F = join(F,{B_{i+1}^{0..(i+1)},-B_{i+1}^{0..(i+1)}}))
	      else (
		   -- Lifting the existing vectors from 'H'
		   nullmap := map(ZZ^1,ZZ^s,0);
		   nullvec := map(ZZ^1,ZZ^1,0);
		   F = apply(H, h -> B*substitute(vertices intersection(nullmap,nullvec,B^{0..i},h),ZZ)));
	      -- Computing the S-pairs from the elements of 'F' and saving them in 'C'
	      C := select(subsets(#F,2), j -> (
			f := F#(j#0);
			g := F#(j#1);
			(f_(i+1,0))*(g_(i+1,0)) < 0 and f+g != 0*(f+g)));
	      C = apply(C, j -> F#(j#0)+F#(j#1));
	      -- The elements of 'F' are saved in 'G'
	      G := F;
	      j := 0;
	      -- Adding those elements of 'C' to 'G' that satisfy the "normalform" condition by increasing last entry
	      while C != {} do (
		   Cnow := partition(e -> sum drop(flatten entries e,-1) == j,C);
		   C = if Cnow#?false then Cnow#false else {};
		   Cnow = if Cnow#?true then select(Cnow#true, f -> all(G, g -> not lowvec(g,f))) else {};
		   Cnew := flatten apply(Cnow, f -> apply(select(G, g -> f_(i+1,0)*g_(i+1,0) < 0 and f+g != 0*(f+g)), g -> f+g));
		   if all(Cnew, e -> sum drop(flatten entries e,-1) != j) then j = j+1;
		   C = unique (C | Cnew);
		   G = unique (G | Cnow));
	      -- saving those elements of 'G' with positive last entry into 'H'
	      H = select(G, g -> g_(i+1,0) >= 0)));
    H)



-- PURPOSE : select those cones in a list that do not contain any other cone of the list
--   INPUT : 'L',  a list of cones
--  OUTPUT : The list of cones that don't contain any of the other
inclMinCones = L -> (
     newL := {};
     -- Scanning the list
     while L != {} do (
	  C := L#0;
	  L = drop(L,1);
	  -- check, if 'C' contains any cone remaining in
	  if all(L, C1 -> not contains(C,C1)) then (
	       -- if not, then check if 'C' contains any of the cones already in the final list
	       if all(newL, C1 -> not contains(C,C1)) then (
		    -- if not again, then add 'C' to the final list.
		    newL = newL | {C})));
     newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of Sequences each containing a set of vertices and a set of rays giving the faces of a 
--     	    	   certain dimension of a polyhedron
--     	     'F', a list of Sequences each containing a set of vertices and a set of rays giving the facets 
--     	    	   of the same polyhedron
--  OUTPUT : a list of Sequences each containing a set of vertices and a set of rays giving the faces 
--     	    	   of the same polyhedron one dimension lower then the ones in 'L'
intersectionWithFacets = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e#0 =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := ((l#0)*(f#0),(l#1)*(f#1));
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			      if isValid(e,l) then (
				   if not any(newL, g -> isSubset(e#0,g#0) and isSubset(e#1,g#1)) then (
					newL = select(newL, g -> not (isSubset(g#0,e#0) and isSubset(g#1,e#1)))|{e}))))));
	  newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of sets each containing the rays of the faces of a certain dimension of a polyhedron
--     	     'F', a list of sets each containing the rays of the facets of the same polyhedron
--  OUTPUT : a list of sets each containing the rays of the faces of the same polyhedron one dimension lower 
--     	     then the ones in 'L'
intersectionWithFacetsCone = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := l*f;
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			     if isValid(e,l) then (
				  if not any(newL, g -> isSubset(e,g)) then (
					newL = select(newL, g -> not isSubset(g,e))|{e}))))));
	  newL);


-- PURPOSE : Computes the common refinement of a list of cones
--   INPUT : 'L',  a list of cones
--  OUTPUT : A fan, the common refinement of the cones
refineCones = L -> (
     -- Collecting the rays of all cones
     R := rays L#0;
     n := numRows R;
     R = apply(numColumns R, i -> R_{i});
     L1 := drop(L,1);
     R = unique flatten (R | apply(L1, C -> apply(numColumns rays C, i -> (rays C)_{i})));
     -- Writing the rays into one matrix
     M := matrix transpose apply(R, r -> flatten entries r);
     -- Compute the coarsest common refinement of these rays
     F := ccRefinement M;
     -- Collect for each cone of the ccRef the intersection of all original cones, that contain
     -- the interior of that cone
     fan apply(maxCones F, C -> (
	       v := interiorVector(C);
	       intersection select(L, c -> contains(c,v)))))


---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     	Key => OldPolyhedra,
	Headline => "for computations with convex polyhedra, cones, and fans",
	
	"A rational convex ", TO Polyhedron, " is the intersection of finitely many affine half-spaces 
	over ", TO QQ, " or equivalently, the convex hull of a finite set of vertices and rays. 
	A rational convex polyhedral ", TO Cone, " is the intersection of finitely many linear half-spaces 
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
	Texts in Mathematics 152, Springer-Verlag, New York, 1995.",
	
	PARA{}, "The author would like to thank ",HREF("http://people.cs.uchicago.edu/~nilten/", "Nathan Ilten")," 
	for contributing several functions to the package."
	
	}
   
document {
     Key => "Working with polyhedra",
     
     "We start with a polyhedron in 2-space which is the ",TO convexHull," of a given set of points.",
     
     EXAMPLE {
	  " V = matrix {{0,2,-2,0},{-1,1,1,1}}",
	  " P = convexHull V"
	  },
     
     PARA{}, "This gives an overview of the characteristics of the polyhedron. If we want to know 
     more details, we can ask for them.",
     
     EXAMPLE {
	  " vertices P"
	  },
     
     PARA{}, "Here we see that the point (0,1) is not a vertex and ",TT "P"," is actually a triangle.",
     
     EXAMPLE {
	  " (HS,v) = halfspaces P"
	  },
     
     PARA{}, "This gives the defining affine half-spaces, i.e. ",TT "P"," is given by all ",TT "p"," such 
     that ",TT "HS*p =< v"," and that lie in the defining affine hyperplanes. To get the hyperplanes we use:",
     
     EXAMPLE {
	  " hyperplanes P"
	  },
     
     PARA{}, "There are none, so the polyhedron is of full dimension. It is also compact, since ",TT "P"," has 
     no rays and the lineality space is of dimension zero.",
     
     EXAMPLE {
	  " rays P",
	  " linSpace P"
	  },
     
     PARA{}, "Furthermore, we can construct the convex hull of a set of points and a set of rays.",
     
     EXAMPLE {
	  " R = matrix {{1},{0},{0}}",
	  " V1 = V || matrix {{1,1,1,1}}",
	  " P1 = convexHull(V1,R)",
	  " vertices P1"
	  },
     
     PARA{}, "This polyhedron is not compact anymore and also not of full dimension.",
     
     EXAMPLE {
	  " rays P1",
	  " hyperplanes P1"
	  },
     
     PARA{}, "On the other hand we can construct a polyhedron as the ",TO intersection," of affine 
     half-spaces and affine hyperplanes.",
     
     EXAMPLE {
	  " HS = transpose (V || matrix {{-1,2,0,1}})",
	  " v = matrix {{1},{1},{1},{1}}",
	  " HP = matrix {{1,1,1}}",
	  " w = matrix {{3}}",
	  " P2 = intersection(HS,v,HP,w)"
	  },
     
     PARA{}, "This is a triangle in 3-space with the following vertices.",
     
     EXAMPLE {
	  " vertices P2"
	  },
     
     PARA{}, "If we don't intersect with the hyperplane we get a full dimensional polyhedron.",
     
     EXAMPLE {
	  " P3 = intersection(HS,v)",
	  " vertices P3",
	  " linSpace P3"
	  },
     
     PARA{}, "Note that the vertices are given modulo the lineality space. Besides constructing 
     polyhedra by hand, there are also some basic polyhedra implemented such as 
     the ",TO hypercube,", in this case with edge-length four.",
     
     EXAMPLE {
	  " P4 = hypercube(3,2)",
	  " vertices P4"
	  },
     
     PARA{}, "Another on is the ",TO crossPolytope,", in this case with diameter six. ",
     
     EXAMPLE {
	  " P5 = crossPolytope(3,3)",
	  " vertices P5"
	  },
     
     PARA{}, "Furthermore the standard simplex (",TO stdSimplex,").",
     
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
     of polyhedra and matrices defining vertices/rays or affine half-spaces/hyperplanes.
     All of these must be in the same ambient space. For example:",
     
     EXAMPLE {
	  " P9 = convexHull {(V1,R),P2,P6}",
	  " vertices P9"
	  },
     
     PARA{}, "Further functions are for example the Minkowski sum (",TO minkowskiSum,") of 
     two polyhedra.",
     
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
     
     PARA{}, "Here the polyhedra in the hash table ",TT "L"," are all possible Minkowski 
     summands up to scalar multiplication and the columns of ",TT "M"," give the minimal 
     decompositions. So the hexagon ",TT "P10"," is not only the sum of two triangles but also the sum 
     of three lines. Furthermore, we can take the direct product of two polyhedra.",
     
     EXAMPLE {
	  " P11 = P * Q",
	  " vertices P11"
	  },
     
     PARA{}, "The result is in QQ^4.",
     
     EXAMPLE {
	  "ambDim P11"
	  },
     
     PARA{}, "To find out more about this polyhedron use for example.",
     
     EXAMPLE {
	  " fVector P11"
	  },
     
     PARA{}, "The function ",TO fVector," gives the number of faces of each dimension, so it has 9 
     vertices, 18 edges and so on. We can access the faces of a certain codimension via:",
     
     EXAMPLE {
	  " L = faces(1,P11)",
	  " apply(L,vertices)"
	  },
     
     PARA{}, "We can compute all lattice points of the polyhedron with ",TO latticePoints,".",
     
     EXAMPLE {
	  " L = latticePoints P11",
	  " #L"
	  },
     
     PARA{}, "Evenmore the tail/recession cone of a polyhedron with ",TO tailCone,".",
     
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
     
     "We start with a cone in 2-space which is the positive hull (",TO posHull,") of a given set of rays.",
     
     EXAMPLE {
	  " R = matrix {{1,1,2},{2,1,1}}",
	  " C = posHull R",
	  " ambDim C"
	  },
     
     PARA{}, "This gives an overview of the characteristics of the cone. If we want to know 
     more details, we can ask for them.",
     
     EXAMPLE {
	  " rays C"
	  },
     
     PARA{}, "Using ",TO rays," we see that (1,1) is not an extremal ray of the cone.",
     
     EXAMPLE {
	  " HS = halfspaces C"
	  },
     
     PARA{}, "The function ",TO halfspaces," gives the defining linear half-spaces, i.e. ",TT "C"," is given by all ",TT "p"," in 
     the defining linear hyperplanes that satisfy ",TT "HS*p >= 0",". But in this case there are none, so the polyhedron is of full 
     dimension. Furthermore, we can construct the positive hull of a set of rays and a linear subspace.",
     
     EXAMPLE {
	  " R1 = R || matrix {{0,0,0}}",
	  " LS = matrix {{1},{1},{1}}",
	  " C1 = posHull(R1,LS)",
	  " rays C1"
	  },
     
     PARA{}, "Note that the rays are given modulo the lineality space. On the other hand we can 
     construct cones as the ",TO intersection," of linear half-spaces and hyperplanes.",
     
     EXAMPLE {
	  " HS = transpose R1",
	  " HP = matrix {{1,1,1}}",
	  " C2 = intersection(HS,HP)"
	  },
     
     PARA{}, "This is a two dimensional cone in 3-space with the following rays:",
     
     EXAMPLE {
	  " rays C2"
	  },
     
     PARA{}, "If we don't intersect with the hyperplane we get a full dimensional cone.",
     
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
     
     PARA{}, "On the other hand, we can take their positive hull by using ",TO posHull,":",
     
     EXAMPLE {
	  " C6 = posHull(C1,C2)",
	  " rays C6",
	  " linSpace C6"
	  },

     PARA{}, "Furthermore, both functions (",TO intersection," and ",TO posHull,") can 
     be applied to a list containing any number of cones and matrices defining 
     rays and lineality space or linear half-spaces and hyperplanes. These must be in the 
     same ambient space. For example:",
     
     EXAMPLE {
	  " R2 = matrix {{2,-1},{-1,2},{-1,-1}}",
	  " C7 = posHull {R2,C3,C4}",
	  " rays C7",
	  " linSpace C7"
	  },
     
     PARA{}, "Since they are all cones their positive hull is the same as their 
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
     two cones.",
     
     EXAMPLE {
	  " C8 = C * C1",
	  " rays C8",
	  " linSpace C8"
	  },
     
     PARA{}, "The result is in QQ^5.",
     
     EXAMPLE {
	  "ambDim C8"
	  },
     
     PARA{}, "To find out more about this cone use for example ",TO fVector,":",
     
     EXAMPLE {
	  " fVector C8"
	  },
     
     PARA{}, "This function gives the number of faces of each dimension, so it has 1 
     vertex, the origin, 1 line, 4 two dimensional faces and so on. We can access the
     faces of a certain codimension via ",TO faces,":",
     
     EXAMPLE {
	  " L = faces(1,C8)",
	  " apply(L,rays)"
	  },
     
     PARA{}, "We can also check if the cone is smooth:",
     
     EXAMPLE {
	  " isSmooth C8"
	  },
     
     PARA{}, "Evenmore we can compute the Hilbert basis of the cone with ",TO hilbertBasis,".",
     
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
     
     "We start by constructing a fan, which consists of a single cone and all of its 
     faces:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " F = fan C"
	  },
     
     PARA{}, "By this, we have already constructed the fan consisting of the 
     positive orthant and all of its faces. The package saves the generating cones 
     of the fan, which can be accessed by:",
     
     EXAMPLE {
	  "maxCones F"
	  },
     
     PARA{}, "Now we could expand the fan by adding more cones, for example the following:",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0,0},{1,1,0},{0,0,-1}}"
	  },
     
     PARA{}, "But in this case we can not, because the two cones are not compatible, 
     i.e. their intersection is not a face of each. So, when one tries to add a cone 
     to a fan that is not compatible with one of the generating cones of the fan, the 
     function ",TO addCone," gives an error. For two cones one can 
     check if their intersection is a common face by using ",TO commonFace,":",
     
     EXAMPLE {
	  " commonFace(C,C1)"
	  },
     
     PARA{}, "Since the intersection of both is already computed in this function 
     there is a different function, which also returns the intersection, to save 
     computation time when one needs the intersection afterward anyway:",
     
     EXAMPLE {
	  " (b,C2) = areCompatible(C,C1)",
	  " rays C2"
	  },
     
     PARA{}, "So we can make the cone compatible and add it to the fan.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}}",
	  " F = addCone(C1,F)"
	  },
     
     PARA{}, "Instead of creating a fan with one cone and then adding more cones, we 
     can also make a fan out of a list of cones:",
     
     EXAMPLE {
	  " C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " C3 = posHull matrix {{-1,0,0},{0,1,0},{0,0,-1}};",
	  " C4 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,1}};",
	  " C5 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = fan {C2,C3,C4,C5}"
	  },
     
     PARA{}, "Furthermore, we could add a list of cones to an existing fan:",
     
     EXAMPLE {
	  " C6 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};",
	  " C7 = posHull matrix {{1,0,0},{0,-1,0},{0,0,-1}};",
	  " F1 = addCone( {C6,C7}, F1)",
	  },
     
     PARA{}, "Finally, we can add a whole fan to another fan:",
     
     EXAMPLE {
	  " F1 = addCone(F,F1)"
	  },
     
     PARA{}, "So, ",TO fan," and ",TO addCone," are the methods to construct 
     fans ''from scratch'', but there are also methods to get fans directly, for example ",TO normalFan,", 
     which constructs the inner normal fan of a polytope.",
     
     EXAMPLE {
	  " P = hypercube 4",
	  " F2 = normalFan P"
	  },
     
     PARA{}, "Now we have seen how to construct fans, so we turn to functions on fans, 
     for example the direct product (",TO directProduct,":",
     
     EXAMPLE {
	  " F3 = fan {posHull matrix {{1}},posHull matrix {{-1}}}",
	  " F1 = F3 * F1"},
     
     PARA{}, "The result is in the direct product of the ambient spaces.",
     
     EXAMPLE {
	  " ambDim F1"
	  },
     
     PARA{}, "Of course, we can check if two fans are the same:",
     
     EXAMPLE {
	  " F1 == F2"
	  },
     
     PARA{}, "A bit more on fans can be found in part 2: ",TO "Working with fans - Part 2","."
     
     }

document {
     Key => "Working with fans - Part 2",
     
     "Now we construct a new fan to show some other functions.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};",
	  " C2 = posHull matrix {{1,1,1},{0,1,-1},{-1,1,1}};",
	  " C3 = posHull matrix {{-1,-1,-1},{0,1,-1},{-1,1,1}};",
	  " C4 = posHull matrix {{1,-1},{0,0},{-1,-1}};",
	  " F = fan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "This is not a ''very nice'' fan, as it is neither complete nor 
     of pure dimension:",
     
     EXAMPLE {
	  " isComplete F",
	  " isPure F"
	  },
     
     PARA{}, "If we add two more cones the fan becomes complete.",
     
     EXAMPLE {
	  " C5 = posHull matrix {{1,-1,1,-1},{-1,-1,0,0},{1,1,-1,-1}};",
	  " C6 = posHull matrix {{1,-1,1,-1},{1,1,0,0},{1,1,-1,-1}};",
	  " F = addCone({C5,C6},F)",
	  " isComplete F"
	  },
     
     PARA{}, "For a complete fan we can check if it is projective:",
     
     EXAMPLE {
	  " isPolytopal F"
	  },
     
     PARA{}, "If the fan is projective, the function returns a polyhedron such that  
     the fan is its  normal fan, otherwise it returns the empty polyhedron. This means 
     our fan is projective."
     
     }

document {
     Key => PolyhedralObject,
     Headline => "the class of all polyhedral objects in Polyhedra",
          
     TT "PolyhedralObject"," is the parent class of the three polyhedral objects in Polyhedra:",
     
     UL {
	  {TO "Polyhedron"},
	  {TO "Cone"},
	  {TO "Fan"}
	},
   
     EXAMPLE {
	  " convexHull matrix {{1,1,0,0},{1,0,1,0}}",
	  " posHull matrix {{1,2},{2,1}}",
	  " hirzebruch 3"
	  }
        
     }
        
document {     
     Key => Polyhedron,
     Headline => "the class of all convex polyhedra",
          
     "A Polyhedron represents a rational polyhedron. It can be bounded or unbounded, 
     need not be full dimensional or may contain a proper affine subspace. It can 
     be empty or zero dimensional. It is saved as a hash table which contains 
     the vertices, generating rays, and the basis of the lineality space of the 
     Polyhedron as well as the defining affine half-spaces and hyperplanes. The 
     output of a Polyhedron looks like this:",
     
     EXAMPLE {
	  " convexHull(matrix {{0,0,-1,-1},{2,-2,1,-1},{0,0,0,0}},matrix {{1},{0},{0}})",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Polyhedron. 
     Note that the number of rays and vertices are modulo the lineality space. So 
     for example a line in QQ^2 has one vertex and no rays. However, one can not
     access the above information directly, because this is just a virtual hash table 
     generated for the output. The data defining a Polyhedron is extracted 
     by the functions included in this package. A Polyhedron can be constructed as 
     the convex hull (",TO convexHull,") of a set of points and a set of rays or as the 
     intersection (",TO intersection,") of a set of affine half-spaces and affine hyperplanes.",
     
     PARA{}, "For example, consider the square and the square with an emerging ray 
     for the convex hull:",
     
     EXAMPLE {
	  " V = matrix {{1,1,-1,-1},{1,-1,1,-1}}",
	  " convexHull V",
	  " R = matrix {{1},{1}}",
	  " convexHull(V,R)"
	  },
     
     PARA{}, "If we take the intersection of the half-spaces defined by the directions of the 
     vertices and 1 we get the crosspolytope:",
     
     EXAMPLE {
	  " HS = transpose V",
	  " v = R || R",
	  " P = intersection(HS,v)",
	  " vertices P"
	  },
     
     PARA{}, "This can for example be embedded in 3-space on height 1:",
     
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
     dimensional, i.e. the origin. It is saved as a hash table which 
     contains the generating rays and the basis of the lineality space of the 
     cone as well as the defining half-spaces and hyperplanes. The output of a 
     Cone looks like this:",
     
     EXAMPLE {
	  " posHull matrix {{0,0,-1,-1,1},{2,-2,1,-1,0},{1,1,1,1,0}}",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Cone. The number 
     of rays is modulo the lineality space. However, one can not access the above information 
     directly, because this is just a virtual hash table generated for the output. The data 
     describing a Cone is extracted by the functions included in this package. A Cone 
     can be constructed as the positive hull (",TO posHull,")of a set of rays or as the intersection 
     (",TO intersection,") of a set of linear half-spaces and linear hyperplanes.",
     
     PARA{}, "As examples for the positive hull consider the following cones:",
     
     EXAMPLE {
	  " R = matrix{{1,2,3,1},{2,3,1,1},{3,1,2,1}}",
	  " C = posHull R",
	  " rays C",
	  " LS = matrix{{1},{1},{-2}}",
	  " C = posHull(R,LS)",
	  " rays C"
	  },
     
     PARA{}, "On the other hand, we can use these matrices as defining half-spaces and hyperplanes 
     for the intersection:",
     
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
     as a hash table which contains a list of the generating cones of the fan starting 
     with those of maximal dimension. So for every cone in this list all faces are considered 
     to be in the fan. The output of a Fan looks like this:",
     
     EXAMPLE {
	  " normalFan crossPolytope 3",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the Fan. 
     However, one can not access the above information directly, because this 
     is just a virtual hash table generated for the output. The data defining a Fan 
     is extracted by the functions included in this package. A Fan can be constructed by 
     collecting Cones that satisfy the intersection condition. Every cone that is added to 
     a Fan is always considered as the collection of the Cone and all of its faces.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{2,2},{1,-1}};",
	  " C2 = posHull matrix {{2,-2},{1,1}};",
	  " C3 = posHull matrix {{-2,-2},{1,-1}};",
	  " C4 = posHull matrix {{-2,2},{-1,-1}};",
	  " F = fan {C1,C2,C3,C4}"
	  },
     
     PARA{}, "This fan is for example the normal fan of a ''flattened'' crosspolytope in 2-space.",
     
     PARA{}, " See also ",TO "Working with fans","."
     
     }

document {     
     Key => PolyhedralComplex,
     Headline => "the class of all polyhedral complexes",
     
     "A PolyhedralComplex represents a complex of rational convex polyhedra, i.e. a collection of polyhedra, 
     such that for every polyhedron in the complex all faces are in the complex and for every two polyhedra in 
     the complex their intersection is a face of each (intersection condition). 
     It need not be full dimensional or pure, and the polyhedra need not be compact. It is saved 
     as a hash table which contains a list of the generating polyhedra of the complex starting 
     with those of maximal dimension. So for every polyhedron in this list all faces are considered 
     to be in the complex. The output of a PolyhedralComplex looks like this:",
     
     EXAMPLE {
	  " polyhedralComplex crossPolytope 3",
	  },
     
     PARA{}, "This table displays a short summary of the properties of the PolyhedralComplex. 
     However, one can not access the above information directly, because this 
     is just a virtual hash table generated for the output. The data defining a PolyhedralComplex 
     is extracted by the functions included in this package. A PolyhedralComplex can be constructed by 
     collecting Polyhedra that satisfy the intersection condition. Every polyhedron that is added to 
     a PolyhedralComplex is always considered as the collection of the Polyhedron and all of its faces.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{2,2,0},{1,-1,0}};",
	  " P2 = convexHull matrix {{2,-2,0},{1,1,0}};",
	  " P3 = convexHull matrix {{-2,-2,0},{1,-1,0}};",
	  " P4 = convexHull matrix {{-2,2,0},{-1,-1,0}};",
	  " F = polyhedralComplex {P1,P2,P3,P4}"
	  }
     
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
     a set of rays and computes the polyhedron that is the convex hull 
     of the points plus the rays. The two matrices must have the same 
     number of rows, i.e. the columns must lie in the same ambient space. 
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
     must have the same number of rows, which must equal the ambient dimension 
     of all cones and polyhedra.",
     
     PARA{}, "For example, consider the square in ",TO QQ,"^2:",
     
     EXAMPLE {
	  " M = matrix {{1,1,-1,-1},{1,-1,1,-1}}",
	  " P = convexHull M"
	  },
     
     PARA{}, "If we add a ray, then it is not compact anymore:",
     
     EXAMPLE {
	  " r = matrix {{1},{2}}",
	  " P =convexHull(M,r)"
	  },
     
     PARA{}, "If we add some more vertices to ",TT "M"," then we get a hexagon:",
     
     EXAMPLE {
	  " N = matrix {{-2,-2,0},{0,-2,-2}}",
	  " Q = convexHull(M|N)"
	  },
     
     PARA{}, "Again if we add the ray ",TT "r"," then the polyhedron is not compact:",
     
     EXAMPLE {
	  " Q1 = convexHull(M|N,r)"
	  },
     
     PARA{}, "To get this polyhedron we could also have used the application of ",TT "convexHull"," 
     to lists or pairs of polyhedra:",
     
     EXAMPLE {
	  " P1 = convexHull {P,N}",
	  " P1 == Q1",
	  " P1 = convexHull(P,Q)",
	  " P1 == Q1"
	  }
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
     space and computes the cone that is the positive hull of the rays plus 
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
     must have the same number of rows, which must equal the ambient dimension 
     of all cones and polyhedra.",
     
     PARA{}, "As a first example consider the following 2 dimensional cone in 3 space:",
     
     EXAMPLE {
	  " R = matrix {{1,2},{2,1},{0,0}}",
	  " C = posHull R"
	  },
     
     PARA{}, "We can construct a full dimensional cone out of this one by adding a lineality 
     space for example:",
     
     EXAMPLE {
	  " LS = matrix {{0},{0},{1}}",
	  " C1 = posHull (R,LS)"
	  },
     
     PARA{}, "The resulting cone is not pointed anymore, because it contains the subspace spanned 
     by (0,0,1). To get a full dimensional pointed cone we have to add another ray to C. For 
     this we can apply ",TT "posHull"," to a list containing ",TT "C"," and the new ray:",
     
     EXAMPLE {
	  " r = matrix {{0},{1},{2}}",
	  " C2 = posHull {C,r}"
	  },
     
     PARA{}, "Another way would be, if we would have ",TT "r"," not as a ray but already as 
     a cone:",
     
     EXAMPLE {
	  " r = posHull r"
	  },
     
     PARA{}, "In this case we can just take the positive hull of the two cones:",
     
     EXAMPLE {
	  " C3 = posHull(C,r)",
	  " C3 == C2"
	  }
     
     }

document {
     Key => {intersection, (intersection,Cone,Cone), (intersection,List), (intersection,Matrix), 
	  (intersection,Matrix,Matrix), (intersection,Matrix,Matrix,Matrix,Matrix), (intersection,Polyhedron,Polyhedron),
	  (intersection,Cone,Polyhedron), (intersection,Polyhedron,Cone)},
     Headline => "computes the intersection of half-spaces, hyperplanes, cones, and polyhedra",
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
     must have the same number of rows, which must equal the ambient dimension 
     of all cones and polyhedra.",
     
     PARA{}, "The first use of ",TT "intersection"," is to construct a cone:",
     
     EXAMPLE {
	  " M = matrix {{1,2,3},{2,3,1},{3,1,2}}",
	  " C = intersection M"
	  },
     
     PARA{}, "This is the cone of all points that are positive on the rows of the 
     matrix ",TT "M",". If we add another row to this matrix and enter a condition 
     vector we get a polyhedron:",
     
     EXAMPLE {
	  " M = M || matrix {{-1,-1,-1}}",
	  " v = matrix {{1},{2},{3},{4}}",
	  " P = intersection(M,v)"
	  },
     
     PARA{}, " This polyhedron, a tetrahedron, consists of all points ",TT "p"," such 
     that ",TT "M*p <= v",". If add a another pair of matrices, these conditions are 
     evaluated as equalities. Thus we get a polyhedron which is not of full dimension. 
     It is an intersection with an affine hyperplane.",
     
     EXAMPLE {
	  " N = matrix {{1,2,0}}",
	  " w = matrix {{2}}",
	  " Q = intersection (M,v,N,w)"
	  },
     
     PARA{}, "If we have another polyehdron or cone, we can also intersect them with the others.",
     
     EXAMPLE {
	  " HC = intersection(matrix {{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}},matrix {{1},{1},{1},{1},{1},{1}})",
	  " C1 = intersection(C,HC)",
	  " Q1 = intersection(P,HC)"
	  }     

     }

document {
     Key => {fan, (fan,Cone), (fan,List)},
     Headline => "generates a Fan",
     Usage => " F = fan C \nF = fan L",
     Inputs => {
	  "C" => Cone,
	  "L" => List => {"with elements of class ", TO Cone, " or ", TO Fan}
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, " If ",TT "fan", " is applied to a ", TO Cone, " it generates 
     the ", TO Fan, " given by the the Cone and all of its faces. If applied to 
     a ", TO List, " the list must only contain Cones and Fans in the same 
     ambient space. Then it adds the Cones in the List and the generating Cones 
     of the Fans in the List one by one to the Fan, checking each time if the 
     new Cone is compatible with the cones that have already been added, i.e. 
     that the intersection with each of them is a face of both Cones 
     (intersection condition).",
     
     PARA{}, "If one of the cones is in the wrong ambient space, there will be an 
     error and no fan will be returned. If the intersection condition fails, there 
     will also be an error. The pairs of incompatible cones can be accessed with the 
     function ",TO incompCones,".",
          
     EXAMPLE {
	  " C = posHull matrix {{1,-1},{0,-1}}",
	  " F = fan C",
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{0,-1},{1,-1}};",
	  " F = fan {C,C1,C2}"
	  }
     
     }

document {
     Key => {polyhedralComplex, (polyhedralComplex,Polyhedron), (polyhedralComplex,List)},
     Headline => "generates a PolyhedralComplex",
     Usage => " PC = polyhedralComplex P \nPC = polyhedralComplex L",
     Inputs => {
	  "P" => Polyhedron,
	  "L" => List => {"with elements of class ", TO Polyhedron, " or ", TO PolyhedralComplex}
	  },
     Outputs => {
	  "F" => PolyhedralComplex
	  },
     
     PARA{}, " If ",TT "polyhedralComplex", " is applied to a ", TO Polyhedron, " it generates 
     the ", TO PolyhedralComplex, " given by the the Polyhedron and all of its faces. If applied to 
     a ", TO List, " the list must only contain Polyhedra and PolyhedralComplexes in the same 
     ambient space. Then it adds the Polyhedra in the List and the generating Polyhedra 
     of the PolyhedralComplexes in the List one by one to the new PolyhedralComplex, checking each time if the 
     new Polyhedron is compatible with the polyhedra that have already been added, i.e. 
     that the intersection with each of them is a face of both Polyhedra 
     (intersection condition).",
     
     PARA{}, "If one of the polyhedra is in the wrong ambient space (i.e. not the ambient space of the first object in 
     the list), then there will be an error and no PolyhedralComplex will be returned. If the intersection condition fails, there 
     will also be an error. The pairs of incompatible polyhedra can be accessed with the 
     function ",TO incompPolyhedra,".",
          
     EXAMPLE {
	  " P = convexHull matrix {{1,-1,0},{0,-1,0}}",
	  " PC = polyhedralComplex P",
	  " P1 = convexHull matrix {{1,0,0},{0,1,0}};",
	  " P2 = convexHull matrix {{0,-1,0},{1,-1,0}};",
	  " PC = polyhedralComplex {PC,P1,P2}"
	  }
     
     }

document {
     Key => {addCone, (addCone,Cone,Fan), (addCone,List,Fan), (addCone,Fan,Fan)},
     Headline => "adds cones to a Fan",
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
     compatible with every generating Cone of ",TT "F", ", but is not a face of one 
     of them. If one of the first two conditions fails, there will be an error and no fan 
     will be returned. The pairs of incompatible cones can be accessed with the 
     function ",TO incompCones,". If the last condition fails, then the cone is already in 
     the fan as a face of one of the cones, so it does not have to be added. The conditions 
     are checked in this order.",
     
     PARA{}, "If ",TT "addCone"," is applied to a ",TO List," and a ",TO Fan,", then 
     the function adds the list cone by cone and stops if one of the three conditions 
     fails for one of the cones. There is again an error for the first two conditions. The 
     pairs of incompatible cones can again be retrieved using ",TO incompCones,".",
     
     PARA{}, "If applied to a pair of fans it adds the generating cones of the first 
     fan to the second fan, again checking for the same conditions as above.",
     
     PARA{}, " As an example, we make a fan consisting of the following cone and 
     try to add an adjacent orthant.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,1},{0,0,1}};",
	  " F = fan C",
	  " C = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};",
	  " incompCones(C,F)"
	  },
     
     PARA{}, "This shows that the two cones do not intersect in a common face, but 
     if we divide C into two parts, we get a fan.",
     
     EXAMPLE {
	  " C1 = intersection {C, (matrix {{0,1,-1}}, matrix {{0}})};",
	  " C2 = intersection {C, (matrix {{0,-1,1}}, matrix {{0}})};",
	  " F = addCone({C1,C2},F)"
	  }
     
     }

document {
     Key => {addPolyhedron, (addPolyhedron,Polyhedron,PolyhedralComplex), (addPolyhedron,List,PolyhedralComplex), (addPolyhedron,PolyhedralComplex,PolyhedralComplex)},
     Headline => "adds Polyhedra to a PolyhedralComplex",
     Usage => " PC1 = addPolyhedron(P,PC) \nPC1 = addPolyhedron(L,PC)",
     Inputs => {
	  "P" => Polyhedron,
	  "L" => List => {"with elements of class ", TO Polyhedron, " or ", TO PolyhedralComplex},
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "PC1" => PolyhedralComplex
	  },
     
     PARA{}, "If ",TT "addPolyhedron", " is applied to a ", TO Polyhedron, " and a ",TO PolyhedralComplex, " 
     it adds the Polyhedron to the PolyhedralComplex if they are in the same ambient space, if the Polyhedron is 
     compatible with every generating Polyhedron of ",TT "PC", ", but is not a face of one 
     of them. If one of the first two conditions fails, there will be an error and no PolyhedralComplex 
     will be returned. The pairs of incompatible polyhedra can be accessed with the 
     function ",TO incompPolyhedra,". If the last condition fails, then the Polyhedron is already in 
     the PolyhedralComplex as a face of one of the polyhedra, so it does not have to be added. The conditions 
     are checked in this order.",
     
     PARA{}, "If ",TT "addPolyhedron"," is applied to a ",TO List," and a ",TO PolyhedralComplex,", then 
     the function adds the list Polyhedron by Polyhedron and stops if one of the three conditions 
     fails for one of the polyhedra. There is again an error for the first two conditions. The 
     pairs of incompatible polyhedra can again be retrieved using ",TO incompPolyhedra,". Note that the may also 
     contain PolyhedralComplexes. Then the function replaces it by its list of generating polyhedra.",
     
     PARA{}, "If applied to a pair of PolyhedralComplexes it adds the generating polyhedra of the first 
     PolyhedralComplex to the second PolyhedralComplex, again checking for the same conditions as above.",
     
     PARA{}, " As an example, we make a PolyhedralComplex consisting of the following Polyhedron and 
     try to add an adjacent cube.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,1,1,1,2,2,2,2},{0,0,1,1,0,0,1,1},{0,1,0,1,0,1,0,1}};",
	  " PC = polyhedralComplex P",
	  " P = hypercube 3;",
	  " incompPolyhedra(P,PC)"
	  },
     
     PARA{}, "This shows that the two polyhedra do not intersect in a common face, but 
     if we divide P into three parts, we get a PolyhedralComplex.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,0,1,1,0,0,1,1},{0,1,0,1,0,1,0,1}};",
	  " P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,1,1,-1,0,1,1,-1},{0,0,-1,-1,0,0,-1,-1}};",
	  " P3 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{0,0,-1,-1,0,0,-1,-1},{0,1,1,-1,0,1,1,-1}};",
	  " P == convexHull {P1,P2,P3}",
	  " PC = addPolyhedron({P1,P2,P3},PC)"
	  }
     
     }

document {
     Key => {ambDim, (ambDim,PolyhedralObject)},
     Headline => "ambient dimension of a Polyhedron, Cone or Fan",
     Usage => "d = ambDim P \nd = ambDim C \nd = ambDim F",
     Inputs => {
	  "P" => Polyhedron,
	  "C" => Cone,
	  "F" => Fan
	  },
     Outputs => {
	  "d" => ZZ => {", the dimension of the ambient space"}
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
     rays, for example:",
     
     EXAMPLE {
	  " apply(L,rays)"
	  }
     
     }

document {
     Key => {polyhedra, (polyhedra,ZZ,PolyhedralComplex)},
     Headline => "computes all polyhedra of a polyhedral complex of a certain dimension",
     Usage => " L = polyhedra(d,PC)",
     Inputs => {
	  "d" => ZZ => {" between 0 and the dimension of the polyhedral complex"},
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "polyhedra", " computes the ", TO List, " of all Polyhedra in ",
     TT "PC", " of dimension ", TT "d", ".",
     
     EXAMPLE {
	  " PC = polyhedralComplex hypercube 3",
	  " L = polyhedra(2,PC)",
	  },
     
     PARA{}, "To actually see the polyhedra of the complex we can look at their 
     vertices, for example:",
     
     EXAMPLE {
	  " apply(L,vertices)"
	  }
     
     }

document {
     Key => {maxCones, (maxCones,Fan)},
     Headline => "displays the generating Cones of a Fan",
     Usage => " L = maxCones F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "maxCones", " displays the ", TO List, " of generating cones 
     of the ", TO Fan, ", i.e. all Cones that are not a face of one 
     of the other cones. These are all of the same dimension if and only if 
     the Fan is pure (see: ", TO isPure,").",
     
     EXAMPLE {
	  " F = normalFan crossPolytope 3",
	  " L = maxCones F",
	  " apply(L,rays)"
	  }
     
     }

document {
     Key => {maxPolyhedra, (maxPolyhedra,PolyhedralComplex)},
     Headline => "displays the generating Polyhedra of a PolyhedralComplex",
     Usage => " L = maxPolyhedra PC",
     Inputs => {
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, TT "maxPolyhedra", " displays the ", TO List, " of generating polyhedra 
     of the ", TO PolyhedralComplex, ", i.e. all Polyhedra that are not a face of one 
     of the other Polyhedra. These are all of the same dimension if and only if 
     the PolyhedralComplex is pure (see: ", TO isPure,").",
     
     EXAMPLE {
	  " PC = skeleton(1,polyhedralComplex hypercube 2)",
	  " L = maxPolyhedra PC",
	  " apply(L,vertices)"
	  }
     
     }

document {
     Key => {halfspaces, (halfspaces,Cone), (halfspaces,Polyhedron)},
     Headline => "computes the defining half-spaces of a Cone or a Polyhedron",
     Usage => " M = halfspaces C \n(M,v) = halfspaces P",
     Inputs => {
	  "C" => Cone,
	  "P" => Polyhedron
	  },
     Outputs => {
	  "M" => Matrix => {"with entries over ", TO QQ},
	  "v" => Matrix => {"with entries over ", TO QQ, " and only one column"}
	  },
     
     PARA{}, TT "halfspaces", " returns the defining affine half-spaces. For a 
     polyhedron ", TT "P", " the output is ", TT "(M,v)", ", where the source 
     of ", TT "M", " has the dimension of the ambient space of ", TT "P", " 
     and ", TT "v", " is a one column matrix in the target space 
     of ", TT "M", " such that ",TT "P = {p in H | M*p =< v}", " where 
     ", TT "H", " is the intersection of the defining affine hyperplanes.",
     
     PARA{}, " For a cone ", TT "C", " the output is the matrix", TT "M"," that is the 
     same matrix as before but ", TT "v", " is omitted since it is 0, 
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
     ", TT "H", " is the intersection of the defining affine half-spaces.",
     
     PARA{}, " For a cone ", TT "C", " the output is the matrix ", TT "N", ", that 
     is the same matrix as before but ", TT "w", " is omitted since it is 0, 
     so ", TT "C = {c in H | N*c = 0}", " and ", TT "H", " is the intersection 
     of the defining linear half-spaces.",
     
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
     Key => {vertices, (vertices,Polyhedron), (vertices,PolyhedralComplex)},
     Headline => "displays the vertices of a Polyhedron or a PolyhedralComplex",
     Usage => " V = vertices P",
     Inputs => {
	  "P" => Polyhedron => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "V" => Matrix
	  },
     
     PARA{}, TT "vertices", " returns the vertices of the Polyhedron or PolyhedralComplex ", TT "P", " 
     as the columns of the Matrix ", TT "V",".",
     
     EXAMPLE {
	  " P = intersection(matrix{{1,-1},{0,-1},{-1,-1},{0,1}}, matrix{{0},{-1},{0},{1}})",
	  " vertices P",
	  " PC = skeleton(2,polyhedralComplex hypercube 3)",
	  " vertices PC"
	  }
     
     }

document {
     Key => {areCompatible, (areCompatible,Cone,Cone), (areCompatible,Polyhedron,Polyhedron)},
     Headline => "checks if the intersection of two cones/polyhedra is a face of each",
     Usage => " (b,X) = areCompatible(X1,X2)",
     Inputs => {
	  "X1" => Cone => {"or ",ofClass Polyhedron},
	  "X2" => Cone => {"or ",ofClass Polyhedron}
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face of each cone, 
	       and ", TO false, " otherwise."},
	  "C" => Cone => {"or ",ofClass Polyhedron,", the intersection of both if they are of the 
	                  same type and compatible, otherwise the empty polyhedron."}
	  },
     
     PARA{}, TT "areCompatible", " is an extension of ", TO commonFace, " for two Cones and for two 
     Polyhedra. It also checks if the intersection ", TT "X", " of ", TT "X1", " and ", TT "X2", " is a 
     face of each and the answer is given by ", TT "b", ". Furthermore, the intersection 
     is given for further calculations if the two cones/polyhedra lie in the same ambient space. Otherwise,  
     the empty polyhedron in the ambient space of ",TT "X1"," is given. Note that the input arguments 
     must either both be polyhedra or both be cones.",
     
     PARA{}, "For example, consider the following three cones",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-1}};",
	  " C3 = posHull matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "These might form a fan, but if we check if they are compatible, we see they 
     are not:",
     
     EXAMPLE {
	  " areCompatible(C1,C2)",
	  " areCompatible(C2,C3)",
	  " areCompatible(C3,C1)",
	  }
     
     }

document {
     Key => {commonFace, (commonFace,Cone,Cone), (commonFace,Polyhedron,Polyhedron), 
	  (commonFace,Cone,Fan), (commonFace,Fan,Cone), (commonFace,Fan,Fan), (commonFace,List)},
     Headline => "checks if the intersection is a face of both Cones or Polyhedra, or of cones with fans",
     Usage => " b = commonFace(C1,C2) \nb = commonFace(P1,P2) \nb = commonFace(X,F) \nb = commonFace(F,X) \nb = commonFace L",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone,
	  "P1" => Polyhedron,
	  "P2" => Polyhedron,
	  "F" => Fan,
	  "X" => {TO Cone," or ",TO Fan},
	  "L" => List
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the intersection is a face both, 
	       and ", TO false, " otherwise."}
	  },
     
     PARA{}, TT "commonFace", " checks if the intersection of ", TT "C1", " 
     and ", TT "C2", " or the intersection of ", TT "P1", " and ", TT "P2", " is 
     a face of both. If it is applied to a pair of a cone ",TT "C"," and a fan ",TT "F"," then 
     it checks if the intersection of ",TT "C"," with every generating cone of ",TT "F"," is 
     a face of each. For two fans it checks this condition for every pair of generating cones. 
     If applied to a list then the list must contain Fans and Cones and it checks pairwise for 
     a common face.",
     
     PARA{}, "For example, consider the following three cones:",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-1}};",
	  " C3 = posHull matrix {{1,-1},{2,-1}};",
	  },
     
     PARA{}, "for each pair of two of them we can check if their intersection is a common face:",
     
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
     Headline => "checks if the first argument contains the second argument",
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
    
    PARA{}, "For example, we can check if the 3 dimensional crosspolytope contains the hypercube 
    or the other way around:",
    
    EXAMPLE {
	" P = hypercube 3",
	" Q = crossPolytope 3",
	" contains(Q,P)",
	" contains(P,Q)"
	},
    
    PARA{}, "We can also check if the hypercube lies in the positive orthant.",
    
    EXAMPLE {
	 " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};",
	 " contains(C,P)",
	 " P = affineImage(P,matrix{{1},{1},{1}})",
	 " contains(C,P)"
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
     Key => {isComplete, (isComplete,Fan), (isComplete,PolyhedralComplex)},
     Headline => "checks completeness of a Fan or PolyhedralComplex",
     Usage => " b = isComplete X",
     Inputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "b" => Boolean => { TO true, " if the ", TO Fan, "/",TO PolyhedralComplex," is complete, ", TO false, " otherwise"}
	  },
     
     PARA{}, TT "isComplete"," just calls an entry in the hash table of the Fan. The check for completeness 
     is done while generating the fan. Whenever a full dimensional Cone is added (see ", TO fan," 
     or ", TO addCone,") the set of faces of codimension 1 that appear only in one full dimensional Cone 
     is updated. The Fan is then complete if and only if this set is empty and there is at least one 
     full dimensional Cone.",
     
     PARA{}," For a ",TO PolyhedralComplex," the function does the same. Just note, that a complete polyhedral 
     complex must contain non-compact polyhedra.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-2}};",
	  " C3 = posHull matrix {{0,-2},{1,-1}};",
	  " F = fan {C1,C2,C3}",
	  " isComplete F"
	  },
     
     PARA{}, "Hence the fan above is not complete, but we can add the missing cone:",
     
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
     
     PARA{}, "Thus, ",TT "Q"," is not a face of ",TT "P",", but we can extend it to a face.",
     
     EXAMPLE {
	  " v = matrix{{1},{-1},{-1}};",
	  " Q = convexHull{Q,v}",
	  " isFace(Q,P)"
	  }
     
     }

document {
     Key => {isLatticePolytope, (isLatticePolytope,Polyhedron)},
     Headline => "checks if a polyhedron is a lattice polytope",
     Usage => "b = isLatticePolytope P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "b" => Boolean => {"true if P is a lattice polytope"}
	  },

     PARA{}, TT "isLatticePolytope"," can only be applied to polytopes, i.e. compact polyhedra. It
     simply checks if it is compact and all vertices are lattice points.",

     EXAMPLE {
	  " P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{1},{1},{1},{1}})",
	  " isLatticePolytope P",
	  " P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{4},{6},{3},{6}})",
	  " isLatticePolytope P"
	    }

     }

document {
     Key => (isNormal,Polyhedron),
     Headline => "checks if a polytope is normal in the ambient lattice",
     Usage => "b = isNormal P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "b" => Boolean => {"true if P is normal in the ambient lattice"}
	  },

     PARA{}, TT "isNormal"," can only be applied to polytopes, i.e. compact polyhedra. It
     embeds the polytope on height 1 in a space of dimension plus 1 and takes the Cone over
     this polytope. Then it checks if all elements of the Hilbert basis lie in height 1.",

     EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " isNormal P"
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
     Cones is pointed. This is equivalent to all Cones being pointed.",
     
     EXAMPLE {
	  " C = intersection(matrix{{1,1,-1},{-1,-1,-1}})",
	  " isPointed C",
	  " C = intersection{C,(matrix{{1,-1,-1}},0)}",
	  " isPointed C"
	  }
     
     }

document {
     Key => {isPolytopal, (isPolytopal,Fan)},
     Headline => "checks if a Fan is polytopal",
     Usage => "b = isPolytopal F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "b" => Boolean => {TO true, " if the ",TO Fan," is polytopal, ",TO false," otherwise"}
	  },
     
     PARA{}, "If ",TT "F"," is projective, then there exists a polyhedron ",TT "P"," such that ",TT "F"," 
     is the normalFan of ",TT "P",". This means every codimension 1 cone of the Fan corresponds exactly to 
     an edge of the polytope. So consider ", TO QQ," to the number of all edges. This can be considered as the 
     space of all edge lengths. If we take arbitrary lengths now for every edge we do not get a polytope. But 
     every codimension 2 cone of the fan corresponds to a 2 dimensional face of the polytope and if the edges 
     belonging to this face add up to 0 zero, they form in fact a 2 dimensional face. This gives linear 
     equations on the space of edge lengths and if we intersect these equations with the positive orthant in 
     the space of edge lengths we get a Cone. Thus, there exists such a polytope if and only if there is a 
     vector in this cone with strictly positive entries, since every edge has to appear in the polytope.",
     
     PARA{}, "IF ",TT "F"," is polytopal, the function ",TO polytope," returns a polytope of which ",TT "F"," is 
     the normalFan.",
     
     PARA{}, "Note that the function first checks if the fan is complete.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1}};",
	  " C2 = posHull matrix {{1,-1},{0,-2}};",
	  " C3 = posHull matrix {{0,-2},{1,-1}};",
	  " C4 = posHull matrix {{-1,-2},{-2,-1}};",
	  " F = fan{C1,C2,C3,C4}",
	  " isPolytopal F"
	  }
     
     }

document {
     Key => {isPure,(isPure,Fan),(isPure,PolyhedralComplex)},
     Headline => "checks if a Fan or PolyhedralComplex is of pure dimension",
     Usage => " b = isPure X",
     Inputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Fan,"/",TO PolyhedralComplex," is of pure dimension, ",TO false," otherwise"}
	  },
     
     PARA{}, TT "isPure", " tests if the ", TO Fan,"/",TO PolyhedralComplex," is pure by checking if the first 
     and the last entry in the list of generating Cones/Polyhedra are of the same dimension.",
     
     PARA{}, "Let us construct a fan consisting of the positive orthant and the ray ",TT "v"," that is the 
     negative sum of the canonical basis, which is obviously not pure:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}}",
	  " v = posHull matrix {{-1},{-1},{-1}}",
	  " F = fan {C,v}",
	  " isPure F",
	  },
     
     PARA{}, "But we can make a pure fan if we choose any two dimensional face of the positive 
     orthant and take the cone generated by this face and ",TT "v"," and add it to the cone:",
     
     EXAMPLE {
	  " C1 = posHull{(faces(1,C))#0,v}",
	  " F = addCone(C1,F)",
	  " isPure F"
	  }
     
     }

document {
     Key => {isReflexive, (isReflexive,Polyhedron)},
     Headline => " checks if a Polytope is reflexive",
     Usage => " b = isReflexive P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Polyhedron," is reflexive, ",TO false," otherwise"}
	  },
     
     PARA{},"A lattice polytope ",TT "P"," in the ",TO QQ," space of a lattice ",TEX ///$M$///," 
     is reflexive if its polar polytope is also a lattice polytope. The function checks if ",TT "P"," 
     is compact, a lattice polytope and if the dual is a lattice polytope.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,0,-1},{0,1,-1}}",
	  " isReflexive P"
	  },
     
     SeeAlso => {isCompact,
	  isLatticePolytope,
	  polar}
     
     }

document {
     Key => {isSimplicial, (isSimplicial,PolyhedralObject)},
     Headline => " checks if a polyhedral object is simplicial",
     Usage => " b = isSimplicial X",
     Inputs => {
	  "X" => PolyhedralObject
	  },
     Outputs => {
	  "b" => {TO true," if the ",TO PolyhedralObject," is simplicial, ",TO false," otherwise"}
	  },
     
     PARA{},"A ",TO Polyhedron," of dimension ",TEX///$d$///," is simplicial if it is compact and 
     has ",TEX///$d+1$///," vertices.",
     
     EXAMPLE {
	  " P = convexHull matrix {{3,0,0,0,1},{0,3,0,0,1},{0,0,3,0,1}}",
	  " isSimplicial P",
	  " P = hypercube 2",
	  " isSimplicial P"
	  },
     
     PARA{},"A ",TO Fan," of dimension ",TEX///$d$///," is simplicial if it is pointed and 
     has ",TEX///$d$///," rays.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0,1},{0,1,0,1},{0,0,1,1}}",
	  " isSimplicial C",
	  " C = posHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " isSimplicial C"
	  },
     
     PARA{},"A ",TO Fan,"/",TO PolyhedralComplex," is simplicial if  every ",TO Cone,"/",TO Polyhedron," 
     of it is simplicial.",
     
     EXAMPLE {
	  " F = normalFan hypercube 3",
	  " isSimplicial F",
	  " PC = skeleton(2,polyhedralComplex crossPolytope 3)",
	  " isSimplicial PC"
	  },
     
     SeeAlso => {isCompact,
	  isPointed,
	  dim,
	  vertices,
	  rays}
     
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
     Key => {isVeryAmple,(isVeryAmple,Polyhedron)},
     Headline => "checks if the Polyhedron is very ample",
     Usage => " b = isVeryAmple P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "b" => {TO true," if the ", TO Polyhedron," is very ample, ",TO false," otherwise"}
	  },
     
     PARA{}, "A lattice polytope ",TT "P"," in the ",TO QQ," space of a lattice ",TEX ///$M$///," 
     is very ample if for every vertex ",TEX ///$v\in P$///," the semigroup ",TEX ///$\mathbb{N}(P\cap M - v)$///," 
     generated by ",TEX ///$P\cap M - v = \{v'-v|v'\in P\cap M\}$///," is saturated in ",TEX ///$M$///,". 
     For example, normal lattice polytopes are very ample.",
     
     PARA{}, "Note that therefore ",TT "P"," must be compact and a lattice polytope.",
     
     EXAMPLE {
	  " P = convexHull matrix {{0,1,0,0,1,0,1,2,0,0},{0,0,1,0,1,0,2,2,0,-1},{0,0,0,1,2,0,1,2,0,-1},{0,0,0,0,-1,1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,1,1}}",
	  " isVeryAmple P"
	  }
     }	  

document {
     Key => dualFaceLattice,
     Headline => "computes the dual face lattice of a cone or polyhedron"
     }

document {
     Key => {(dualFaceLattice,ZZ,Cone), (dualFaceLattice,Cone)},
     Headline => "computes the dual face lattice of a cone",
     Usage => " L = dualFaceLattice C \nL = dualFaceLattice(k,C)",
     Inputs => {
	  "k" => ZZ => {"between 0 and the dimension of ",TT "C"},
	  "C" => Cone
   	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "The dual face lattice of a cone ",TT "C"," displays for each",TT "k"," the faces of 
     dimension ",TT "k"," as a list of integers, indicating the bounding halfspaces of ",TT "C"," that generate 
     this face together with the hyperplanes. If no integer is given the function returns the faces of all dimensions in a list, 
     starting with the Cone itself.",
     
     EXAMPLE{
	  " C = posOrthant 4",
	  " dualFaceLattice(2,C)"
	  },
     
     PARA{}, "Returns the faces of dimension two, where the integers give the rows in the halfspaces
     matrix of the cone:",
     
     EXAMPLE{
	  " R = halfspaces C"
	  },
     
     PARA{}, "The complete dual face lattice is returned if no integer is given:",
     
     EXAMPLE{
	  " dualFaceLattice C",
	  }
     }

document {
     Key => {(dualFaceLattice,ZZ,Polyhedron), (dualFaceLattice,Polyhedron)},
     Headline => "computes the dual face lattice of a polyhedron",
     Usage => " L = dualFaceLattice P \nL = dualFaceLattice(k,P)",
     Inputs => {
	  "k" => ZZ => {"between 0 and the dimension of ",TT "X"},
	  "P" => Polyhedron
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "The dual face lattice of a polyhedron ",TT "P"," displays for each",TT "k"," the faces of 
     dimension ",TT "k"," as a list of integers, indicating the halfspaceces of ",TT "P"," that generate this 
     face together with the hyperplanes. If no integer is given the function returns the faces of all dimensions 
     in a list, starting with the polyhedron itself.",
     
     EXAMPLE{
	  " P = convexHull(matrix{{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}},matrix {{0},{0},{-1}})",
	  " dualFaceLattice(2,P)"
	  },
     
     PARA{}, "Returns the faces of dimension two where each list of integers gives the rows in the halfspaces
     matrix of the polyhedron:",
     
     EXAMPLE{
	  " V = halfspaces P"
	  },
     
     PARA{}, "The complete face lattice is returned if no integer is given:",
     
     EXAMPLE{
	  " faceLattice P",
	  }
     }

document {
     Key => faceLattice,
     Headline => "computes the face lattice of a cone or polyhedron"
     }

document {
     Key => {(faceLattice,ZZ,Cone), (faceLattice,Cone)},
     Headline => "computes the face lattice of a cone",
     Usage => " L = faceLattice C \nL = faceLattice(k,C)",
     Inputs => {
	  "k" => ZZ => {"between 0 and the dimension of ",TT "C"},
	  "C" => Cone
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "The face lattice of a cone ",TT "C"," displays for each",TT "k"," the faces of 
     codimension ",TT "k"," as a list of integers, indicating the rays of ",TT "C"," that generate 
     this face together with the lineality space. If no integer is given the function returns the faces of all codimensions in a list, 
     starting with the 0 dimensional face.",
     
     EXAMPLE{
	  " C = posOrthant 4",
	  " faceLattice(1,C)"
	  },
     
     PARA{}, "Returns the faces of codimension one where the integers give the columns in the rays 
     matrix of the cone:",
     
     EXAMPLE{
	  " R = rays C"
	  },
     
     PARA{}, "The complete face lattice is returned if no integer is given:",
     
     EXAMPLE{
	  " faceLattice C",
	  }
     }

document {
     Key => {(faceLattice,ZZ,Polyhedron), (faceLattice,Polyhedron)},
     Headline => "computes the face lattice of a polyhedron",
     Usage => " L = faceLattice P \nL = faceLattice(k,P)",
     Inputs => {
	  "k" => ZZ => {"between 0 and the dimension of ",TT "P"},
	  "P" => Polyhedron
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "The face lattice of a polyhedron ",TT "P"," displays for each",TT "k"," the faces of 
     codimension ",TT "k"," as two lists of integers, the first indicating the vertices of ",TT "P"," and 
     the second indicating the rays of ",TT "P"," that generate this face together with the lineality space. 
     If no integer is given the function returns the faces of all codimensions in a list, 
     starting with the 0 dimensional faces",
     
     EXAMPLE{
	  " P = convexHull(matrix{{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}},matrix {{0},{0},{-1}})",
	  " faceLattice(1,P)"
	  },
     
     PARA{}, "Returns the faces of codimension one where the first list of integers give the columns in the vertices
     matrix of the polyhedron and the second list the columns in the rays matrix of the polyhedron:",
     
     EXAMPLE{
	  " V = vertices P",
	  " R = rays P"
	  },
     
     PARA{}, "The complete face lattice is returned if no integer is given:",
     
     EXAMPLE{
	  " faceLattice P",
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
     
     PARA{}, "For example, we can look at the edges of the cyclicPolytope with 5 vertices in 3 space",
     
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
     the cone modulo the lineality space, so it returns the list of one column matrices that give 
     the Hilbert basis of the Cone if one adds the basis of the lineality space and its negative. 
     For the Project-and-Lift-algorithm see: ",
     
     PARA{}, HREF("http://www.hemmecke.de/raymond/", "Raymond Hemmecke's"), " ", EM "On the 
     computation of Hilbert bases of cones", ", in A. M. Cohen, X.-S. Gao, and N. Takayama, 
     editors, Mathematical Software, ICMS 2002, pages 307317. World Scientific, 2002.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2},{2,1}}",
	  " hilbertBasis C"
	  }
     
     }

document {
     Key => {incompCones, (incompCones,List), (incompCones,Cone,Fan), (incompCones,Fan,Cone), (incompCones,Fan,Fan)},
     Headline => "returns the pairs of incompatible cones",
     Usage => " Lpairs = incompCones L \nLpairs = incompCones(X,F) \nLpairs = incompCones(F,X)",
     Inputs => {
	  "L" => List,
	  "F" => Fan,
	  "X" => {TO Cone," or ",TO Fan,}
	  },
     Outputs => {
	  "Lpairs" => List
	  },
     
     PARA{}, "If ",TT "incompCones"," is applied to a list of cones and fans, then it returns the pairs of elements 
     whose intersection is not a face of each. For a cone ",TT "C"," and a fan ",TT "F"," in the list this means there 
     is at least one generating cone of ",TT "F"," whose intersection with ",TT "C"," is not a face of each. For two 
     fans in the list this means there is at least one generating cone each such that their intersection is not a face
     of each. If applied to a pair consisting of a cone and a fan or two fans, then it returns the pairs of cones that 
     do not share a common face.",
     
     EXAMPLE {
	  " C1 = posHull matrix{{1,0},{1,1}};",
	  " C2 = posHull matrix{{1,0},{0,-1}};",
	  " C3 = posHull matrix{{-1,0},{0,1}};",
	  " C4 = posHull matrix{{1,1},{0,1}};",
	  " C5 = posHull matrix {{1,2},{2,1}};",
	  " L = {C1,C2,C3,C4,C5};",
	  " Lpairs = incompCones L",
	  " Lpairs == {(C1,C4),(C1,C5)}",
	  }
     
     }

document {
     Key => {incompPolyhedra, (incompPolyhedra,List), (incompPolyhedra,Polyhedron,PolyhedralComplex), (incompPolyhedra,PolyhedralComplex,Polyhedron), (incompPolyhedra,PolyhedralComplex,PolyhedralComplex)},
     Headline => "returns the pairs of incompatible polyhedra",
     Usage => " Lpairs = incompPolyhedra L \nLpairs = incompPolyhedra(X,PC) \nLpairs = incompPolyhedra(PC,X)",
     Inputs => {
	  "L" => List,
	  "PC" => PolyhedralComplex,
	  "X" => {TO Polyhedron," or ",TO PolyhedralComplex,}
	  },
     Outputs => {
	  "Lpairs" => List
	  },
     
     PARA{}, "If ",TT "incompPolyhedra"," is applied to a list of polyhedra and polyhedral complexes, then it returns the pairs of elements 
     whose intersection is not a face of each. For a Polyhedron ",TT "P"," and a PolyhedralComplex ",TT "PC"," in the list this means there 
     is at least one generating Polyhedron of ",TT "PC"," whose intersection with ",TT "P"," is not a face of each. For two 
     polyhedral complexes in the list this means there is at least one generating polyhedron each such that their intersection is not a face
     of each. If applied to a pair consisting of a polyhedron and a polyhedral complex or two polyhedral complexes, then it returns the pairs of polyhedra that 
     do not share a common face.",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,0,0},{1,1,0}};",
	  " P2 = convexHull matrix {{1,0,0},{0,-1,0}};",
	  " P3 = convexHull matrix {{-1,0,0},{0,1,0}};",
	  " P4 = convexHull matrix {{1,1,0},{0,1,0}};",
	  " P5 = convexHull matrix {{1,2,0},{2,1,0}};",
	  " L = {P1,P2,P3,P4,P5};",
	  " Lpairs = incompPolyhedra L",
	  " Lpairs == {(P1,P4),(P1,P5)}",
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
     Key => {interiorLatticePoints, (interiorLatticePoints,Polyhedron)},
     Headline => "computes the lattice points in the relative interior of a polytope",
     Usage => " L = interiorLatticePoints P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "L" => List => {"containing the interior lattice points as matrices over ",TO ZZ," with only 
	       one column"}
	  },
     
     PARA{}, TT "latticePoints"," can only be applied to polytopes, i.e. compact polyhedra. It 
     returns all lattice points in the relative interior of the polytope.",
     
     EXAMPLE {
	  " P = crossPolytope(3,2)",
	  " interiorLatticePoints P",
	  " Q = cyclicPolytope(2,4)",
	  " interiorLatticePoints Q"
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
     Key => {maxFace, (maxFace,Matrix,Polyhedron), (maxFace,Matrix,Cone)},
     Headline => "computes the face of a Polyhedron or Cone where a weight attains its maximum",
     Usage => " F = maxFace(w,P) \nF = maxFace(w,C)",
     Inputs => {
	  "w" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a 
	       weight vector"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => {"Depending on the input, a Cone or a Polyhedron, the face where ",TT "w"," attains 
	       its maximum"}
	  },
     
     PARA{}, TT "maxFace"," computes the face of the given Polyhedron ",TT "P"," or Cone ",TT "C","  
     where ",TT "w"," attains its maximum.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " w = matrix {{1},{-1},{0}}",
	  " F = maxFace(w,P)",
	  " vertices F"
	  }
     }
	  

document {
     Key => {minFace, (minFace,Matrix,Polyhedron), (minFace,Matrix,Cone)},
     Headline => "computes the face of a Polyhedron or Cone where a weight attains its minimum",
     Usage => " F = minFace(w,P) \nF = minFace(w,C)",
     Inputs => {
	  "w" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a 
	       weight vector"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "F" => {"Depending on the input, a Cone or a Polyhedron, the face where ",TT "w"," attains 
	       its minimum"}
	  },
     
     PARA{}, TT "minFace"," computes the face of the given Polyhedron ",TT "P"," or Cone ",TT "C"," 
     where ",TT "w"," attains its minimum.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " w = matrix {{1},{2},{0}}",
	  " F = minFace(w,P)",
	  " vertices F"
	  }
     }
	  

document {
     Key => {minkSummandCone, (minkSummandCone,Polyhedron)},
     Headline => "computes the Cone of all Minkowski summands and the minimal decompositions",
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
     of the positive orthant with these equations. Thus, every point in ",TT "C"," corresponds 
     to a Minkowski summand (probably after rescaling). The corresponding polyhedron to every 
     minimal generator of ",TT "C"," is saved in the hash table ",TT "L",". Finally, all possible 
     minimal decompositions of ",TT "P"," are saved as columns in the matrix ",TT "M",".",
     
     PARA{}, "For example, consider the Minkowski summand cone of the hexagon.",
     
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
     Key => {mixedVolume, (mixedVolume,List)},
     Headline => "computes the mixed volume of a list of polytope",
     Usage => " v = mixedVolume L",
     Inputs => {
	  "L" => List => {"containing n polytopes in n-space"}
	  },
     Outputs => {
	  "v" => ZZ => {"the mixed volume"}
	  },
     
     PARA{},"Let ",TEX ///$P_1,...,P_n$///," be polytopes in ",TEX ///$n$///,"-space. Then the volume 
     of the Minkowski sum ",TEX ///$\lambda_1 P_1 + ... + \lambda_n P_n$///," is a homogeneous polynomial of degree ",
     TEX ///$n$///," in nonnegative variables ",TEX ///$\lambda_1,...,\lambda_n$///,". The coefficient Vol",
     TEX ///$(P_1,...,P_n)$///," of ",TEX ///$\lambda_1\lambda_2 ... \lambda_n$///," is called 
     the mixed volume of ",TEX ///$P_1,...,P_n$///,". For example, the number of toric solutions 
     to a generic system of ",TEX ////$n$///," polynomial equations on ",TEX ///$n$///,"-space amounts to 
     the mixed volume of the corresponding Newton polytopes.",
     
     PARA{},"The function ",TT "mixedVolume"," takes the ",TO List," ",TT "L"," with ",TEX ///$n$///," polytopes 
     in ",TEX ///$n$///,"-space and computes their mixed Volume by using the algorithm by Ioannis Z. Emiris in his paper ",
     HREF("http://www.nag.co.uk/projects/frisco/frisco/reports/d33421.ps", "Mixed Volume Implementation"),". Note that this function 
     computes an upper bound by using a random lifting. To reassure the result run the function until it returns the same result.",
     
     PARA{},"CAVEAT: So far the input is not checked so use the function with care!",
     
     EXAMPLE {
	  " P = crossPolytope 2",
	  " Q = hypercube 2",
	  " mixedVolume {P,Q}"
	  }
     
     }
     

document {
     Key => {objectiveVector, (objectiveVector,Polyhedron,Polyhedron)},
     Headline => "computes an objective vector of a face of a polyhedron",
     Usage => " v = objectiveVector(P,Q)",
     Inputs => {
	  "P" => Polyhedron,
	  "Q" => Polyhedron => {"which must be a face of ",TT "P"}
	  },
     Outputs => {
	  "v" => Matrix => {"one column vector over ",TO QQ," representing a vector"}
	  },
     
     PARA{}, "An objective vector ",TT "v"," of a face ",TT "Q"," of a polyhedron ",TT "P"," is vector 
     such that ",TT "Q = {p in P | v*p = max over P}"," i.e. it is the face on which ",TT "v"," attains 
     its maximum.",
     
     EXAMPLE{
	  " P = hypercube 3",
	  " Q = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " v = objectiveVector(P,Q)"
	  },
     
     PARA{}, "Since it is the face on which ",TT "v"," attains its maximum it can be recovered with ",TO maxFace,":",
     
     EXAMPLE{
	  " Q == maxFace(v,P)"
	  }
     }

document {
     Key => (normalCone,Polyhedron,Polyhedron),
     Headline => "computes the normal cone of a face of a polyhedron",
     Usage => " C = normalCone(P,Q)",
     Inputs => {
	  "P" => Polyhedron,
	  "Q" => Polyhedron => {"which must be a face of ",TT "P"}
	  },
     Outputs => {
	  "C" => Cone
	  },
     
     PARA{}, "The normal cone of a face ",TT "Q"," of a polyhedron ",TT "P"," is the cone in the normal fan (see ",TO normalFan,")
     that corresponds to this face. This is the cone of all vectors attaining their maximum on this face.",
     
     EXAMPLE{
	  " P = hypercube 3",
	  " Q = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}}",
	  " C = normalCone(P,Q)",
	  " rays C"
	  }
     }

document {
     Key => {polytope, (polytope,Fan)},
     Headline => "returns a polytope of which the fan is the normal fan if it is polytopal",
     Usage => " P = polytope F",
     Inputs => {
	  "F" => Fan
	  },
     Outputs => {
	  "P" => Polyhedron
	  },
     
     PARA{}, "If the fan ",TT "F"," is polytopal then ",TT "polytope"," returns a polytope ",TT "P",". ",TT "F"," is the 
     normal fan of this polytope. Note that such a polytope is not unique.",
     
     EXAMPLE {
	  " F = fan {posHull matrix {{1,0},{0,1}},posHull matrix {{0,-1},{1,1}},posHull matrix {{-1,-1},{0,1}},posHull matrix {{-1,1},{0,-1}},posHull matrix {{1,1},{0,-1}}}",
	  " P = polytope F"
	  }
     
     }

document {
     Key => {proximum, (proximum,Matrix,Polyhedron), (proximum,Matrix,Cone)},
     Headline => "computes the proximum of the Polyhedron/Cone to a point in euclidian metric",
     Usage => " q = proximum(p,P) \nq = proximum(p,C)",
     Inputs => {
	  "p" => Matrix => {" over ",TO ZZ," or ",TO QQ," with only one column representing a point"},
	  "P" => Polyhedron,
	  "C" => Cone
	  },
     Outputs => {
	  "q" => Matrix => {" over ",TO QQ," with only one column representing the closest point"}
	  },
     
     PARA{}, "For a point ",TT "p"," and a Polyhedron ",TT "P"," or a Cone ",TT "C",", ",TT "proximum"," 
     computes the point in ",TT "P"," or ",TT "C"," with minimal euclidian distance to ",TT "p",".",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " p = matrix {{1},{2},{3}}",
	  " q = proximum(p,P)"
	  }
     
     }

document {
     Key => {skeleton, (skeleton,ZZ,Fan), (skeleton,ZZ,PolyhedralComplex)},
     Headline => "computes the k-skeleton of a Fan or PolyhedralComplex",
     Usage => " X = skeleton(k,F) \nX = skeleton(k,PC)",
     Inputs => {
	  "k" => ZZ,
	  "F" => Fan,
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "X" => Fan => {"or ",ofClass PolyhedralComplex}
	  },
     
     PARA{}, "For a ",TO Fan,TT " F"," and an integer ",TT "k"," between 0 and the dimension of ",TT "F",", 
     ",TT "skeleton"," computes the ",TT "k","-skeleton of the ",TO Fan," ",TT "F",", 
     i.e. the ",TO Fan," ",TT "F1"," generated by all cones of dimension 
     ",TT "k"," in ",TT "F",".",
     
     PARA{}, "For example, we can look at the 2-skeleton of the fan of projective 
     3-space:",
     
     EXAMPLE {
	  " P = convexHull matrix{{1,0,0,0},{0,1,0,0},{0,0,1,0}}",
	  " F = normalFan P",
	  " F1 = skeleton(2,F)",
	  " apply(maxCones F1,rays)"
	  },
     
     PARA{}, "For a ",TO PolyhedralComplex,TT " PC"," and an integer ",TT "k"," between 0 and the dimension of ",TT "PC",", 
     ",TT "skeleton"," computes the ",TT "k","-skeleton of the ",TO PolyhedralComplex," ",TT "PC",", 
     i.e. the ",TO PolyhedralComplex," ",TT "PC1"," generated by all polyhedra of dimension 
     ",TT "k"," in ",TT "PC",".",
     
     EXAMPLE {
	  " PC = polyhedralComplex hypercube 3",
	  " PC1 = skeleton(2,PC)",
	  " apply(maxPolyhedra PC1,vertices)"
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
     second argument. The function computes the smallest face of the second argument that 
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
	  " F = fan C"
	  },
     
     PARA{}, "This cone is not smooth, therefore also the fan is not. But if we remove the interior and one 
     of the two dimensional faces the resulting subfan is smooth.",
     
     EXAMPLE {
	  " F1 = smoothSubfan F",
	  " apply(maxCones F1, rays)"
	  }
     
     }

document {
     Key => {stellarSubdivision, (stellarSubdivision,Fan,Matrix)},
     Headline => "computes the stellar subdivision of the fan by a ray",
     Usage => "F1 = stellarSubdivision(F,r)",
     Inputs => {
	  "F" => Fan,
	  "r" => Matrix => {"with one column in the ambient space of the fan"}
	  },
     Outputs => {
	  "F1" => Fan
	  },
     
     PARA{}, "This function computes the stellar subdivision of ",TT "F"," by inserting the 
     ray given by ",TT "r",".",
     
     EXAMPLE {
	  " F = normalFan hypercube 2",
	  " r = matrix {{1},{1}}",
	  " F1 = stellarSubdivision(F,r)"
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
     Key => {triangulate, (triangulate,Polyhedron)},
     Headline => "computes a triangulation of a polytope",
     Usage => " L = triangulate P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "L" => List => {" containing the simplices of the triangulation"}
	  },
     
     PARA{}, TT "triangulate","  computes the triangulation of the polyhedron ",TT "P",", if it is compact, 
     i.e. a polytope, recursively. For this, it takes all facets and checks if they are simplices. If so, then 
     it takes the convex hull of these with the weighted centre of the polytope (the sum of the vertices divided 
     by the number of vertices). For those that are not simplices it takes all their facets and does the same 
     for these.",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " triangulate P"
	  }
     
     }

document {
     Key => {volume, (volume,Polyhedron)},
     Headline => "computes the volume of a polytope",
     Usage => " v = volume P",
     Inputs => {
	  "P" => Polyhedron => {", which must be compact"}
	  },
     Outputs => {
	  "v" => QQ
	  },
     
     PARA{}, TT "volume"," computes the volume of a polytope. To do this, it triangulates the polytope first. The volume 
     of a simplex is |det(v_1-v_0,..,v_n-v_0)|/n!, where v_0,..,v_n are the vertices of the simplex.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " volume P"
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
     
     PARA{}, "Computes the affine hull of a polyhedron. This is the affine subspace with the same 
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
     must have the same number of rows. Then ",TT "affineImage"," computes the 
     polyhedron ",TT "{(A*c)+b | c in C}"," and the cone ",TT "{A*c | c in C}"," if ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone.",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "This Cone can be mapped to the positive orthant:",
     
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
     must have the same number of rows. Then ",TT "affineImage"," computes the 
     polyhedron ",TT "{(A*p)+v | p in P}"," where ",TT "v"," is set to 0 if omitted and ",TT "A"," is the 
     identity if omitted.",
     
     PARA{}, "For example, consider the following two dimensional polytope:",
     
     EXAMPLE {
	  " P = convexHull matrix {{-2,0,2,4},{-8,-2,2,8}}",
	  },
     
     PARA{}, "This polytope is the affine image of the square:",
     
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
     polyhedron ",TT "{q | (A*q)+b in C}"," or the cone ",TT "{q | (A*q) in C}"," if ",TT "b"," is 0 or omitted. 
     If ",TT "A"," is omitted then it is set to identity.",
     
     PARA{}, "For example, consider the following three dimensional cone:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2,3},{3,1,2},{2,3,1}}",
	  },
     
     PARA{}, "We can look at its preimage under the following map:",
     
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
     
     PARA{}, "As an example, we construct the octahedron as the bipyramid over the square 
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
     Usage => " F = ccRefinement R",
     Inputs => {
	  "R" => Matrix
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "The coarsest common refinement of a set of rays ",TT "R"," is the common refinement 
     of all possible triangulations  of the rays.",
     
     PARA{}, "For example, consider a three dimensional cone with four rays:",
     
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
     converts the cone into the same cone but of class ",TO Polyhedron,".",
     
     PARA{}, "Consider the positive orthant in ",TO QQ,"^3:",
     
     EXAMPLE {
	  " C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}}"
	  },
     
     PARA{}, "If we want to consider the positive orthant not as cone but as a polyhedron we 
     apply ",TT "coneToPolyhedron",":",
     
     EXAMPLE {
	  " P = coneToPolyhedron C"
	  },
     
     PARA{}, "Although, they are the same geometric object but of different classes, Polyhedra 
     considers them not as equal:",
     
     EXAMPLE {
	  " P === C"
	  }
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
     Key => { faceFan, (faceFan,Polyhedron)},
     Headline => " computes the fan generated by the cones over the faces",
     Usage => " F = faceFan P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "F" => Fan
	  },
     
     PARA{}, "For a polyhedron with the origin in its relative interior, the face fan is the fan 
     generated by the cones over the faces of the polytope. Hence the origin must be in hte relative interior.",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " F = faceFan P",
	  "apply(maxCones F, rays)"
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
     
     PARA{}, "The Minkowski sum of ",TT "X"," and ",TT "Y"," is the polyhedron 
     ",TT "X + Y = {x + y | x in X, y in Y}",". If ",TT "X"," and ",TT "Y"," are both 
     cones, then their Minkowski sum is their positive hull, which is a cone, so the 
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
	  " apply(maxCones F,rays)"
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
     Key => {polarFace, (polarFace,Polyhedron)},
     Headline => " computes the dual face of the polar polyhedron",
     Usage => " fv = polarFace f",
     Inputs => {
	  "f" => Polyhedron
	  },
     Outputs => {
	  "fv" => Polyhedron
	  },
     
     PARA{}, "When computing a face ",TT "f"," of a polyhedron ",TT "P"," with the function ",TO faces,", 
     it is stored in the cache that ",TT "f"," is a face of ",TT "P",". Then the function ",TT "polarFace"," 
     computes the ",TO polar," ",TT "P'"," of ",TT "P"," and the corresponding face of ",TT "P'"," on which 
     all points of ",TT "f"," attain their minimum. Note that this function only works correctly for polyhedra 
     with the origin in its relative interior.",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " f = first faces(1,P)",
	  " fv = polarFace f",
	  " vertices fv"
	  },
     
     PARA{}, "If ",TT "f"," is not a face of another polytope, then it considers ",TT "f"," as a face of itself. 
     Thus, it computes the polar of ",TT "f",", and returns the empty polyhedron as a face of the polar of ",
     TT "f",".",
     
     EXAMPLE {
	  " P = hypercube 3",
	  " polarFace P"
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
     Key => {sublatticeBasis, (sublatticeBasis,Matrix), (sublatticeBasis,Polyhedron)},
     Headline => "computes a basis for the sublattice generated by integral vectors or the lattice points of a polytope",
     Usage => " B = sublatticeBasis M \nB = sublatticeBasis P",
     Inputs => {
	  "M" => Matrix => {" over ",TO ZZ," with each column representing a sublattice generator"},
	  "P" => Polyhedron,
	  },
     Outputs => {
	  "B" => {"A matrix over ", TO ZZ," containing a sublattice basis"}
	  },

     PARA{}, TT "sublatticeBasis"," computes a basis for the sublattice generated by the columns of",TT "M"," or 
     by the lattice points of",TT "P",".",

      EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " sublatticeBasis P"
	    }
     }

document {
     Key => {toSublattice, (toSublattice,Polyhedron)},
     Headline => "calculates the preimage of a polytope in the sublattice generated by its lattice points",
     Usage => "Q = toSublattice P",
     Inputs => {
	  "P" => Polyhedron => {"which must be compact"}
	  },
     Outputs => {
	  "Q" => Polyhedron => {"preimage of P in the sublattice generated by its lattice points"}
	  },

     PARA{}, TT "toSublattice"," can only be applied to polytopes, i.e. compact polyhedra. It
     calculates a basis of the sublattice generated by its lattice points, and then takes the affine 
     preimage under the corresponding map.",

     EXAMPLE {
	  " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
	  " toSublattice P"
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
     Key => {cellDecompose, (cellDecompose,Polyhedron,Matrix)},
     Headline => "computes the regular cell decomposition",
     Usage => " L = cellDecompose(P,w)",
     Inputs => {
	  "P" => Polyhedron => {"compact"},
	  "w" => Matrix => {"a one row matrix, with an entry for each lattice point of the polyhedron"}
	  },
     Outputs => {
	  "L" => List
	  },
     
     PARA{}, "This function computes the regular cell decomposition of ",TT "P"," given by the weight vector ",TT "w",". 
     This is computed by placing the i-th lattice point of ",TT "P"," on height ",TT "w","_i in n+1 space, taking the 
     convexHull of these with the ray (0,...,0,1), and projecting the compact faces into n space. Note that the polyhedron 
     must be compact, i.e. a polytope and the length of the weight vector must be the number of lattice points.",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " w =  matrix {{1,2,2,2,2,2,1}}",
	  " L = cellDecompose(P,w)",
	  " apply(L,vertices)"
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
    Key => {ehrhart, (ehrhart,Polyhedron)},
    Headline => "calculates the Ehrhart polynomial of a polytope",
    Usage => "f = ehrhart P",
    Inputs => {
         "P" => Polyhedron => {"which must be compact"}
         },
    Outputs => {
         "f" => RingElement => {"Ehrhart polynomial as element of QQ[x]"}
         },

    PARA{}, TT "ehrhart"," can only be applied to polytopes, i.e. compact polyhedra. 
    To calculate the Ehrhart polynomial, the number of lattice points in the first n 
    dilations of the polytope are calculated, where n is the dimension of the polytope. 
    A system of linear equations is then solved to find the polynomial.",

    EXAMPLE {
         " P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}",
         " ehrhart P"
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
	  " apply(maxCones F,rays)"
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
     
     PARA{}, "Consider the Vandermond determinant in 3 variables:",
     
     EXAMPLE {
	  " R = QQ[a,b,c]",
	  " f = (a-b)*(a-c)*(b-c)"
	  },
     
     PARA{}, "If we compute the Newton polytope we get a hexagon in ",TT "QQ","^3.",
     
     EXAMPLE {
	  " P = newtonPolytope f"
	  }
     
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
     Key => {secondaryPolytope, (secondaryPolytope, Polyhedron)},
     Headline => "computes the secondary polytope of a compact polyhedron",
     Usage => " Q = secondaryPolytope P",
     Inputs => {
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "The secondary polytope parametrises the regular cell decompositions of a polytope. See ...
     ",TT "to be added",".",
     
     EXAMPLE {
	  " P = crossPolytope 2",
	  " Q = secondaryPolytope P",
	  " vertices Q"
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
     first edition, 1995.",
     
     PARA{}, "Consider the following ideal in a ring with 3 variables:",
     
     EXAMPLE {
	  " R = QQ[a,b,c]",
	  " I = ideal (a-b,a-c,b-c)"
	  },
     
     PARA{}, "The state polytope of this ideal is a triangle in 3 space, because the ideal has three 
     initial ideals:",
     
     EXAMPLE {
	  " statePolytope I"
	  },
     
     PARA{}, "The generators of the three initial ideals are given in the first part of the result."
     
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
     Key => (symbol ?,Cone,Cone),
     Headline => "compares the Cones",
     Usage => " b = C1 ? C2",
     Inputs => {
	  "C1" => Cone,
	  "C2" => Cone
	  },
     Outputs => {
	  "b" => {TT ">"," or ",TT "<"," or ",TT "="}
	  },
     
     PARA{}, "This induces an order on Cones. ",TT "C1"," is greater then ",TT "C2"," if 
     its ambient dimension is greater, if this is equal then if its dimension is higher and 
     if this is equal if it has the higher ordered rays and lineality space.",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,0},{0,1},{1,1}}",
	  " C2 = posHull matrix {{1,0,1},{0,1,0},{1,1,0}}",
	  " C1 ? C2"
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
     
     PARA{}, "Computes the direct product of ",TT "C1"," and ",TT "C2",". This is the cone 
     ",TT "{(x,y) | x in C1, y in C2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO directProduct,".",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,2},{2,1}}",
	  " C2 = posHull matrix {{1}}",
	  " C = C1 * C2",
	  " rays C"
	  }
     
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
     
     PARA{}, "Computes the direct product of ",TT "C"," and ",TT "P",". This is the 
     polyhedron ",TT "{(c,p) | c in C, p in P}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO directProduct,".",
     
     EXAMPLE {
	  " C = posHull matrix {{1,2},{2,1}}",
	  " P =convexHull matrix {{1},{-1}}",
	  " Q = C * P",
	  " (vertices Q,rays Q)"
	  }
     
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
     
     PARA{}, "Computes the direct product of ",TT "P"," and ",TT "C",". This is the polyhedron 
     ",TT "{(p,c) | p in P, x in C}",", in the direct product of the ambient spaces.", 
     
     PARA{}, "See also ",TO directProduct,".",
     
     EXAMPLE {
	  " P =convexHull matrix {{1},{-1}}",
	  " C = posHull matrix {{1,2},{2,1}}",
	  " Q = P * C",
	  " (vertices Q,rays Q)"
	  }
     
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
     
     PARA{}, "Computes the direct product of ",TT "P1"," and ",TT "P2",".This is the polyhedron 
     ",TT "{(x,y) | x in P1, y in P2}",", in the direct product of the ambient spaces.",
     
     PARA{}, "See also ",TO directProduct,".",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,-1,0,0},{0,0,1,-1}}",
	  " P2 = convexHull matrix {{1},{-1}}",
	  " P = P1 * P2",
	  " vertices P"
	  }
     
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
     
     PARA{}, "Computes the direct product of two fans. This is the fan given by ",TT "C=C1 x C2"," 
     for all cones ",TT "C1 in F1"," and ",TT "C2 in F2",", in the direct product of the 
     ambient spaces.",
     
     PARA{}, "See also ",TO (directProduct,Fan,Fan),".",
     
     EXAMPLE {
	  " F1 = normalFan hypercube 1",
	  " F2 = normalFan hypercube 2",
	  " F = F1 * F2",
	  " F == normalFan hypercube 3"
	  }
          
     }

document {
     Key => {(symbol *,QQ,Polyhedron), (symbol *,ZZ,Polyhedron)},
     Headline => "rescales a polyhedron by a given positive factor",
     Usage => " Q = k * P",
     Inputs => {
	  "k" => {TO ZZ," or ",TO QQ,", strictly positive"},
	  "P" => Polyhedron
	  },
     Outputs => {
	  "Q" => Polyhedron
	  },
     
     PARA{}, "Rescales the ",TO Polyhedron," by the strictly positive factor
     ",TT "k",".",
     
     EXAMPLE {
	  " P = crossPolytope 3",
	  " k = 3",
	  " Q = k * P",
	  " vertices Q"
	  }
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
     
     PARA{}, "Computes the Minkowski sum of ",TT "C1"," and ",TT "C2",". This is the cone 
     ",TT "C1 + C2 = {x + y | x in C1, y in C2}",". Note that ",TT "C1"," and ",TT "C2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " C1 = posHull matrix {{1,2,3},{2,3,1},{3,1,2}}",
	  " C2 = posHull matrix {{1},{0},{0}}",
	  " C = C1 + C2",
	  " rays C"
	  }
     
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
     
     PARA{}, "Computes the Minkowski sum of ",TT "C"," and ",TT "P",".This is the polyhedron 
     ",TT "C + P = {c + p | c in C, p in P}",". Note that ",TT "C"," and ",TT "P"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " C = posHull matrix {{1},{2},{0}}",
	  " P = hypercube 3",
	  " Q = C + P",
	  " (vertices Q,rays Q)"
	  }
     
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
     
     PARA{}, "Computes the Minkowski sum of ",TT "P"," and ",TT "C",". This is the polyhedron 
     ",TT "P + C = {p + c | p in P, c in C}",". Note that ",TT "P"," and ",TT "C"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " P = hypercube 2",
	  " C = posHull matrix {{1},{2}}",
	  " Q = P + C",
	  " (vertices Q,rays Q)"
	  }
     
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
     
     PARA{}, "Computes the Minkowski sum of ",TT "P1"," and ",TT "P2",".This is the polyhedron 
     ",TT "P1 + P2 = {x + y | x in P1, y in P2}",". Note that ",TT "P1"," and ",TT "P2"," have 
     to lie in the same ambient space.", 
     
     PARA{}, "See also ",TO minkowskiSum,".",
     
     EXAMPLE {
	  " P1 = convexHull matrix {{1,0,0},{0,1,0}}",
	  " P2 = convexHull matrix {{-1,0,0},{0,-1,0}}",
	  " P = P1 + P2",
	  " vertices P"
	  }
          
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
     
     PARA{}, "Returns the dimension of a cone.",
     
     EXAMPLE {
	  " C = posHull matrix {{2,3},{3,2}}",
	  " dim C"
	  }
     
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
     is the maximal dimension of all cones of 
     the fan.",
     
     EXAMPLE {
	  " F = hirzebruch 3",
	  " dim F"
	  }
     
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
     
     PARA{}, "Returns the dimension of a polyhedron.",
     
     EXAMPLE {
	  " P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}}",
	  " dim P"
	  }
     
     }

document {
     Key => (dim,PolyhedralComplex),
     Headline => "computes the dimension of a polyhedral complex",
     Usage => " d = dim PC",
     Inputs => {
	  "PC" => PolyhedralComplex
	  },
     Outputs => {
	  "d" => ZZ
	  },
     
     PARA{}, "Returns the dimension of a polyhedral complex. This 
     is the maximal dimension of all polyhedra of the complex.",
     
     EXAMPLE {
	  " PC = polyhedralComplex crossPolytope 3",
	  " dim PC"
	  }
     
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
     Key => (net,PolyhedralComplex),
     Headline => "displays characteristics of a polyhedral complex",
     Usage => " net PC",
     Inputs => {
	  "PC" => PolyhedralComplex
	  },
     
     PARA{}, "Displays an overview of the properties of the 
     PolyhedralComplex, the ambient dimension, the number of generating 
     polyhedra, and the top dimension of the polyhedra.",
     
     EXAMPLE {
	  " PC = polyhedralComplex faces(2,hypercube 3)",
	  " net PC"
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
     
     PARA{}, "All convex polyhedral objects (",TO Cone,",",TO Fan,",",TO Polyhedron,") that have been assigned 
     to a ",TO Symbol," will be saved into the file ",TT "F",". If the package ",TT "PPDivisor"," is loaded, then 
     also all ",TT "PolyhedralDivisors"," are saved into ",TT "F",". Also every ",TO List," or ",TO Sequence," 
     containing any of the above types or lists and sequences of them in arbitrary nested depth of lists is saved.",
     
     PARA{}, "To recover the session simply call ",TT "load F",". It is not necessary that ",TT "Polyhedra"," is already 
     loaded (if not, it will be) and also ",TT "PPDivisor"," is loaded if it was loaded when the session had been saved."
     
     }


-- Test 0
-- Checking convexHull basics
TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
assert(P#"number of vertices" == 3)
assert(dim P == 2)
assert(ambDim P == 3)
assert(rays P == 0)
assert(linSpace P == 0)
M = matrix {{3,4,1}};
v = matrix {{10}};
assert(hyperplanes P == (M,v) or hyperplanes P == (-M,-v))
///

-- Test 1
-- Checking convexHull basics
TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
P = convexHull {P,(matrix{{4},{0},{-2}},matrix{{1,0,0},{0,1,-1},{0,0,0}})};
assert(dim P == 3)
assert(image linSpace P == image matrix {{0},{1},{0}})
assert(hyperplanes P == (0,0))
///

-- Test 2
-- Checking convexHull halfspaces
TEST ///
P = convexHull (matrix{{1},{1}},matrix{{1,0},{0,1}});
M1 = matrix {{-1,0},{0,-1}};
v = matrix {{-1},{-1}};
assert(halfspaces P == (M1,v))
///

-- Test 3
-- Checking convexHull and intersection
TEST ///
P2 =  convexHull matrix {{1,-2,-1,2},{2,1,-2,-1}};
M = matrix{{3,1},{-3,-1},{1,-3},{-1,3}};
v = matrix{{5},{5},{5},{5}};
assert(intersection(M,v) == P2)
///

-- Test 4
-- Checking intersection
TEST ///
P = intersection (matrix{{1,0},{0,1},{-1,0},{0,-1}},matrix{{1},{2},{3},{4}});
V1 = vertices P;
V1 = set apply(numColumns V1, i -> V1_{i});
V2 = set {matrix{{1_QQ},{2}},matrix{{1_QQ},{-4}},matrix{{-3_QQ},{2}},matrix{{-3_QQ},{-4}}};
assert(isSubset(V1,V2) and isSubset(V2,V1))
///

-- Test 5
-- Checking polar
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
Q = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
P = polar P;
assert(P == Q)
P = convexHull(matrix {{1,-1,1,-1},{1,1,-1,-1},{1,2,3,4}},matrix {{0,0},{0,0},{1,-1}});
Q = convexHull matrix {{1,-1,0,0},{0,0,1,-1},{0,0,0,0}};
P = polar P;
assert(P == Q)
///

-- Test 6
-- Checking intersections that give cones
TEST ///
C = intersection matrix {{1,2},{2,1}};
R1 = rays C;
R1 = set apply(numColumns R1, i -> R1_{i});
R2 = set {matrix{{2},{-1}},matrix{{-1},{2}}};
assert(isSubset(R1,R2) and isSubset(R2,R1))
assert(linSpace C == 0)
assert(dim C == 2)
assert(ambDim C == 2)
///

-- Test 7
-- Checking intersection that give a not pointed cone and intersection for lists
TEST ///
C = intersection matrix {{1,2,1},{2,1,1}};
assert(image linSpace C == image matrix{{1},{1},{-3}})
assert(ambDim C == 3)
P = intersection {hypercube 3,C,(matrix{{1,1,1}},matrix{{1}})};
V = matrix {{1/3,1,0,1,1,-1,-1/3},{1/3,0,1,1,-1,1,-1/3},{-1,-1,-1,-1,1,1,1}};
assert(vertices P == V);
///

-- Test 8
-- Checking posHull
TEST ///
C = posHull(matrix{{1,0},{0,1},{0,0}},matrix{{0},{0},{1}});
assert(halfspaces C == matrix{{1,0,0},{0,1,0}})
assert(C#"number of rays" == 2)
///

-- Test 9
-- Checking contains for polyhedra
TEST ///
P1 = convexHull matrix {{0,1,1,0},{0,0,1,1}};
P2 = convexHull matrix {{0,2,0},{0,0,2}};
assert contains(P2,P1)
assert(not contains(P1,P2))
P1 = convexHull(matrix {{0,1,1,0},{0,0,1,1},{0,0,0,0}},matrix {{0},{0},{1}});
P2 = convexHull matrix {{0,1,1,0},{0,0,1,1},{1,2,3,4}};
assert(not contains(P2,P1))
assert contains(P1,P2)
///

-- Test 10
-- Checking contains for cones
TEST ///
C1 = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C2 = posHull matrix {{1},{1},{1}};
assert contains(C1,C2)
assert(not contains(C2,C1))
C2 = posHull {C2, matrix {{1,-1,0,0},{0,0,1,-1},{0,0,0,0}}};
assert contains(C2,C1)
assert(not contains(C1,C2))
///

-- Test 11
-- Checking equality for polyhedra and cones
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
Q = intersection(matrix{{1,0},{-1,0},{0,1},{0,-1}},matrix{{1},{1},{1},{1}});
assert(P == Q)
C1 = posHull matrix {{1,2},{2,1}};
C2 =intersection matrix {{2,-1},{-1,2}};
assert(C1 == C2)
///

-- Test 12
-- Checking dualCone
TEST ///
C1 = posHull matrix {{1,2,3},{2,3,1},{3,1,2}};
C2 = posHull matrix {{-5,7,1},{1,-5,7},{7,1,-5}};
C1 = dualCone C1;
assert(C1 == C2)
C1 = intersection matrix {{0,1,2,3},{1,2,3,0},{2,3,0,1}};
C2 = intersection matrix {{7,-5,1,1},{0,1,4,-3},{0,9,-6,1},{0,-3,2,9},{-7,5,-1,-1}};
C1 = dualCone C1;
assert(C1 == C2)
///

-- Test 13
-- Checking faces and minkSummandCone
TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
F1 = faces(1,P);
F2 = {convexHull matrix{{-1,0},{1,1}},convexHull matrix{{0,1},{1,0}},convexHull matrix{{1,1},{0,-1}},convexHull matrix{{1,0},{-1,-1}},convexHull matrix{{0,-1},{-1,0}},convexHull matrix{{-1,-1},{0,1}}};
assert(set F1 === set F2)
(C,L,M) = minkSummandCone(P);
assert(rays(C)*M == matrix{{1_QQ,1},{1,1},{1,1},{1,1},{1,1},{1,1}})
L1 = {convexHull matrix{{0,1},{0,0}},convexHull matrix{{0,0},{0,1}},convexHull matrix{{0,1},{0,-1}},convexHull matrix{{0,0,1},{0,1,0}},convexHull matrix{{0,1,1},{0,0,-1}}};
assert(set values L === set L1)
///

-- Test 14
-- Checking bipyramid, faces and fVector
TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
P = bipyramid P;
F1 = set apply(faces(3,P), f -> vertices f);
F2 = set {matrix{{-1_QQ},{0},{0}},matrix{{1_QQ},{0},{0}},matrix{{0_QQ},{1},{0}},matrix{{0_QQ},{-1},{0}},matrix{{1_QQ},{-1},{0}},matrix{{-1_QQ},{1},{0}},matrix{{0_QQ},{0},{1}},matrix{{0_QQ},{0},{-1}}};
assert(isSubset(F1,F2) and isSubset(F2,F1))
assert(fVector P == {8,18,12,1})
///

-- Test 15
-- Checking isEmpty
TEST ///
P = intersection(matrix{{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{0},{0},{0}});
assert not isEmpty P
P = intersection {P,(matrix{{-1,-1,-1}},matrix{{-2}})};
assert isEmpty P
///

-- Test 16
-- Checking isPointed
TEST ///
C = posHull matrix {{1,1,1,1},{1,-1,1,-1},{1,1,-1,-1}};
assert isPointed C
C = posHull {C,matrix{{-1},{0},{-1}}};
assert not isPointed C
///

-- Test 17
-- Checking isSmooth
TEST ///
C = posHull matrix {{1,0,0},{-1,2,3},{1,1,2}};
assert isSmooth C
C = posHull {C,matrix{{1},{0},{2}}};
assert not isSmooth C
C = posHull matrix {{1,2},{2,1},{1,2}};
assert not isSmooth C
C = posHull matrix {{1,1,-1,-1},{1,2,1,-1},{1,3,0,-1}};
assert isSmooth C
C = posHull {C,matrix{{1},{0},{1}}};
assert isSmooth C
///

-- Test 18
-- Checking is Face
TEST ///
C1 = posHull matrix {{1,1,1,1},{1,-1,0,0},{0,0,1,-1}};
C2 = posHull matrix {{1,1},{1,-1},{0,0}};
assert not isFace(C2,C1)
C2 = posHull matrix {{1},{1},{1}};
assert not isFace(C2,C1)
C2 = posHull matrix {{1},{0},{-1}};
assert isFace(C2,C1)
C2 = posHull matrix {{0},{0},{0}};
assert isFace(C2,C1)
///

-- Test 19
-- Checking isFace
TEST ///
P1 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
P2 = intersection(matrix {{1,0,0},{-1,0,0}},matrix {{-1},{-1}});
assert isFace(P2,P1)
P2 = convexHull matrix {{1,1,1},{1,1,-1},{1,-1,1}};
assert not isFace(P2,P1)
P2 = intersection {P2,{matrix{{0,1,0}},matrix{{1}}}};
assert isFace(P2,P1)
///

-- Test 20
-- Checking isCompact
TEST ///
P = intersection(matrix {{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1}},matrix {{1},{2},{3},{4},{5}});
assert not isCompact P
P = intersection {P, (matrix {{0,0,-1}},matrix {{6}})};
assert isCompact P
P = intersection {P, {matrix {{1,1,1}},matrix {{0}}}};
assert isCompact P
///

-- Test 21
-- Checking tailCone
TEST ///
P = intersection(matrix {{1,0},{-1,0},{0,1}},matrix {{1},{2},{3}});
C = posHull matrix {{0},{-1}};
assert(tailCone P == C)
P = intersection (matrix{{2,1,1},{1,2,1},{1,1,2}},matrix{{2},{2},{2}});
C = posHull matrix{{1,1,-3},{1,-3,1},{-3,1,1}};
assert(tailCone P == C)
///

-- Test 22
-- Checking smallestFace for polyhedra
TEST ///
P = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
F1 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};
F2 = convexHull matrix {{1,1},{1,1},{-1,1}};
assert(smallestFace(matrix{{0},{0},{0}},P) == P)
assert(smallestFace(matrix{{1/2},{1/3},{1}},P) == F1)
assert(smallestFace(matrix{{1},{1},{3/4}},P) == F2)
///

-- Test 23
-- Checking smallestFace for cones
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
F1 = posHull matrix {{1,0},{0,1},{0,0}};
F2 = posHull matrix {{0},{0},{1}};
assert(smallestFace(matrix{{1},{2},{3}},C) == C)
assert(smallestFace(matrix{{2},{3},{0}},C) == F1)
assert(smallestFace(matrix{{0},{0},{5}},C) == F2)
///

-- Test 24
-- Checking inInterior for polyhedra and cones
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
assert not inInterior(matrix{{2},{1}},P)
assert not inInterior(matrix{{1},{0}},P)
assert inInterior(matrix{{0},{0}},P)
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
assert not inInterior(matrix{{0},{0},{0}},C)
assert not inInterior(matrix{{-1},{0},{0}},C)
assert inInterior(matrix{{1},{2},{3}},C)
///

-- Test 25
-- Checking interiorPoint
TEST ///
P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
p = matrix {{0_QQ},{0}};
assert(interiorPoint P == p)
///

-- Test 26
-- Checking interiorVector
TEST ///
C = posHull matrix {{1,2,3},{2,3,1},{3,1,2}};
p = matrix {{1},{1},{1}};
assert(interiorVector C == p)
///

-- Test 27
-- Checking commonFace for polyhedra
TEST ///
P1 = convexHull matrix {{1,1,1,1,-1},{1,1,-1,-1,0},{1,-1,1,-1,0}};
P2 = intersection (matrix {{-1,0,0},{0,1,0},{0,-1,0},{0,0,1}},matrix {{-1},{1},{1},{1}});
assert not commonFace(P1,P2)
P2 = intersection {P2,(matrix {{0,0,-1}},matrix {{1}})};
assert commonFace(P1,P2)
///

-- Test 28
-- Checking commonFace for cones
TEST ///
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,1},{1,0}};
assert not commonFace(C1,C2)
C1 = posHull matrix {{1,1},{2,1}};
assert commonFace(C1,C2)
///

-- Test 29
-- Checking areCompatible
TEST ///
C1 = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C2 = posHull matrix {{1,1,0},{1,0,1},{0,-1,-1}};
assert not (areCompatible(C1,C2))#0
C2 = posHull {matrix {{1,0},{0,1},{0,0}}, C2};
assert (areCompatible(C1,C2))#0
///

-- Test 30
-- Checking fan and Fan basics
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
F = fan C;
assert(F#"generatingCones" === set {C})
assert(F#"ambient dimension" == 3)
assert(F#"top dimension of the cones" == 3)
assert(F#"number of generating cones" == 1)
assert not isComplete F
///

-- Test 31
-- Checking fan and addCone
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C1 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};
C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
C3 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}};
F = fan {C,C1,C2,C3};
assert(F#"generatingCones" === set {C,C1,C2,C3})
assert(F#"ambient dimension" == 3)
assert(F#"number of generating cones" == 4)
assert(F#"isPure")
L = set {matrix {{1},{0},{0}},matrix {{-1},{0},{0}},matrix {{0},{1},{0}},matrix {{0},{-1},{0}},matrix {{0},{0},{1}},matrix {{0},{0},{-1}}};
assert(L === F#"rays")
C = posHull matrix {{-1,0},{0,1},{0,0}};
F1 = addCone(C,F);
assert(F == F1)
///

-- Test 32
-- Checking fan, skeleton, isComplete, isPure, addCone, isPolytopal, polytope
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C1 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};
C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
C3 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}};
F = fan {C,C1,C2,C3};
L = {posHull matrix {{1,0},{0,-1},{0,0}},posHull matrix {{0,0},{0,-1},{1,0}},posHull matrix {{-1,0},{0,1},{0,0}},posHull matrix {{-1,0},{0,0},{0,1}},posHull matrix {{1,0},{0,0},{0,-1}},posHull matrix {{0,0},{1,0},{0,-1}},posHull matrix {{1,0},{0,1},{0,0}},posHull matrix {{1,0},{0,0},{0,1}},posHull matrix {{0,0},{1,0},{0,1}}};
assert(set L === set cones(2,F))
F1 = fan {posHull matrix {{1},{0},{0}},posHull matrix {{-1},{0},{0}},posHull matrix {{0},{1},{0}},posHull matrix {{0},{-1},{0}},posHull matrix {{0},{0},{1}},posHull matrix {{0},{0},{-1}}};
assert(skeleton(1,F) == F1)
assert not isComplete F
assert isPure F
C = posHull matrix {{-1,0,0},{0,-1,0},{0,0,-1}};
C1 = posHull matrix {{-1,0,0},{0,1,0},{0,0,-1}};
C2 = posHull matrix {{1,0,0},{0,-1,0},{0,0,-1}};
C3 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,1}};
F = addCone({C,C1,C2,C3},F);
assert(F#"number of generating cones" == 8)
assert isPure F
assert isComplete F
assert isSmooth F
assert isPolytopal F
assert(normalFan polytope F == F)
///

-- Test 33
-- Checking isSmooth and smoothSubfan
TEST ///
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,0},{2,1}};
C3 = posHull matrix {{1,2},{0,1}};
F = fan {C1,C2,C3};
assert not isSmooth F
F1 = fan {C2,C3};
assert(smoothSubfan F == F1)
///

-- Test 34
-- Checking normalFan
TEST ///
P = convexHull matrix {{1,0,0},{0,1,0}};
F = normalFan P;
L = {posHull matrix {{1,0},{0,1}},posHull matrix {{1,-1},{0,-1}},posHull matrix {{0,-1},{1,-1}}};
assert(F == fan L)
P =  convexHull (matrix {{1,0,0},{0,1,0}},matrix {{1},{1}});
F = normalFan P;
L = {posHull matrix {{1,0},{0,1}},posHull matrix {{1,1},{0,-1}},posHull matrix {{0,-1},{1,1}}};
assert(F == fan L)
///

-- Test 35
--Checking ccRefinement
TEST ///
M = matrix {{1,-1,0,0},{0,0,1,-1},{1,1,1,1}};
F = ccRefinement M;
F1 = fan {posHull matrix {{1,0,0},{0,1,0},{1,1,1}},posHull matrix {{-1,0,0},{0,1,0},{1,1,1}},posHull matrix {{-1,0,0},{0,-1,0},{1,1,1}},posHull matrix {{1,0,0},{0,-1,0},{1,1,1}}};
assert(F == F1)
///

-- Test 36
-- Checking imageFan
TEST ///
C = posHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};
F = imageFan(matrix {{1,0,0},{0,1,0}},C);
F1 = fan {posHull matrix {{1,1},{1,-1}},posHull matrix {{1,-1},{1,1}},posHull matrix {{-1,-1},{1,-1}},posHull matrix {{1,-1},{-1,-1}}};
assert(F == F1)
F = imageFan(matrix {{1,2,0},{0,0,1}},C);
F1 = fan {posHull matrix {{-3,-1},{1,1}},posHull matrix {{-1,1},{1,1}},posHull matrix {{1,3},{1,1}}};
assert(F == F1)
///

-- Test 37
-- Checking hilbertBasis
TEST ///
C = posHull matrix {{1,2},{2,1}};
H = hilbertBasis C;
L = {matrix {{1},{1}},matrix {{2},{1}},matrix {{1},{2}}};
assert(set H === set L)
C = posHull matrix {{1,1,0},{0,3,0},{0,0,1}};
H = hilbertBasis C;
L = {matrix {{1},{0},{0}},matrix {{1},{1},{0}},matrix {{1},{2},{0}},matrix {{1},{3},{0}},matrix {{0},{0},{1}}};
assert(set H === set L)
///

-- Test 38
-- Checking latticePoints
TEST ///
P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
LP = latticePoints P;
LP1 = {matrix {{1},{0}},matrix {{-1},{0}},matrix {{0},{1}},matrix {{0},{-1}},matrix {{0},{0}}};
assert(set LP === set LP1)
P = intersection(matrix {{-6,0,0},{0,-6,0},{0,0,-6},{1,1,1}},matrix{{-1},{-1},{-1},{1}});
assert(latticePoints P == {})
///

-- Test 39
-- Checking minkowskiSum
TEST ///
P1 = convexHull matrix {{1,0,0},{0,1,0}};
P2 = convexHull matrix {{-1,0,0},{0,-1,0}};
P1 = minkowskiSum(P1,P2);
P2 = convexHull matrix {{1,1,0,0,-1,-1},{-1,0,-1,1,0,1}};
assert(P1 == P2)
P1 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{0,0,0,0}};
P2 = convexHull matrix {{0,0},{0,0},{1,-1}};
P1 = minkowskiSum(P1,P2);
P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
assert(P1 == P2)
///

-- Test 40
-- Checking directProduct
TEST ///
P1 = convexHull matrix {{1,-1}};
P2 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
P1 = directProduct(P1,P2);
P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
assert(P1 == P2)
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,0},{0,1}};
C1 = directProduct(C1,C2);
C2 = posHull matrix {{1,2,0,0},{2,1,0,0},{0,0,1,0},{0,0,0,1}};
assert(C1 == C2)
F1 = normalFan hypercube 1;
F2 = normalFan hypercube 2;
F3 = normalFan hypercube 3;
assert(F3 == directProduct(F1,F2))
///

-- Test 41
-- Checking affineImage for polyhedra
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
A = matrix {{1,2},{3,4}};
v = matrix {{-1},{1}};
P = affineImage(A,P,v);
Q = convexHull matrix {{2,-2,0,-4},{8,0,2,-6}};
assert(P == Q)
P = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{1},{1}});
A = matrix {{0,2,0},{1,0,1},{0,0,2}};
v = matrix {{1},{1},{1}};
P = affineImage(A,P,v);
Q = convexHull(matrix{{-1},{-1},{-1}},matrix{{0,2,0},{1,0,1},{0,0,2}});
assert(P == Q)
///

-- Test 42
-- Checking affineImage for cones
TEST ///
C = posHull matrix {{1,1,2},{1,2,1},{2,1,1}};
A = matrix {{1,-1,0},{0,1,-1},{-1,0,1}};
C = affineImage(A,C);
C1 = posHull matrix {{0,-1,1},{-1,1,0},{1,0,-1}};
assert(C == C1)
///

-- Test 43
-- Checking affinePreimage for polyhedra
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
A = matrix {{1,2},{3,4}};
v = matrix {{-1},{1}};
P = affinePreimage(A,P,v);
Q = convexHull matrix {{0,-2,-4,-6},{0,1,3,4}};
assert(P == Q)
P = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{1},{1}});
A = matrix {{0,2,0},{1,0,1},{0,0,2}};
v = matrix {{1},{1},{1}};
P = affinePreimage(A,P,v);
Q = convexHull(matrix{{-1},{-1},{-1}},matrix{{1,0,-1},{0,1,0},{0,0,1}});
assert(P == Q)
///

-- Test 44
-- Checking affinePreimage for cones
TEST ///
C = posHull matrix {{1,1,2},{1,2,1},{2,1,1}};
A = matrix {{1,-1,0},{0,1,-1},{-1,0,0}};
C = affinePreimage(A,C);
C1 = posHull matrix {{-2,-1,-1},{-3,-3,-2},{-4,-4,-4}};
assert(C == C1)
///

-- Test 45
-- Checking pyramid
TEST ///
P = intersection(matrix {{1,0},{-1,0},{0,1},{0,-1}},matrix {{1},{1},{1},{1}});
P = pyramid P;
Q = convexHull matrix {{1,1,-1,-1,0},{1,-1,1,-1,0},{0,0,0,0,1}};
assert(P == Q)
///

-- Test 46
-- Checking crossPolytope
TEST ///
P = crossPolytope(3,2);
Q = convexHull matrix {{2,-2,0,0,0,0},{0,0,2,-2,0,0},{0,0,0,0,2,-2}};
assert(P == Q)
///

-- Test 47
-- Checking cyclicPolytope
TEST ///
P = cyclicPolytope(3,5);
Q = convexHull matrix {{0,1,2,3,4},{0,1,4,9,16},{0,1,8,27,64}};
assert(P == Q)
///

-- Test 48
-- Checking emptyPolyhedron
TEST ///
P = emptyPolyhedron 2;
assert(dim P == -1)
assert(ambDim P == 2)
///

-- Test 49
-- Checking hypercube
TEST ///
P = hypercube (3,3);
Q = convexHull matrix {{3,3,3,3,-3,-3,-3,-3},{3,3,-3,-3,3,3,-3,-3},{3,-3,3,-3,3,-3,3,-3}};
assert(P == Q)
///

-- Test 50
-- Checking hirzebruch
TEST ///
F = hirzebruch 3;
F1 = fan {posHull matrix{{1,0},{0,1}},posHull matrix{{1,0},{0,-1}},posHull matrix{{0,-1},{1,3}},posHull matrix{{0,-1},{-1,3}}};
assert(F == F1)
///

-- Test 51
-- Checking newtonPolytope
TEST ///
R = QQ[a,b,c];
f = a^2*b+b^3*c^2+c^4*a^3+a*b*c+a^5*c^6;
P =newtonPolytope f;
Q = convexHull matrix {{2,0,3,1,5},{1,3,0,1,0},{0,2,4,1,6}};
assert(P == Q)
///

-- Test 52
-- Checking posOrthant
TEST ///
C1 = posOrthant 3;
C2 = intersection matrix {{1,0,0},{0,1,0},{0,0,1}};
assert(C1 == C2)
///

-- Test 53
-- Checking statePolytope
TEST ///
R = QQ[a,b,c];
I = ideal(a^2-b,a*b-c);
(L,P) = statePolytope I;
Q = convexHull matrix {{21,3,1,1,6,2},{0,9,7,4,0,2},{0,0,2,4,5,5}};
L1 = { {{b^2,a*b,a^2}}, {{a*c,a*b,a^2,b^3}}, {{b,a^3}}, {{c^2,a*b,a*c,a^2}}, {{c,b}}, {{a^2,c}}};
L = apply(L,entries);
assert(P == Q)
assert(set L === set L1)
///

-- Test 54
-- Checking stdSimplex
TEST ///
P = stdSimplex 2;
Q = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{0},{0},{0}},matrix{{1,1,1}},matrix{{1}});
assert(P == Q)
///

-- Test 55
-- Checking equality of polyhedral objects
TEST ///
L1 = set {posOrthant 3, hypercube 2, crossPolytope 4, hirzebruch 5};
L2 = set {hirzebruch 5, posOrthant 3, hypercube 2, crossPolytope 4};
assert(L1 === L2)
///

-- Test 56
-- Checking vertexEdgeMatrix and vertexFacetMatrix
TEST ///
P = convexHull matrix {{0,-1,1,-1,1},{0,-1,-1,1,1},{-1,1,1,1,1}};
M = matrix {{0,1,2,3,4,5,6,7,8},{1,1,1,0,1,1,0,0,0},{2,1,0,1,0,0,0,1,0},{3,0,0,0,1,0,1,1,0},{4,0,1,1,0,0,0,0,1},{5,0,0,0,0,1,1,0,1}};
N = matrix {{0,1,2,3,4,5},{1,1,1,1,1,0},{2,1,0,1,0,1},{3,0,1,1,0,1},{4,1,0,0,1,1},{5,0,1,0,1,1}};
assert(vertexEdgeMatrix P == M)
assert(vertexFacetMatrix P == N)
///

-- Test 57
-- Checking minFace and maxFace
TEST ///
P = hypercube 3;
w = matrix {{1},{2},{1}};
F1 = convexHull matrix {{1},{1},{1}};
F2 = convexHull matrix {{-1},{-1},{-1}};
assert(F1 == maxFace(w,P))
assert(F2 == minFace(w,P))
C = posHull matrix {{2,-1,1},{-1,1,1},{0,-1,1}};
C1 = posHull matrix {{-1,2},{1,-1},{-1,0}};
assert(C1 == minFace(w,C))
///

-- Test 58
-- Checking proximum
TEST ///
P = crossPolytope 3;
p = matrix {{1},{2},{3}};
q = matrix {{0_QQ},{1},{0}};
assert(q == proximum(p,P))
p = matrix {{1},{1/2},{1}};
q = matrix {{1/2},{0},{1/2}};
assert(q == proximum(p,P))
P = convexHull map(QQ^3,QQ^3,1);
p = matrix {{2},{2},{0}};
q = matrix {{1/2},{1/2},{0}};
assert(q == proximum(p,P))
///

-- Test 59
-- Checking triangulate
TEST ///
P = crossPolytope 3;
L = triangulate P;
L = apply(L,convexHull);
L1 = {convexHull{matrix{{1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}}};
assert(set L === set L1)
///

-- Test 60
-- Checking volume
TEST ///
P = hypercube 3;
assert(volume P == 8)
P = crossPolytope 3;
assert(volume P == 4/3)
///

-- Test 61
-- Checking incompCones
TEST ///
L = {posHull matrix{{1,0},{1,1}},posHull matrix{{1,0},{0,-1}},posHull matrix{{-1,0},{0,1}},posHull matrix{{1,1},{0,1}},posHull matrix {{1,2},{2,1}}};
assert(set incompCones L === set {(L#0,L#4),(L#3,L#4)})
L = L_{0..3}|{hirzebruch 3};
assert(incompCones L == {(L#0,L#4),(L#2,L#4),(L#3,L#4)})
assert(set incompCones(L#2,L#4) === set {(L#2,posHull matrix {{0,-1},{1,3}}),(L#2,posHull matrix {{0,-1},{-1,3}})})
L = {posHull matrix {{-1,0},{0,1}},posHull matrix {{-1,0},{0,-1}},posHull matrix {{0,-1},{-1,3}},posHull matrix {{0,-1},{1,3}}};
L = {(L#0,L#2),(L#0,L#3),(L#1,L#2)};
assert(set incompCones(normalFan hypercube 2,hirzebruch 3) === set L)
///

-- Test 62
-- Checking isNormal for Cones
TEST ///
P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}};
Q = hypercube 2;
assert not isNormal P
assert isNormal Q
///

-- Test 63
-- Checking sublatticeBasis and toSublattice
TEST ///

-- new answer:
assert((sublatticeBasis matrix{{2,4,2,4},{1,2,2,3}}) === matrix({{2, 4}, {2, 3}}) );
-- assert(sublatticeBasis matrix{{2,4,2,4},{1,2,2,3}} == matrix {{2,2},{1,2}})

-- new answer:
assert( (sublatticeBasis convexHull matrix {{1,2,2},{0,-1,2}}) === map(ZZ^2,ZZ^2,{{0, 1}, {-1, 2}}) );
-- assert(sublatticeBasis convexHull matrix {{1,2,2},{0,-1,2}} == matrix {{-1,1},{1,0}})

assert(toSublattice convexHull matrix {{2,0},{0,3}} == convexHull matrix {{0,1}})
///

-- Test 64
-- Checking Scaling
TEST ///
assert(3/2 * hypercube(2,2) == hypercube(2,3))
///

-- Test 65
-- Checking ehrhart
TEST ///
P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{0,1,0}};
assert(ehrhart P == (1/2)*x^2+(3/2)*x+1)
///

-- Test 66
-- Checking isLatticePolytope
TEST ///
P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{1},{1},{1},{1}})
assert not isLatticePolytope P
P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{4},{6},{3},{6}})
assert isLatticePolytope P
///

-- Test 67
-- Checking isVeryAmple
TEST ///
P = convexHull matrix {{0,1,0,0,1,0,1,2,0,0},{0,0,1,0,1,0,2,2,0,-1},{0,0,0,1,2,0,1,2,0,-1},{0,0,0,0,-1,1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,1,1}}
assert not isNormal P
assert isVeryAmple P
///

-- Test 68
-- Checking minkowskiSum for higher dimensions
TEST ///
p = convexHull transpose matrix {{1,0,0,0,0}}

p1 = convexHull transpose matrix {{0,0,0,0,0},{0,-1,0,0,0}}
p2 = convexHull transpose matrix {{0,0,0,0,0},{0,0,-1,0,0}}
p3 = convexHull transpose matrix {{0,0,0,0,0},{0,0,0,-1,0}}
p4 = convexHull transpose matrix {{0,0,0,0,0},{0,0,0,0,-1}}

r1 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,-1,0,0}}
r2 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,0,-1,0}}
r3 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,0,0,-1}}
p = minkowskiSum(p,p1)
assert (numColumns vertices p == 2)
p = minkowskiSum(p,p2)
assert (numColumns vertices p == 4)
p = minkowskiSum(p,p3)
assert (numColumns vertices p == 8)
p = minkowskiSum(p,p4)
assert (numColumns vertices p == 16)
p = minkowskiSum(p,r1)
assert (numColumns vertices p == 32)
p = minkowskiSum(p,r2)
assert (numColumns vertices p == 56)
p = minkowskiSum(p,r3)
assert (numColumns vertices p == 92)
///

end
