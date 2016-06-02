-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron

compute#Polyhedron = new MutableHashTable

vertices Polyhedron := P -> P#"vertices"

-- Modifying the standard output for a polyhedron to give an overview of its characteristica
-- net Polyhedron := P -> ()

-- horizontalJoin flatten (
-- 	  "{",
-- 	  -- prints the parts vertically
-- 	  stack (horizontalJoin \ sort apply({"ambient dimension", 
-- 			                      "dimension",
-- 					      "dimension of lineality space",
-- 					      "number of rays",
-- 					      "number of vertices", 
-- 					      "number of facets"}, key -> (net key, " => ", net P#key))),
-- 	  "}" ))
--   
  
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

Polyhedron == Polyhedron := (P,Q) -> P === Q


-- PURPOSE : Tests if a Polyhedron is empty
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isEmpty = method(TypicalValue => Boolean)
isEmpty Polyhedron := P -> dim P == -1


-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact Polyhedron := P -> linSpace(P) == 0 and rays(P) == 0



-- PURPOSE : Computing the lattice points of a compact Polyhedron 
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the lattice points of 'P'
latticePoints = method(TypicalValue => List)
latticePoints Polyhedron := P -> (
     if not P.cache.?latticePoints then (
	  -- Checking for input errors
	  if  not isCompact P then error("The polyhedron must be compact");
	  -- Recursive function that intersects the polyhedron with paralell hyperplanes in the axis direction
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

-- PURPOSE : Computing the interior lattice points of a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the interior lattice points
interiorLatticePoints = method(TypicalValue => List)
interiorLatticePoints Polyhedron := (cacheValue symbol interiorLatticePoints)(P -> (
     L := latticePoints P;
     select(L,e -> inInterior(e,P))))




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


fVector Polyhedron := P -> apply(dim P + 1, d -> #faces(dim P - d,P))


--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'P'  plus one a polyhedron
--  OUTPUT : a List, containing the faces as polyhedra
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

--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isReflexive = method(TypicalValue => Boolean)
isReflexive Polyhedron := (cacheValue symbol isReflexive)(P -> isLatticePolytope P and inInterior(matrix toList(ambDim P:{0}),P) and isLatticePolytope polar P)


-- PURPOSE : Checks if a lattice polytope is reflexive


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
	   		      	   "dimension" => numRows Ev,
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
     (hyperplanesTmp,w) := hyperplanes P;
     originalFacets := apply(numRows HS, i -> intersection(HS,v, hyperplanesTmp || HS^{i}, w || v^{i}));
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
	       
-- PURPOSE : Computing a point in the relative interior of a Polyhedron 
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'p',  a point given as a matrix
interiorPoint = method(TypicalValue => Matrix)
interiorPoint Polyhedron := P -> (
     -- Checking for input errors
     if isEmpty P then error("The polyhedron must not be empty");
     Vm := vertices P | promote(rays P,QQ);
     n := numColumns Vm;
     ones := matrix toList(n:{1/n});
     -- Take the '1/n' weighted sum of the vertices
     Vm * ones)


-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its minimum
minFace (Matrix,Polyhedron) := (v,P) -> (
     -- Checking for input errors
     if numColumns v =!= 1 or numRows v =!= ambDim(P) then error("The vector must lie in the same space as the polyhedron");
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


-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its maximum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its maximum
maxFace (Matrix,Polyhedron) := (v,P) -> minFace(-v,P)


--   INPUT : '(p,P)',  where 'p' is a point given by a matrix and 'P' is a Polyhedron
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Polyhedron) := (p,P) -> (
     hyperplanesTmp := hyperplanes P;
     hyperplanesTmp = (hyperplanesTmp#0 * p)-hyperplanesTmp#1;
     all(flatten entries hyperplanesTmp, e -> e == 0) and (
	  HS := halfspaces P;
	  HS = (HS#0 * p)-HS#1;
	  all(flatten entries HS, e -> e < 0)))


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


--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
isFace(Polyhedron,Polyhedron) := (P,Q) -> (
     -- Checking if the two polyhedra lie in the same space and computing the dimension difference
     c := dim Q - dim P;
     if ambDim(P) == ambDim(Q) and c >= 0 then (
	  -- Checking if P is the empty polyhedron
	  if c > dim Q then true
	  -- Checking if one of the codim 'c' faces of Q is P
	  else any(faces(c,Q), f -> f === P))
     else false)



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
   -- PURPOSE : Computing the polar of a given polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A Polyhedron, the set { v | v*p<=1 forall p in P}
polar = method(TypicalValue => Polyhedron)
polar Polyhedron := (cacheValue symbol polar)(P -> (
     d := ambDim(P);
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
    

-- PURPOSE : Scaling respectively the multiple Minkowski sum of a polyhedron
--   INPUT : '(k,P)',  where 'k' is a strictly positive rational or integer number and 
--     	    	             'P' is a Polyhedron
--  OUTPUT : The polyehdron 'P' scaled by 'k'
QQ * Polyhedron := (k,P) -> (
     -- Checking for input errors
     if k <= 0 then error("The factor must be strictly positiv");
     convexHull(k*(vertices P),rays P | linSpace P))

ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P

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
 
 -- PURPOSE : Computing the smallest face of 'P' containing 'p'
--   INPUT : '(p,P)',  where 'p' is a point given as a matrix and
--     	    	       'P' is a polyhedron
--  OUTPUT : The smallest face containing 'p' as a polyhedron
smallestFace = method()
smallestFace(Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= ambDim(P) then error("The point must lie in the same space");
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
     else emptyPolyhedron ambDim(P))


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
			 hyperplanesTmp := halfspaces(Qsums#k + Qlist#(k+i+1));
			 hyperplanesTmp = for j from 0 to numRows(hyperplanesTmp#0)-1 list if (hyperplanesTmp#0)_(j,n) < 0 then ((hyperplanesTmp#0)^{j},(hyperplanesTmp#1)^{j}) else continue;
			 returnE := select(Elocal#i, e -> (
				   p := (sum apply(edgeTuple|{e1}, et -> et#1 * center)) + (e#1 * center);
				   any(hyperplanesTmp, pair -> (pair#0)*p - pair#1 == 0)));
			 --if returnE == {} then break{};
			 returnE);
		    mV = selectRecursion(Elocal#0,edgeTuple|{e1},drop(Elocal,1),mV,Qsums,Qlist,k+1)));
	  mV);
     selectRecursion(E1,edgeTuple,Elist,mV,Qsums,Qlist,k))
