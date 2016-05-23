-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron


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
	     "hyperplanes" => hyperplanesTmp,
	     "homogenizedVertices" => verticesA,
	     "homogenizedHalfspaces" => hyperA,
	     symbol cache => new CacheTable})
     
--   INPUT : 'P'  a Polyhedron
posHull Polyhedron := P -> (
     Mrays := makePrimitiveMatrix P#"vertices" | P#"rays";
     Mlinspace := P#"linealitySpace";
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

Polyhedron == Polyhedron := (P,Q) -> P === Q


-- PURPOSE : Tests if a Polyhedron is empty
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isEmpty = method(TypicalValue => Boolean)
isEmpty Polyhedron := P -> P#"dimension of polyhedron" == -1


-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact Polyhedron := P -> P#"linealitySpace" == 0 and P#"rays" == 0



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