-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron

compute#Polyhedron = new MutableHashTable

Polyhedron == Polyhedron := (P1,P2) -> (
   contains(P1, P2) and contains(P2, P1)
)


polyhedron = method()
polyhedron HashTable := inputProperties -> (
   constructTypeFromHash(Polyhedron, inputProperties)
)



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
   if numgens target Mvert =!= numgens target Mlineality then error ("points and lineality generators must lie in the same space");
   result := new HashTable from {
      ambientDimension => numRows Mvert,
      points => Mvert,
      inputRays => Mrays,
      inputLinealityGenerators => Mlineality
   };
   polyhedron result
)

convexHull(Matrix,Matrix) := (Mvert,Mrays) -> (
   r := ring Mvert;
	Mlineality := map(target Mvert,r^0,0);
   convexHull(Mvert, Mrays, Mlineality)
)


--   INPUT : 'M'  a Matrix containing the generating points as column vectors
convexHull Matrix := Mvert -> (
   r := ring Mvert;
	Mrays := map(target Mvert,r^0,0);
	convexHull(Mvert, Mrays)
)


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
   C1 := getProperty(P1, underlyingCone);
   C2 := getProperty(P2, underlyingCone);
   result := new HashTable from {
      ambientDimension => ambDim P1,
      underlyingCone => posHull(C1, C2)
   };
   polyhedron result;
)
   
--   INPUT : 'L',   a list of Cones, Polyhedra, vertices given by M, 
--     	    	    and (vertices,rays) given by '(V,R)'
convexHull List := L -> (
   polyhedra := apply(L,
      l -> (
         if not instance(l, Polyhedron) then convexHull l
         else l
      )
   );
   cones := apply(polyhedra, p -> getProperty(p, underlyingCone));
   underLyingResult := posHull(cones);
   result := new HashTable from {
      underlyingCone => underLyingResult
   };
   polyhedron result
)
