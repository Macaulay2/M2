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
 