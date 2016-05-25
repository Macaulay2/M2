-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone

Cone == Cone := (C1,C2) -> C1 === C2

-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( )
-- 	  "{",
-- 	  -- prints the parts vertically
-- 	  stack (horizontalJoin \ sort apply({"ambient dimension", 
-- 			                      "dimension",
-- 					      "dimension of lineality space",
-- 					      "number of rays",
-- 					      "number of facets"}, key -> (net key, " => ", net C#key))),
-- 	  "}" ))
-- 

-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed Cone := C -> rank linSpace(C) == 0


--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isSmooth Cone := C -> (
     -- generating the non-linealityspace cone of C
     R := lift(transpose rays C,ZZ);
     n := dim C - C#"dimension of lineality space";
     -- if the cone is full dimensional then it is smooth iff its rays form a basis over ZZ
     numRows R == n and (M := (smithNormalForm R)#0; product apply(n, i -> M_(i,i)) == 1))
	   

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


-- PURPOSE : Building the Cone 'C'
--   INPUT : '(genrays,dualgens)',  a pair of two matrices each describing the cone C
--                                	directly  as generating rays ('genrays') and in the 
--						dual description as intersection of half-spaces through 
--						the origin ('dualgens')
--  OUTPUT : The Cone 'C'
coneBuilder = (genrays,dualgens) -> (
      -- Sorting into rays, lineality space generators, supporting half-spaces, and hyperplanes
      << genrays << endl;
      RM := genrays#0;
      LS := genrays#1;
      HS := transpose(-dualgens#0);
      hyperplanesTmp := transpose(dualgens#1);
      -- Defining C
      result := new Cone from {
         "ambient dimension" => numgens target RM,
         "dimension" => (numgens target RM)-(rank hyperplanesTmp),
         "dimension of lineality space" => numgens source LS,
         "linealitySpace" => LS,
         "number of rays" => numgens source RM,
   --	   "rays" => RM,
         "number of facets" => numgens target HS,
         "halfspaces" => HS,
         "hyperplanes" => hyperplanesTmp,
         "genrays" => genrays,
         "dualgens" => dualgens,
         symbol cache => new CacheTable};
      result.cache.computedRays = RM;
      result
)

computeLinealityBasisForCone = method(TypicalValue => Matrix)
computeLinealityBasisForCone Cone := C -> (
   if C.cache.?inputRays and C.cache.?inputLinealityGenerators then computeLinealityBasisFromInputRays C
   else if C.cache.?inequalities and C.cache.?equations then computeLinealityBasisFromInequalities C
   else error "No method for ray computation."
)

computeLinealityBasisFromInputRays = method(TypicalValue => Matrix)
computeLinealityBasisFromInequalities = method(TypicalValue => Matrix)

rules#((set {inputRays, inputLinealityGenerators}, computedRays)) = method(TypicalValue => Matrix)
rules#((set {inputRays, inputLinealityGenerators}, computedRays)) Cone := C -> (
   inputRays := C.cache.inputRays;
   inputLinealityGenerators := C.cache.inputLinealityGenerators;
   dual := fourierMotzkin(inputRays, inputLinealityGenerators);
   (raySide, facetSide) := fMReplacement(inputRays, dual#0, dual#1);
   C.cache.computedLinealityBasis = raySide#1;
   C.cache.computedFacets = transpose( -facetSide#0);
   C.cache.computedLinealitySpace = transpose( -facetSide#1);
   raySide#0
)

rules#((set {inequalities, equations}, computedRays)) = method(TypicalValue => Matrix)
rules#((set {inequalities, equations}, computedRays)) Cone := C -> (
   inequalities := C.cache.inequalities;
   equations := C.cache.equations;
   dual := fourierMotzkin(inequalities, equations);
   C.cache.computedLinealityBasis = dual#1;
   dual#0
)

computeRaysForCone = method()
computeRaysForCone Cone := C -> (
   applyFittingRule(C, computedRays)
)


coneFromRays = method(TypicalValue => Cone)
coneFromRays(Matrix, Matrix) := (inputRays, linealityGenerators) -> (
     -- checking for input errors
     if numRows inputRays =!= numRows linealityGenerators then error("rays and linSpace generators must lie in the same space");
     result := new Cone from {
         "ambient dimension" => numRows inputRays,
         symbol cache => new CacheTable
     };
     result.cache.inputRays = inputRays;
     result.cache.inputLinealityGenerators = linealityGenerators;
     result
)

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
   coneFromRays(Mrays, LS)
)

--     Mrays = chkZZQQ(Mrays,"rays");
--     LS = chkZZQQ(LS,"lineality space");
--     -- Computing generators of the cone and its dual cone
--     dualgens := fourierMotzkin(Mrays,LS);
--     local genrays;
--     (genrays,dualgens) = fMReplacement(Mrays,dualgens#0,dualgens#1);
----     genrays := fourierMotzkin dualgens;
--     coneBuilder(genrays,dualgens))


--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
posHull Matrix := R -> (
     R = chkZZQQ(R,"rays");
     -- Generating the zero lineality space LS
     LS := map(target R,QQ^1,0);
     posHull(R,LS))


--   INPUT : '(C1,C2)'  two cones
posHull(Cone,Cone) := (C1,C2) -> (
	-- Checking for input errors
	if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same ambient space");
	-- Combining the rays and the lineality spaces into one matrix each
	R := rays(C1) | rays(C2);
	LS := linSpace(C1) | linSpace(C2);
	dualgens := fourierMotzkin(R,LS);
	local genrays;
	(genrays,dualgens) = fMReplacement(R,dualgens#0,dualgens#1);
--	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))




--   INPUT : 'C'  a Cone
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Cone := C -> C#"linealitySpace"


     
fVector Cone := C -> apply(dim C + 1, d -> #faces(dim C - d,C))


--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its maximum
maxFace (Matrix,Cone) := (v,C) -> minFace(-v,C)






-- PURPOSE : Computing the face of a Cone where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its minimum
minFace (Matrix,Cone) := (v,C) -> (
     -- Checking for input errors
     if numColumns v =!= 1 or numRows v =!= ambDim(C) then error("The vector must lie in the same space as the polyhedron");
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


--   INPUT : '(p,C)',  where 'p' is a point given by a matrix and 'C' is a Cone
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Cone) := (p,C) -> (
     hyperplanesTmp := hyperplanes C;
     all(flatten entries(hyperplanesTmp*p), e -> e == 0) and (
	  HS := halfspaces C;
	  all(flatten entries(HS*p), e -> e > 0)))

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
isFace(Cone,Cone) := (C1,C2) -> (
     c := dim C2 - dim C1;
     -- Checking if the two cones lie in the same space and the dimension difference is positive
     if ambDim(C1) == ambDim(C2) and c >= 0 then (
	  -- Checking if one of the codim 'c' faces of C2 is C1
	  any(faces(c,C2), f -> f === C1))
     else false)

     

-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
	genrays := (sort transpose halfspaces(C),sort transpose hyperplanes(C));
	dualgens := (sort (-(rays(C))),sort linSpace(C));
	coneBuilder(genrays,dualgens))
