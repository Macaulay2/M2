-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone
compute#Cone = new MutableHashTable

Cone == Cone := (C1,C2) -> C1 === C2

-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( )
-- 	  "{",
-- 	  -- prints the parts vertically
-- 	  stack (horizontalJoin \ sort apply({ambientDimension, 
-- 			                      "dimension",
-- 					      "dimension of lineality space",
-- 					      "number of rays",
-- 					      "number of facets"}, key -> (net key, " => ", net C#key))),
-- 	  "}" ))
-- 



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
         ambientDimension => numgens target RM,
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



coneFromRays = method(TypicalValue => Cone)
coneFromRays(Matrix, Matrix) := (inputRays, linealityGenerators) -> (
     -- checking for input errors
     if numRows inputRays =!= numRows linealityGenerators then error("rays and linSpace generators must lie in the same space");
     result := new Cone from {
         ambientDimension => numRows inputRays,
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
