-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone
compute#Cone = new MutableHashTable

Cone == Cone := (C1,C2) -> C1 === C2


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




coneFromRayData = method(TypicalValue => Cone)
coneFromRayData(Matrix, Matrix) := (iRays, linealityGenerators) -> (
     -- checking for input errors
     if numRows iRays =!= numRows linealityGenerators then error("rays and linSpace generators must lie in the same space");
     result := new Cone from {
         ambientDimension => numRows iRays,
         symbol cache => new CacheTable
     };
     setProperty(result, computedRays, iRays);
     setProperty(result, computedLinealityBasis, linealityGenerators);
     result
)

coneFromFacetData = method(TypicalValue => Cone)
coneFromFacetData(Matrix, Matrix) := (ineq, eq) -> (
   if numColumns ineq =!= numColumns eq then error("facets and hyperplanes must lie in same space");
   result := new Cone from {
      ambientDimension => numColumns ineq,
      symbol cache => new CacheTable
   };
   setProperty(result, computedFacets, ineq);
   setProperty(result, computedHyperplanes, eq);
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
   if numRows Mrays =!= numRows LS then error("rays and linSpace generators must lie in the same space");
   result := new Cone from {
      ambientDimension => numRows Mrays,
      symbol cache => new CacheTable
   };
   setProperty(result, inputRays, Mrays);
   setProperty(result, inputLinealityGenerators, LS);
   result
)

--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
intersection Matrix := M -> (
   r := ring M;
   N := transpose map(source M, r^1, 0); 
   intersection(M, N)
)



--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
posHull Matrix := R -> (
   << "Hello." << endl;
   r := ring R;
   << ring << endl;
   -- Generating the zero lineality space LS
   LS := map(target R, r^1,0);
   posHull(R,LS)
)


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
