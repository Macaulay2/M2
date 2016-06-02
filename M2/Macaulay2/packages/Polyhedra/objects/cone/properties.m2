
-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed Cone := C -> (
   getProperty(C, pointed)
)

compute#Cone#pointed = method()
compute#Cone#pointed Cone := C -> (
   rank linealitySpace C == 0
)



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



compute#Cone#computedDimension = method(TypicalValue => ZZ)
compute#Cone#computedDimension Cone := C -> (
   if hasProperties(C, {computedRays, computedLinealityBasis}) then (
      return (rank rays C) + (numColumns linealitySpace C)
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      return rank (getProperty(C, inputRays) | getProperty(C, inputLinealityGenerators))
-- TODO: Add missing possibilities
   ) else (
      return (rank rays C) + (numColumns linealitySpace C)
   )
)

-- hilbertBasis = method()
hilbertBasis Cone := List => o -> (C -> (
      << C << endl;
      getProperty(C, computedHilbertBasis)
   )
)

compute#Cone#computedHilbertBasis = method()
compute#Cone#computedHilbertBasis Cone := C -> (
   local inputMatrix;
   if hasProperties(C, {computedFacets, computedHyperplanes}) then (
      inputMatrix = transpose facets C;
   ) else (
      inputMatrix = transpose facets C;
   );
   hb := transpose hilbertBasis(inputMatrix, InputType=>"lattice");
   << hb << endl;
   << target hb << endl;
   << target inputMatrix << endl;
   result := apply(0..(numcols hb - 1), i -> solve(transpose inputMatrix, promote(matrix hb_i, QQ)));
   toList apply(result, r-> lift(r, ZZ))
)


compute#Cone#computedLinealityBasis = method()
compute#Cone#computedLinealityBasis Cone := C -> (
   local containingSpace;
   if hasProperties(C, {computedFacets, computedHyperplanes}) then (
      containingSpace = (facets C) || (hyperplanes C);
   ) else if hasProperties(C, {inequalities, equations}) then (
      containingSpace = getProperty(C, inequalities) || getProperty(C, equations);
      << "CS: " << containingSpace << endl;
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      rays C;
      return linealitySpace C
   ) else (
      error "Lineality space not computable."
   );
   << "CS: " << containingSpace << endl;
   orthogonalComplement containingSpace
)

compute#Cone#computedHyperplanes = method()
compute#Cone#computedHyperplanes Cone := C -> (
   local containingSpace;
   if hasProperties(C, {computedRays, computedLinealityBasis}) then (
      containingSpace = rays C | linealitySpace C;
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      containingSpace = getProperty(C, inputRays) | getProperty(C, inputLinealityGenerators);
   ) else if hasProperties(C, {inequalities, equations}) then (
      facets C;
      return hyperplanes C
   ) else (
      error "Hyperplanes not computable"
   );
   result := orthogonalComplement transpose containingSpace;
   transpose result
)

compute#Cone#computedFacets = method()
compute#Cone#computedFacets Cone := C -> (
   (facetData, hyperplaneData) := (0,0);
   if hasProperties(C, {computedRays, computedLinealityBasis}) then (
      (facetData, hyperplaneData) = computeFacetsFromRayData(rays C, linealitySpace C);
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      (facetData, hyperplaneData) = computeFacetsFromRayData(getProperty(C, inputRays), getProperty(C, inputLinealityGenerators));
   ) else if hasProperties(C, {inequalities, equations}) then (
      (facetData, hyperplaneData) = computeFacetsFromRayData(rays C, linealitySpace C);
   ) else (
      error "Facets not computable."
   );
   if not hasProperty(C, computedHyperplanes) then setProperty(C, computedHyperplanes, hyperplaneData);
   facetData
)

compute#Cone#computedRays = method()
compute#Cone#computedRays Cone := C -> (
   local rayData;
   local linealityData;
   if hasProperties(C, {computedFacets, computedHyperplanes}) then (
      (rayData, linealityData) = computeRaysFromFacetData(facets C, hyperplanes C);
   ) else if hasProperties(C, {inequalities, equations}) then (
      (rayData, linealityData) = computeRaysFromFacetData(getProperty(C, inequalities), getProperty(C, equations));
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      (rayData, linealityData) = computeRaysFromFacetData(facets C, hyperplanes C);
   ) else (
      error "Rays not computable."
   );
   if not hasProperty(C, computedLinealityBasis) then setProperty(C, computedLinealityBasis, linealityData);
   rayData
)



-- Helper methods

orthogonalComplement = method()
orthogonalComplement Matrix := M -> (
   gens kernel M
)

computeRaysFromFacetData = method()
computeRaysFromFacetData(Matrix, Matrix) := (facetData, hyperplaneData) -> (
   fourierMotzkin(transpose(-facetData), transpose(hyperplaneData))
)

computeFacetsFromRayData = method()
computeFacetsFromRayData(Matrix, Matrix) := (rayData, linealityData) -> (
   (A, B) := fourierMotzkin(rayData, linealityData);
   (transpose(-A), transpose(B))
)


