compute#Cone#pointed = method()
compute#Cone#pointed Cone := C -> (
   rank linealitySpace C == 0
)


compute#Cone#fullDimensional = method()
compute#Cone#fullDimensional Cone := C -> (
   dim C == ambDim C
)
 

compute#Cone#smooth = method()
compute#Cone#smooth Cone := C -> (
   R := lift(transpose rays C,ZZ);
   L := lift(transpose linealitySpace C, ZZ);
   n := dim C - numRows L;
   -- The rays need to form a lattice basis in N/L
   SNF := (smithNormalForm (R || L))#0; 
   numRows R == n and (product apply(dim C, i -> SNF_(i,i)) == 1)
)


compute#Cone#nFacets = method()
compute#Cone#nFacets Cone := C -> (
   numRows facets C
)


compute#Cone#nRays = method()
compute#Cone#nRays Cone := C -> (
   numColumns rays C
)


compute#Cone#computedFacesThroughRays = method()
compute#Cone#computedFacesThroughRays Cone := C -> (
   result := new MutableHashTable;
   d := dim C;
   raysC := rays C;
   ldim := rank linealitySpace C;
   result#d = {toList (0..(getProperty(C, nRays) - 1))};
   result#(d-1) = getProperty(C, computeFacetsFromRayData);
   for i from 0 to d-2-ldim do (
      oldFaces := result#(d-1-i);
      newFaces := unique flatten apply(oldFaces,
         face -> toList apply(result#(d-1),
            facet -> (
               toSequence elements ((set face) * (set facet))
            )
         )
      );
      << newFaces << endl;
      newFaces = select(newFaces, face -> (rank raysC_face) + ldim == d-2-i);
      result#(d-2-i) = newFaces
   );
   result
)


compute#Cone#computedFacetsThroughRays = method()
compute#Cone#computedFacetsThroughRays Cone := C -> (
   raysC := rays C;
   facetsC := facets C;
   nFacetsC := getProperty(C, nFacets);
   nRaysC := getProperty(C, nRays);
   apply(0..(nFacetsC -1), 
      i -> (
         facet := facetsC^{i};
         positions(0..(nRaysC - 1), 
            j-> (
               ray := raysC_{j};
               eval := (flatten entries (facet * ray))#0;
               eval == 0
            )
         )
      )
   )
)


compute#Cone#computedRaysThroughFacets = method()
compute#Cone#computedRaysThroughFacets Cone := C -> (
   raysC := rays C;
   facetsC := facets C;
   nFacetsC := getProperty(C, nFacets);
   nRaysC := getProperty(C, nRays);
   apply(0..(nRaysC -1), 
      i -> (
         ray := raysC_{i};
         positions(0..(nFacetsC - 1), 
            j-> (
               facet := facetsC^{j};
               eval := (flatten entries (facet * ray))#0;
               eval == 0
            )
         )
      )
   )
)


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


compute#Cone#computedHilbertBasis = method()
compute#Cone#computedHilbertBasis Cone := C -> (
   local inputMatrix;
   if hasProperties(C, {computedFacets, computedHyperplanes}) then (
      inputMatrix = transpose facets C;
   ) else (
      inputMatrix = transpose facets C;
   );
   hb := transpose hilbertBasis(inputMatrix, InputType=>"lattice");
   r := ring inputMatrix;
   result := apply(0..(numColumns hb - 1), i -> (promote(matrix hb_i, r)) // (transpose inputMatrix));
   toList result
)


compute#Cone#computedLinealityBasis = method()
compute#Cone#computedLinealityBasis Cone := C -> (
   local containingSpace;
   if hasProperties(C, {computedFacets, computedHyperplanes}) then (
      containingSpace = (facets C) || (hyperplanes C);
   ) else if hasProperties(C, {inequalities, equations}) then (
      containingSpace = getProperty(C, inequalities) || getProperty(C, equations);
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      rays C;
      return linealitySpace C
   ) else (
      error "Lineality space not computable."
   );
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


compute#Cone#simplicial = method()
compute#Cone#simplicial Cone := C -> (
   (isPointed C) and (numColumns rays C == dim C)
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
