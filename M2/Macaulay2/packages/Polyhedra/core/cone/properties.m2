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
   spanSmoothCone(R, L, dim C)
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
   result#0 = {toList (0..(getProperty(C, nRays) - 1))};
   result#(1) = toList getProperty(C, computedFacetsThroughRayData);
   for i from 0 to d-2-ldim do (
      oldFaces := result#(1+i);
      newFaces := unique flatten apply(oldFaces,
         face -> toList apply(result#(1),
            facet -> (
               plop := elements ((set face) * (set facet));
               sort plop
            )
         )
      );
      -- << newFaces << endl;
      newFaces = select(newFaces, 
         face -> (
            -- << "Face is: " << face << endl;
            -- << raysC << endl;
            -- << raysC_{face} << endl;
            (rank raysC_(toList face)) + ldim == d-2-i
         )
      );
      -- << "Select ok." << endl;
      result#(2+i) = newFaces
   );
   result
)


compute#Cone#computedFacetsThroughRayData = method()
compute#Cone#computedFacetsThroughRayData Cone := C -> (
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


compute#Cone#raysThroughFacets = method()
compute#Cone#raysThroughFacets Cone := C -> (
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


compute#Cone#computedFVector = method()
compute#Cone#computedFVector Cone := C -> (
   reverse apply(dim C + 1, d -> #faces(dim C - d,C))
)


compute#Cone#computedDimension = method(TypicalValue => ZZ)
compute#Cone#computedDimension Cone := C -> (
   if hasProperties(C, {rays, computedLinealityBasis}) then (
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
   inputMatrix := (facets C) || (hyperplanes C) || ( - hyperplanes C);
   hb := transpose hilbertBasis(transpose inputMatrix, InputType=>"lattice");
   r := ring inputMatrix;
   result := apply(0..(numColumns hb - 1), i -> hb_i);
   eq := hyperplanes C;
   zero :=  transpose matrix {toList ((numRows eq):0)};
   result = apply(result, h -> (promote(matrix h, r)) // inputMatrix);
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
   if hasProperties(C, {rays, computedLinealityBasis}) then (
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
   if hasProperties(C, {rays, computedLinealityBasis}) then (
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


compute#Cone#rays = method()
compute#Cone#rays Cone := C -> (
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
   R := rays C;
   L := linealitySpace C;
   testmat := R | L;
   (numColumns testmat) == (rank testmat)
)


compute#Cone#ambientDimension = method()
compute#Cone#ambientDimension Cone := C -> (
   if hasProperty(C, inputRays) then numRows getProperty(C, inputRays)
   else if hasProperty(C, rays) then numRows rays C
   else if hasProperty(C, inputLinealityGenerators) then numRows getProperty(C, inputLinealityGenerators)
   else if hasProperty(C, computedLinealityBasis) then numRows linealitySpace C
   else if hasProperty(C, inequalities) then numColumns getProperty(C, inequalities)
   else if hasProperty(C, computedFacets) then numColumns facets C
   else if hasProperty(C, equations) then numColumns getProperty(C, equations)
   else if hasProperty(C, computedHyperplanes) then numColumns hyperplanes C
   else error("Is the cone fully defined? Cannot compute ambient dimension.")
)



