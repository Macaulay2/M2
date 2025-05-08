compute#Cone#isWellDefined = method()
compute#Cone#isWellDefined Cone := C -> (
   hasEnoughProperties := false;
   testDim := dim C;
   testAmbientDim := ambDim C;
   if hasProperties(C, {rays, computedLinealityBasis}) then (
      hasEnoughProperties = true;
      C1 := coneFromVData(rays C, linealitySpace C);
      if not testDim == dim C1 then return false;
      if not testAmbientDim == ambDim C1 then return false;
      if not C1 == C then return false
   );
   if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      hasEnoughProperties = true;
      C2 := coneFromVData(getProperty(C, inputRays), getProperty(C, inputLinealityGenerators));
      if not testDim == dim C2 then return false;
      if not testAmbientDim == ambDim C2 then return false;
      if not (C2 == C) then return false
   );
   if hasProperties(C, {inequalities, equations}) then (
      hasEnoughProperties = true;
      C3 := coneFromHData(getProperty(C, inequalities), getProperty(C, equations));
      if not testDim == dim C3 then return false;
      if not testAmbientDim == ambDim C3 then return false;
      if not (C3 == C) then return false
   );
   if hasProperties(C, {facets, computedHyperplanes}) then (
      hasEnoughProperties = true;
      C4 := coneFromHData(facets C, hyperplanes C);
      if not testDim == dim C4 then return false;
      if not testAmbientDim == ambDim C4 then return false;
      if not (C4 == C) then return false
   );
   return hasEnoughProperties
)


compute#Cone#pointed = method()
compute#Cone#pointed Cone := C -> (
   rank linealitySpace C == 0
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

getDualFaceIndices := method();
getDualFaceIndices(Matrix, Matrix, List) := (F, R, facetRep) -> (
   toList positions(0..(numColumns R - 1), 
      i -> (
         ray := R_{i};
         all(facetRep,
            j -> (
               facet := F^{j};
               eval := (facet * ray)_(0,0);
               eval == 0
            )
         )
      )
   )
)

compute#Cone#facetRayDataConverter = method()
compute#Cone#facetRayDataConverter Cone := C -> (
   fcr := new MutableHashTable;
   raysC := rays C;
   facetsC := facets C;
   facesC := faces C;
   for i in keys facesC do (
      L := facesC#i;
      for face in L do (
         fcr#face = getDualFaceIndices(transpose raysC, transpose facetsC, face);
      )
   );
   fcr
)

compute#Cone#computedFacesThroughRays = method()
compute#Cone#computedFacesThroughRays Cone := C -> (
   result := new MutableHashTable;
   d := dim C;
   raysC := rays C;
   facetsC := facets C;
   ldim := rank linealitySpace C;
   allFace := toList (0..(getProperty(C, nRays) - 1));
   result#0 = {allFace};
   ftrd := getProperty(C, facetsThroughRayData);
   if d>0 then (
      result#(1) = toList ftrd;
      for i from 0 to d-2-ldim do (
         oldFaces := result#(1+i);
         newFaces := unique flatten apply(oldFaces,
            face -> toList apply(result#(1),
               facet -> (
                  newFace := sort elements ((set face) * (set facet));
                  if (rank raysC_(toList newFace)) + ldim == d-2-i then (
                     newFace
                  )
               )
            )
         );
         newFaces = select(newFaces, nf -> nf =!= null);
         result#(2+i) = newFaces
      );
   );
   hashTable pairs result
)


compute#Cone#facetsThroughRayData = method()
compute#Cone#facetsThroughRayData Cone := C -> (
   raysC := rays C;
   facetsC := facets C;
   nFacetsC := getProperty(C, nFacets);
   nRaysC := getProperty(C, nRays);
   apply(0..(nFacetsC -1), 
      i -> getDualFaceIndices(facetsC, raysC, {i})
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
   toList apply(0..dim C, d -> #faces(dim C - d,C))
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


importFrom_Core { "raw", "rawHilbertBasis" } -- calls libnormaliz
compute#Cone#computedHilbertBasis = method()
compute#Cone#computedHilbertBasis Cone := C -> (
   hb := transpose map(ZZ, rawHilbertBasis raw transpose rays C);
   apply(numColumns hb, i -> hb_{i})
)


compute#Cone#computedLinealityBasis = method()
compute#Cone#computedLinealityBasis Cone := C -> (
   local containingSpace;
   if hasProperties(C, {facets, computedHyperplanes}) then (
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


compute#Cone#facets = method()
compute#Cone#facets Cone := C -> (
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
   if hasProperties(C, {facets, computedHyperplanes}) then (
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
   else if hasProperty(C, facets) then numColumns facets C
   else if hasProperty(C, equations) then numColumns getProperty(C, equations)
   else if hasProperty(C, computedHyperplanes) then numColumns hyperplanes C
   else error("Is the cone fully defined? Cannot compute ambient dimension.")
)



