-- inputRays/inputLinealityGenerators (and, for Polyhedron, points) are pure
-- construction-time data: there is no compute# fallback for them, they are
-- either given when the object is built or not there at all. These getters
-- are the single place that looks them up (still via cacheValue/the shared
-- underlying cache key, so behavior is unchanged), in place of
-- getProperty(*, inputRays) / getProperty(*, inputLinealityGenerators).
-- Named getInputRays/getInputLinealityGenerators, not inputRays/
-- inputLinealityGenerators, since the latter are protected symbols used as
-- the property/cache keys.
getInputRays = method()
getInputRays Cone := (cacheValue symbol inputRays) (
   C -> error("No input rays set for this Cone.")
)

getInputLinealityGenerators = method()
getInputLinealityGenerators Cone := (cacheValue symbol inputLinealityGenerators) (
   C -> error("No input lineality generators set for this Cone.")
)

-- inequalities/equations are pure construction-time H-data, same story as
-- inputRays/inputLinealityGenerators above, just for the halfspace/hyperplane
-- side; also used by Polyhedron (see core/polyhedron/properties.m2).
getInequalities = method()
getInequalities Cone := (cacheValue symbol inequalities) (
   C -> error("No inequalities set for this Cone.")
)

getEquations = method()
getEquations Cone := (cacheValue symbol equations) (
   C -> error("No equations set for this Cone.")
)


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
      C2 := coneFromVData(getInputRays C, getInputLinealityGenerators C);
      if not testDim == dim C2 then return false;
      if not testAmbientDim == ambDim C2 then return false;
      if not (C2 == C) then return false
   );
   if hasProperties(C, {inequalities, equations}) then (
      hasEnoughProperties = true;
      C3 := coneFromHData(getInequalities C, getEquations C);
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
      return rank (getInputRays C | getInputLinealityGenerators C)
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


-- rays, facets, hyperplanes (computedHyperplanes) and linealitySpace
-- (computedLinealityBasis) mutually reference each other's computation (rays
-- needs facets-or-H-data, facets needs rays-or-H-data, hyperplanes/lineality
-- need either side), so they're migrated together here rather than one at a
-- time, the way underlyingCone was. rays/facets/hyperplanes/linealitySpace
-- are Macaulay2 core (or, for linealitySpace, Polyhedra-global) methods, so
-- these install directly under those names -- no renaming needed, unlike
-- inputRays/underlyingCone and friends, since none of these names are
-- protected symbols.
linealitySpace Cone := (cacheValue symbol computedLinealityBasis) (
   C -> (
      local containingSpace;
      if hasProperties(C, {facets, computedHyperplanes}) then (
         containingSpace = (facets C) || (hyperplanes C);
      ) else if hasProperties(C, {inequalities, equations}) then (
         containingSpace = getInequalities C || getEquations C;
      ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
         rays C;
         return linealitySpace C
      ) else (
         error "Lineality space not computable."
      );
      orthogonalComplement containingSpace
   )
)


hyperplanes Cone := (cacheValue symbol computedHyperplanes) (
   C -> (
      local containingSpace;
      if hasProperties(C, {rays, computedLinealityBasis}) then (
         containingSpace = rays C | linealitySpace C;
      ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
         containingSpace = getInputRays C | getInputLinealityGenerators C;
      ) else if hasProperties(C, {inequalities, equations}) then (
         facets C;
         return hyperplanes C
      ) else (
         error "Hyperplanes not computable"
      );
      result := orthogonalComplement transpose containingSpace;
      transpose result
   )
)


facets Cone := (cacheValue facets) (
   C -> (
      -- rays/computedLinealityBasis and inequalities/equations both end up
      -- using (rays C, linealitySpace C) here (the latter by forcing rays C
      -- to be computed from the inequalities/equations), so this is exactly
      -- getVRepresentation's tiering: prefer already-known rays, else raw
      -- input rays, else force the canonical computation.
      (rayData, linealityData) := getVRepresentation C;
      (facetData, hyperplaneData) := computeFacetsFromRayData(rayData, linealityData);
      if not hasProperty(C, computedHyperplanes) then setProperty(C, computedHyperplanes, hyperplaneData);
      facetData
   )
)


rays Cone := {} >> o -> (cacheValue rays) (
   C -> (
      local rayData;
      local linealityData;
      if hasProperties(C, {facets, computedHyperplanes}) then (
         (rayData, linealityData) = computeRaysFromFacetData(facets C, hyperplanes C);
      ) else if hasProperties(C, {inequalities, equations}) then (
         (rayData, linealityData) = computeRaysFromFacetData(getInequalities C, getEquations C);
      ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
         (rayData, linealityData) = computeRaysFromFacetData(facets C, hyperplanes C);
      ) else (
         error "Rays not computable."
      );
      if not hasProperty(C, computedLinealityBasis) then setProperty(C, computedLinealityBasis, linealityData);
      rayData
   )
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
   if hasProperty(C, inputRays) then numRows getInputRays C
   else if hasProperty(C, rays) then numRows rays C
   else if hasProperty(C, inputLinealityGenerators) then numRows getInputLinealityGenerators C
   else if hasProperty(C, computedLinealityBasis) then numRows linealitySpace C
   else if hasProperty(C, inequalities) then numColumns getInequalities C
   else if hasProperty(C, facets) then numColumns facets C
   else if hasProperty(C, equations) then numColumns getEquations C
   else if hasProperty(C, computedHyperplanes) then numColumns hyperplanes C
   else error("Is the cone fully defined? Cannot compute ambient dimension.")
)



