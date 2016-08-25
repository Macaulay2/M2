-- --------------------------------------------------
-- -- Cone
-- -- Input properties
-- protect equations
-- protect inequalities
-- protect inputLinealityGenerators
-- protect inputRays
-- -- Computed properties
-- protect ambientDimension
-- protect computedDimension
-- protect computedFacets
-- protect computedHyperplanes
-- protect computedHilbertBasis
-- protect computedLinealityBasis
-- protect rays
--   "pointed",
--   "smooth",
--   "fullDimensional"

export {
   "equations",
   "inequalities",
   "inputLinealityGenerators",
   "inputRays",
   "ambientDimension",
   "computedDimension",
   "computedFacets",
   "computedFacesThroughRays", -- Which rays span a face?
   "computedFacetsThroughRayData", -- Which rays span a facet?
   "computedFVector",
   "raysThroughFacets", -- Which facets intersect in a ray?
   "computedHyperplanes",
   "computedHilbertBasis",
   "computedLinealityBasis",
   -- "rays",
   "pointed",
   "smooth",
   "fullDimensional",
   "simplicial",
   "nRays",
   "nFacets",

   "points",
   "computedVertices",
   "underlyingCone",
   "lattice",
   "empty",
   "verticesThroughFacets",
   "indexOfTrivialFacet",
   "facetToFacetMap", -- Which facets of underlying cone maps to facet of polyhedron.
   "computedNormalFan",
   "computedLatticePoints",
   "computedPolar",
   "computedCompact", -- Is the polyhedron compact?
   "computedNormal", -- Is the polyhedron normal?
   "computedVeryAmple", -- Is the polyhedron very ample?
   "computedEhrhart", -- The Ehrhart polynomial

   "maximalCones",   -- Maximal cones of a fan.
   "inputCones",  -- Cones defining a fan. These cones are not neccessarily maximal.
   "computedPolytope",  -- Polytope whose normal fan is the given fan.
   "generatingObjects", 
   "pure",  -- is the fan of pure dimension?
   "honestMaxObjects",  -- maximal cones as honest cones
   "computedComplete",  -- is the fan complete?
   "smoothCones",  -- smooth cones of fan as list of index lists
   "polytopal", -- Is the fan the normal fan of a polytope?

   "underlyingFan", -- The underlying fan of a polyhedral complex, i.e. intersecting at ht 1 gives complex
   "computedFacesThroughVertices"
}
