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
-- protect computedRays
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
   "computedFacetsThroughRays", -- Which rays span a facet?
   "computedFVector",
   "computedRaysThroughFacets", -- Which facets intersect in a ray?
   "computedHyperplanes",
   "computedHilbertBasis",
   "computedLinealityBasis",
   "computedRays",
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

   "maximalCones",   -- Maximal cones of a fan.
   "inputCones",  -- Cones defining a fan. These cones are not neccessarily maximal.
   "computedPolytope",  -- Polytope whose normal fan is the given fan.
   "generatingObjects", 
   "pure",  -- is the fan of pure dimension?
   "honestMaxObjects",  -- maximal cones as honest cones
   "computedComplete",  -- is the fan complete?
   "smoothCones"  -- smooth cones of fan as list of index lists
}
