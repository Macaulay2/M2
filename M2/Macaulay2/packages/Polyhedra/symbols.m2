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
-- protect facets
-- protect computedHyperplanes
-- protect computedHilbertBasis
-- protect computedLinealityBasis
-- protect rays
--   "pointed",
--   "smooth",
--   "fullDimensional"

protect equations
protect inequalities
protect inputLinealityGenerators
protect inputRays
protect ambientDimension
protect computedDimension
protect computedFacesThroughRays -- Which rays span a face?
protect facetRayDataConverter
protect facetsThroughRayData -- Which rays span a facet?
protect computedFVector
protect raysThroughFacets -- Which facets intersect in a ray?
protect computedHyperplanes
protect computedHilbertBasis
protect computedLinealityBasis
protect pointed
protect smooth
protect fullDimensional
protect simplicial
protect nRays
protect nFacets
protect points
protect computedVertices
protect underlyingCone
protect lattice
protect empty
protect verticesThroughFacets
protect indexOfTrivialFacet
protect facetToFacetMap -- Which facets of underlying cone maps to facet of polyhedron.
protect computedNormalFan
protect computedLatticePoints
protect computedPolar
protect computedCompact -- Is the polyhedron compact?
protect computedNormal -- Is the polyhedron normal?
protect computedVeryAmple -- Is the polyhedron very ample?
protect computedEhrhart -- The Ehrhart polynomial

protect maximalCones   -- Maximal cones of a fan.
protect inputCones  -- Cones defining a fan. These cones are not necessarily maximal.
protect computedPolytope  -- Polytope whose normal fan is the given fan.
protect generatingObjects 
protect pure  -- is the fan of pure dimension?
protect honestMaxObjects  -- maximal cones as honest cones
protect computedComplete  -- is the fan complete?
protect smoothCones  -- smooth cones of fan as list of index lists
protect polytopal -- Is the fan the normal fan of a polytope?
protect underlyingFan -- The underlying fan of a polyhedral complex, i.e. intersecting at ht 1 gives complex
protect computedFacesThroughVertices
