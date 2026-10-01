-- PURPOSE : Giving the k dimensional Polyhedra of the Polyhedral Complex
--   INPUT : (k,PC)  where 'k' is a positive integer and PC is a PolyhedralComplex 
--  OUTPUT : a List of Polyhedra
polyhedra = method(TypicalValue => List)
polyhedra(ZZ,PolyhedralComplex) := (k,PC) -> (
   -- Checking for input errors
   if k < 0 or dim PC < k then error("k must be between 0 and the dimension of the fan.");
   L := getProperty(PC, computedFacesThroughRays);
   -- Collecting the 'k'-dim faces of all generating polyhedra of dimension greater than 'k'
   dk := dim(PC) - k;
   if L#?dk then return L#dk
   else return {}
)


-- PURPOSE : Giving the generating Polyhedra of the PolyhedralComplex
--   INPUT : 'PC'  a PolyhedralComplex
--  OUTPUT : a List of Polyhedra (or, via maxPolyhedra(PC, List), of their
--           (vertex,ray) index list pairs)
-- See maxCones in core/fan/methods.m2 for why Dispatch is {Thing, Type}.
maxPolyhedra = method(TypicalValue => List, Dispatch => {Thing, Type})
maxPolyhedra PolyhedralComplex := PC -> maxPolyhedra(PC, List)
maxPolyhedra(PolyhedralComplex, List) := (PC, List) -> getProperty(PC, generatingObjects)
-- Materialized Polyhedron objects, parallel-indexed to maxPolyhedra(PC, List).
-- See maxCones(Fan, Cone) in core/fan/methods.m2 for the same idea.
maxPolyhedra(PolyhedralComplex, Polyhedron) := (PC, Polyhedron) -> (
   accessor := (cacheValue (symbol maxPolyhedra => Polyhedron)) (
      PC -> (
         vertPC := vertices PC;
         raysPC := rays PC;
         linPC := linealitySpace PC;
         apply(maxPolyhedra PC, m -> convexHull(vertPC_(m#0), raysPC_(m#1), linPC))
      )
   );
   accessor PC
)


-- vertices PolyhedralComplex is installed in core/polyhedralComplex/properties.m2


skeleton(ZZ,PolyhedralComplex) := (n,PC) -> (
   -- Checking for input errors
   if n < 0 or dim PC < n then error("The integer must be between 0 and dim PC");
   GP := polyhedra(n,PC);
   vertPC := vertices PC;
   raysPC := rays PC;
   linPC := linealitySpace PC;
   polyhedralComplex(vertPC, raysPC, linPC, GP)
)


isPure PolyhedralComplex := PC -> getProperty(PC, pure)
isComplete PolyhedralComplex := PC -> getProperty(PC, computedComplete)
maxObjects PolyhedralComplex := PC -> getProperty(PC, generatingObjects)

objectsOfDim(ZZ, PolyhedralComplex) := (k,PC) -> (
	-- Checking for input errors
	if k < 0 or dim PC < k then error("k must be between 0 and the dimension of the polyhedral object family.");
	L := select(maxPolyhedra(PC, Polyhedron), C -> dim C >= k);
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	unique flatten apply(L, C -> faces(dim(C)-k,C)))


isWellDefined PolyhedralComplex := PC -> getProperty(PC, isWellDefined)

fan (PolyhedralComplex) := PC -> getUnderlyingFan PC
