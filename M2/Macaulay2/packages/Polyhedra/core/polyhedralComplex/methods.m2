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
--  OUTPUT : a List of Cones
maxPolyhedra = method(TypicalValue => List)
maxPolyhedra PolyhedralComplex := PC -> getProperty(PC, generatingObjects)


vertices PolyhedralComplex := PC -> getProperty(PC, computedVertices)


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
	L := select(getProperty(PC, honestMaxObjects), C -> dim C >= k);
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	unique flatten apply(L, C -> faces(dim(C)-k,C)))


isWellDefined PolyhedralComplex := PC -> getProperty(PC, isWellDefined)

fan (PolyhedralComplex) := PC -> getProperty(PC, underlyingFan)
