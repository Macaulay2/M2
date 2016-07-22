-- Defining the new type PolyhedralObjectFamily
PolyhedralObjectFamily = new Type of PolyhedraHash
globalAssignment PolyhedralObjectFamily

-- PURPOSE : Checks if the Fan or PolyhedralComplex is of pure dimension
--   INPUT : 'PC'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isPure = method(TypicalValue => Boolean)
isPure PolyhedralObjectFamily := POF -> getProperty(POF, pure)

-- PURPOSE : Tests if a Fan or PolyhedralComplex is complete
--   INPUT : 'POF'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isComplete = method(TypicalValue => Boolean)
isComplete PolyhedralObjectFamily := POF -> getProperty(POF, computedComplete)

--     if not POF.cache.?isComplete then (
--	  n := dim POF;
--	  POF.cache.isComplete = if n == ambDim POF then (
--	       symmDiff := (x,y) -> ((x,y) = (set x,set y); toList ((x-y)+(y-x)));
--	       Lfaces := {};
--	       scan(maxObjects POF, C -> if dim C == n then Lfaces = symmDiff(Lfaces,faces(1,C)));
--	       Lfaces == {})
--	  else false);
--     POF.cache.isComplete)

maxObjects = method(TypicalValue => List)
maxObjects PolyhedralObjectFamily := POF -> getProperty(POF, generatingObjects)

objectsOfDim = method(TypicalValue => List)
objectsOfDim(ZZ,PolyhedralObjectFamily) := (k,POF) -> (
	-- Checking for input errors
	if k < 0 or dim POF < k then error("k must be between 0 and the dimension of the polyhedral object family.");
	L := select(maxObjects POF, C -> dim C >= k);
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	unique flatten apply(L, C -> faces(dim(C)-k,C)))
