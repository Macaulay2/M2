isPure PolyhedralObjectFamily := POF -> getProperty(POF, pure)

isComplete PolyhedralObjectFamily := POF -> getProperty(POF, computedComplete)


maxObjects PolyhedralObjectFamily := POF -> getProperty(POF, generatingObjects)

objectsOfDim(ZZ, PolyhedralObjectFamily) := (k,POF) -> (
	-- Checking for input errors
	if k < 0 or dim POF < k then error("k must be between 0 and the dimension of the polyhedral object family.");
	L := select(getProperty(POF, honestMaxObjects), C -> dim C >= k);
	-- Collecting the 'k'-dim faces of all generating cones of dimension greater than 'k'
	unique flatten apply(L, C -> faces(dim(C)-k,C)))
